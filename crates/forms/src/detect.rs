//! Prepare a form ▸ automatic field detection: find where a printed form expects answers —
//! underscore runs, rules (horizontal lines), empty boxes, rows of comb cells and small squares —
//! and name each new field from the label next to it (Acrobat's auto-naming).
//!
//! [`detect`] is a pure function over the page's words and drawn shapes; [`page_shapes`] reads
//! the shapes from a page's content, [`raster::shapes`] from a picture of the page (a scan).

use pdfcraft_content::{Matrix, Pieces};
use pdfcraft_cos::{Document, Object};

pub mod raster;

/// A piece of text on the page, in user space.
#[derive(Clone, Debug, PartialEq)]
pub struct Word {
    pub text: String,
    /// [x0, y0, x1, y1], y up.
    pub rect: [f64; 4],
}

/// Shapes drawn on the page, in user space: axis-aligned rectangle paths and horizontal segments.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Shapes {
    pub boxes: Vec<[f64; 4]>,
    pub rules: Vec<[f64; 4]>,
}

/// What detection proposes.
#[derive(Clone, Debug, PartialEq)]
pub enum Kind {
    Text,
    CheckBox,
    /// A row of touching cells, one character each: a text field with the comb flag and
    /// `cells` as its character limit.
    Comb {
        cells: usize,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Candidate {
    pub kind: Kind,
    pub rect: [f64; 4],
    /// From the nearby label, made a valid field name.
    pub name: String,
}

/// The most points a subpath may have and still be read as a rectangle: its four corners plus
/// points along its sides (#2942: a cell drawn with a midpoint on each vertical side).
const MAX_RECT_POINTS: usize = 16;

/// The rectangles and horizontal segments a page draws (stroked or filled), in user space.
#[derive(Default)]
struct DetectionPath {
    points: Vec<[f64; 2]>,
    rules: Vec<[f64; 4]>,
    complex: bool,
    closed: bool,
}

impl DetectionPath {
    fn finish(&mut self, paths: &mut Vec<Self>) {
        let path = std::mem::take(self);
        if !path.rules.is_empty() || (!path.complex && path.points.len() >= 4) {
            paths.push(path);
        }
    }

    fn rectangle(&self, implicitly_closed: bool) -> Option<[f64; 4]> {
        if self.complex {
            return None;
        }
        if self.points.iter().flatten().any(|v| !v.is_finite()) {
            return None;
        }
        let near = |a: [f64; 2], b: [f64; 2]| (a[0] - b[0]).abs() < 1e-6 && (a[1] - b[1]).abs() < 1e-6;
        let mut points = self.points.clone();
        let repeated_end = points.len() > 1 && near(points[0], *points.last()?);
        if repeated_end {
            points.pop();
        }
        if !(self.closed || implicitly_closed || repeated_end) {
            return None;
        }
        // A point on a straight side, between its neighbours, doesn't change the shape: drop it,
        // so a cell drawn with a midpoint on a side is the rectangle it looks like (#2942). A
        // point that turns back along the side is not between its neighbours and stays, so a
        // path that doubles back is still not a rectangle.
        let between = |a: f64, m: f64, b: f64| a.min(b) - 1e-6 <= m && m <= a.max(b) + 1e-6;
        let mut i = 0;
        while points.len() > 4 && i < points.len() {
            let n = points.len();
            let (Some(&a), Some(&m), Some(&b)) = (points.get((i + n - 1) % n), points.get(i), points.get((i + 1) % n)) else { break };
            let on_row = (a[1] - m[1]).abs() < 1e-6 && (m[1] - b[1]).abs() < 1e-6 && between(a[0], m[0], b[0]);
            let on_column = (a[0] - m[0]).abs() < 1e-6 && (m[0] - b[0]).abs() < 1e-6 && between(a[1], m[1], b[1]);
            if on_row || on_column {
                points.remove(i);
                i = 0;
            } else {
                i += 1;
            }
        }
        if points.len() != 4 {
            return None;
        }
        let mut horizontal = [false; 4];
        for i in 0..4 {
            let (a, b) = (points[i], points[(i + 1) % 4]);
            let h = (a[1] - b[1]).abs() < 1e-6;
            let v = (a[0] - b[0]).abs() < 1e-6;
            if h == v {
                return None; // Diagonal or zero-length edge.
            }
            horizontal[i] = h;
        }
        if (0..4).any(|i| horizontal[i] == horizontal[(i + 1) % 4]) {
            return None;
        }
        Some([
            points.iter().map(|p| p[0]).fold(f64::INFINITY, f64::min),
            points.iter().map(|p| p[1]).fold(f64::INFINITY, f64::min),
            points.iter().map(|p| p[0]).fold(f64::NEG_INFINITY, f64::max),
            points.iter().map(|p| p[1]).fold(f64::NEG_INFINITY, f64::max),
        ])
    }
}

pub fn page_shapes(doc: &Document, page: usize) -> Shapes {
    let mut out = Shapes::default();
    let Some(p) = pdfcraft_model::pages(doc).into_iter().nth(page) else { return out };
    let list: Vec<Object> = match p.dict.get(b"Contents").map(|c| doc.resolve(c)) {
        Some(c) => match &*c {
            Object::Array(a) => a.clone(),
            Object::Stream(_) => vec![p.dict.get(b"Contents").cloned().unwrap_or(Object::Null)],
            _ => Vec::new(),
        },
        None => Vec::new(),
    };
    let mut pieces = Vec::new();
    for o in list {
        if let Object::Stream(s) = &*doc.resolve(&o) {
            pieces.push(s.decoded().unwrap_or_default());
        }
    }
    let mut ctm = Matrix::IDENTITY;
    let mut stack = Vec::new();
    let mut path_boxes: Vec<[f64; 4]> = Vec::new();
    let mut subpaths: Vec<DetectionPath> = Vec::new();
    let mut path = DetectionPath::default();
    let mut cur: Option<[f64; 2]> = None;
    for op in Pieces::join(&pieces).parse().ops {
        match op.op.as_slice() {
            b"q" => stack.push(ctm),
            b"Q" => ctm = stack.pop().unwrap_or(ctm),
            b"cm" => {
                if let Some(m) = op.nums::<6>() {
                    ctm = Matrix(m).then(&ctm);
                }
            }
            b"re" => {
                path.finish(&mut subpaths);
                if let Some([x, y, w, h]) = op.nums::<4>() {
                    path_boxes.push(ctm.bbox([x.min(x + w), y.min(y + h), x.max(x + w), y.max(y + h)]));
                    let (x, y) = ctm.apply(x, y);
                    cur = Some([x, y]);
                    path.complex = true;
                    path.points.push([x, y]);
                }
            }
            b"m" => {
                path.finish(&mut subpaths);
                cur = op.nums::<2>().map(|[x, y]| {
                    let (a, b) = ctm.apply(x, y);
                    [a, b]
                });
                if let Some(p) = cur {
                    path.points.push(p);
                }
            }
            b"l" => {
                if let (Some(a), Some([x, y])) = (cur, op.nums::<2>()) {
                    // Continuing after closepath is not a single four-edge rectangle.
                    if path.closed {
                        path.complex = true;
                    }
                    let (bx, by) = ctm.apply(x, y);
                    let b = [bx, by];
                    if !path.complex {
                        if path.points.len() > MAX_RECT_POINTS {
                            path.complex = true;
                        } else {
                            if path.points.is_empty() {
                                path.points.push(a);
                            }
                            path.points.push(b);
                        }
                    }
                    if (a[1] - b[1]).abs() < 0.5 && (a[0] - b[0]).abs() > 1.0 {
                        path.rules.push([a[0].min(b[0]), a[1], a[0].max(b[0]), a[1]]);
                    }
                    cur = Some(b);
                }
            }
            b"c" | b"v" | b"y" => {
                path.complex = true;
                let end = if op.op.as_slice() == b"c" { op.nums::<6>().map(|v| [v[4], v[5]]) } else { op.nums::<4>().map(|v| [v[2], v[3]]) };
                if let Some([x, y]) = end {
                    let (x, y) = ctm.apply(x, y);
                    cur = Some([x, y]);
                }
            }
            b"h" => {
                path.closed = true;
                cur = path.points.first().copied().or(cur);
            }
            b"S" | b"s" | b"f" | b"F" | b"f*" | b"B" | b"B*" | b"b" | b"b*" => {
                if matches!(op.op.as_slice(), b"s" | b"b" | b"b*") {
                    path.closed = true;
                }
                path.finish(&mut subpaths);
                let filled = !matches!(op.op.as_slice(), b"S" | b"s");
                let mut path_rules = Vec::new();
                for subpath in subpaths.drain(..) {
                    if let Some(rect) = subpath.rectangle(filled) {
                        path_boxes.push(rect);
                    } else {
                        path_rules.extend(subpath.rules);
                    }
                }
                for r in path_boxes.drain(..) {
                    // A very flat filled rectangle is a rule.
                    if r[3] - r[1] < 1.5 && r[2] - r[0] > 1.0 {
                        out.rules.push([r[0], (r[1] + r[3]) / 2.0, r[2], (r[1] + r[3]) / 2.0]);
                    } else {
                        out.boxes.push(r);
                    }
                }
                out.rules.append(&mut path_rules);
                cur = None;
            }
            b"n" => {
                path_boxes.clear();
                subpaths.clear();
                path = DetectionPath::default();
                cur = None;
            }
            _ => {}
        }
    }
    out
}

fn mid(r: [f64; 4]) -> f64 {
    (r[1] + r[3]) / 2.0
}

/// A label's words made into a field name: letters, digits and spaces, no trailing colon.
fn clean(label: &str) -> String {
    let s: String = label.chars().map(|c| if c.is_alphanumeric() || c == ' ' || c == '-' || c == '/' { c } else { ' ' }).collect();
    let s = s.split_whitespace().collect::<Vec<_>>().join(" ");
    s.chars().take(40).collect::<String>().trim().to_string()
}

/// Whether `text` runs right to left (Hebrew, Arabic …), from its first letter; `None` without
/// letters (digits and punctuation take the direction of what is around them).
fn rtl(text: &str) -> Option<bool> {
    let c = text.chars().find(|c| c.is_alphabetic())?;
    Some(matches!(u32::from(c), 0x0590..=0x08FF | 0xFB1D..=0xFDFF | 0xFE70..=0xFEFF | 0x1_0800..=0x1_0FFF | 0x1_E800..=0x1_EFFF))
}

/// The label beside `rect` on one side (`right`) on the same line: the group of words nearest the
/// field, up to a gap wider than a few spaces, in the order they were read (the logical order, so
/// a Hebrew label's words come out as typed), and its distance from the field.
fn side_label(words: &[Word], rect: [f64; 4], right: bool) -> Option<(String, f64)> {
    let h = (rect[3] - rect[1]).max(8.0);
    let same_line = |w: &Word| (mid(w.rect) - mid(rect)).abs() < h.max(10.0) * 0.7;
    let gap = |w: &Word| if right { w.rect[0] - rect[2] } else { rect[0] - w.rect[2] };
    let mut side: Vec<(usize, &Word)> =
        words.iter().enumerate().filter(|(_, w)| same_line(w) && gap(w) >= -1.0 && gap(w) < 250.0 && !w.text.chars().all(|c| c == '_')).collect();
    let (_, nearest) = side.iter().min_by(|(_, a), (_, b)| gap(a).total_cmp(&gap(b)))?;
    let (line, first) = (mid(nearest.rect), gap(nearest));
    side.retain(|(_, w)| (mid(w.rect) - line).abs() < 3.0);
    // Outwards from the field: nearest first.
    side.sort_by(|(_, a), (_, b)| gap(a).total_cmp(&gap(b)));
    let mut picked: Vec<(usize, &str)> = Vec::new();
    let mut edge = if right { rect[2] } else { rect[0] };
    for (index, w) in side {
        let step = if right { w.rect[0] - edge } else { edge - w.rect[2] };
        if !picked.is_empty() && step > 24.0 {
            break;
        }
        picked.push((index, w.text.as_str()));
        edge = if right { w.rect[2] } else { w.rect[0] };
    }
    picked.sort_by_key(|(index, _)| *index);
    let label = clean(&picked.iter().map(|(_, t)| *t).collect::<Vec<_>>().join(" "));
    (!label.is_empty()).then_some((label, first))
}

/// A caption printed under a blank line ("Signature", "חתימה"): short, just below the line and
/// centred on it.
fn caption_below(words: &[Word], rect: [f64; 4]) -> Option<String> {
    let line = rect[1] - 1.0;
    let (x0, x1) = (rect[0], rect[2]);
    let under: Vec<(usize, &Word)> = words
        .iter()
        .enumerate()
        .filter(|(_, w)| w.rect[3] <= line + 1.0 && line - w.rect[3] < 14.0 && w.rect[0] >= x0 - 4.0 && w.rect[2] <= x1 + 4.0)
        .collect();
    let row = under.iter().map(|(_, w)| mid(w.rect)).max_by(f64::total_cmp)?;
    let mut on: Vec<(usize, &Word)> = under.into_iter().filter(|(_, w)| (mid(w.rect) - row).abs() < 3.0).collect();
    let (a, b) = on.iter().fold((f64::MAX, f64::MIN), |(a, b), (_, w)| (a.min(w.rect[0]), b.max(w.rect[2])));
    let centre = (a + b) / 2.0;
    if on.len() > 4 || (centre - (x0 + x1) / 2.0).abs() > (x1 - x0) * 0.3 {
        return None;
    }
    on.sort_by_key(|(index, _)| *index);
    let label = clean(&on.iter().map(|(_, w)| w.text.as_str()).collect::<Vec<_>>().join(" "));
    (!label.is_empty()).then_some(label)
}

/// The label for a text field at `rect`. On its line, a left-to-right label comes before the
/// field (to its left) and a right-to-left one before it from the other side (to its right), so
/// "שם: ____ טלפון: ____" names each blank from the Hebrew label on its right, not from the
/// next field's label on its left. The nearer of the two wins; a label on the wrong side only
/// when there is nothing else on the line. Then, for a blank line (`line`), a caption under it,
/// then the words just above.
fn label_for(words: &[Word], rect: [f64; 4], line: bool) -> String {
    let left = side_label(words, rect, false);
    let right = side_label(words, rect, true);
    let reads_in = |l: &Option<(String, f64)>, want_rtl: bool| l.clone().filter(|(t, _)| rtl(t).unwrap_or(false) == want_rtl);
    let found = match (reads_in(&left, false), reads_in(&right, true)) {
        (Some(l), Some(r)) => Some(if r.1 < l.1 { r } else { l }),
        (l, r) => l.or(r),
    };
    if let Some((label, _)) = found.or(left) {
        return label;
    }
    if line && let Some(caption) = caption_below(words, rect) {
        return caption;
    }
    // Above: the nearest line of words over the field's left part.
    let above: Vec<&Word> = words
        .iter()
        .filter(|w| w.rect[1] >= rect[3] - 1.0 && w.rect[1] - rect[3] < 24.0 && w.rect[0] < rect[2] && w.rect[2] > rect[0] - 4.0)
        .collect();
    let Some(row) = above.iter().map(|w| mid(w.rect)).min_by(f64::total_cmp) else { return String::new() };
    let mut on: Vec<&&Word> = above.iter().filter(|w| (mid(w.rect) - row).abs() < 3.0).collect();
    on.sort_by(|a, b| a.rect[0].total_cmp(&b.rect[0]));
    if on.iter().any(|w| rtl(&w.text) == Some(true)) {
        on.reverse();
    }
    clean(&on.iter().map(|w| w.text.as_str()).collect::<Vec<_>>().join(" "))
}

/// The label for a check box at `rect`: the words that follow it on its line, to its right in a
/// left-to-right form and to its left in a right-to-left one ("☐ מאשר" reads from the right), the
/// nearer of the two when both are there.
fn check_box_label(words: &[Word], rect: [f64; 4]) -> String {
    let right = side_label(words, rect, true);
    let left = side_label(words, rect, false);
    let reads_in = |l: &Option<(String, f64)>, want_rtl: bool| l.clone().filter(|(t, _)| rtl(t).unwrap_or(false) == want_rtl);
    let found = match (reads_in(&right, false), reads_in(&left, true)) {
        (Some(r), Some(l)) => Some(if l.1 < r.1 { l } else { r }),
        (r, l) => r.or(l),
    };
    found.or(right).map(|(label, _)| label).unwrap_or_default()
}

/// Rows of a table drawn with lines: two horizontal rules over the same span, 12–60 pt apart
/// with no other rule between them, as the row's rectangle.
fn rule_rows(rules: &[[f64; 4]]) -> Vec<[f64; 4]> {
    let span = |a: &[f64; 4], b: &[f64; 4]| {
        let (lo, hi) = (a[0].max(b[0]), a[2].min(b[2]));
        let shorter = (a[2] - a[0]).min(b[2] - b[0]);
        (shorter >= 40.0 && hi - lo >= shorter * 0.8).then_some((lo, hi))
    };
    let mut rows = Vec::new();
    for low in rules {
        // The nearest rule above `low` over the same span.
        let up = rules
            .iter()
            .filter(|up| up[1] - low[1] >= 12.0 && up[1] - low[1] <= 60.0 && span(low, up).is_some())
            .min_by(|a, b| a[1].total_cmp(&b[1]));
        let Some(up) = up else { continue };
        let between = rules.iter().any(|r| r[1] > low[1] + 1.0 && r[1] < up[1] - 1.0 && span(low, r).is_some());
        if let (Some((lo, hi)), false) = (span(low, up), between) {
            rows.push([lo, low[1], hi, up[1]]);
        }
    }
    rows
}

/// Where to write in a table row: the widest stretch of the row no words cover, if it is at
/// least 40 pt wide.
fn free_in_row(words: &[Word], row: [f64; 4]) -> Option<[f64; 4]> {
    let mut taken: Vec<(f64, f64)> = words
        .iter()
        .filter(|w| mid(w.rect) > row[1] && mid(w.rect) < row[3] && w.rect[2] > row[0] && w.rect[0] < row[2])
        .map(|w| (w.rect[0] - 4.0, w.rect[2] + 4.0))
        .collect();
    taken.sort_by(|a, b| a.0.total_cmp(&b.0));
    let (mut best, mut x) = (None::<(f64, f64)>, row[0]);
    for (a, b) in taken.into_iter().chain(std::iter::once((row[2], row[2]))) {
        if a - x >= 40.0 && best.is_none_or(|(p, q)| a - x > q - p) {
            best = Some((x, a));
        }
        x = x.max(b);
    }
    best.map(|(a, b)| [a + 2.0, row[1] + 1.0, b - 2.0, row[3] - 1.0])
}

/// Rows of two or more touching cells of one size, as (the row's rect, cell count). Two is
/// enough: dates split into day and month groups of two cells. Cells are square or taller
/// than wide; an empty table of wider cells is not a comb.
fn combs(boxes: &[[f64; 4]]) -> Vec<([f64; 4], usize)> {
    let cells: Vec<[f64; 4]> = boxes
        .iter()
        .copied()
        .filter(|r| {
            let (w, h) = (r[2] - r[0], r[3] - r[1]);
            (5.0..=30.0).contains(&w) && (8.0..=40.0).contains(&h) && w <= h * 1.2
        })
        .collect();
    // `b` comes right after `a` in a row: same size, its left edge on a's right edge.
    let follows = |a: &[f64; 4], b: &[f64; 4]| {
        (b[0] - a[2]).abs() <= 1.0 && (b[1] - a[1]).abs() <= 0.5 && (b[3] - a[3]).abs() <= 0.5 && ((b[2] - b[0]) - (a[2] - a[0])).abs() <= 0.5
    };
    let mut out = Vec::new();
    for first in &cells {
        if cells.iter().any(|a| follows(a, first)) {
            continue;
        }
        let (mut last, mut n) = (first, 1usize);
        // Each step moves at least 4 pt right, so this ends; the bound is for safety.
        while let Some(b) = cells.iter().find(|b| follows(last, b))
            && n < cells.len()
        {
            last = b;
            n += 1;
        }
        if n >= 2 {
            out.push(([first[0], first[1], last[2], first[3]], n));
        }
    }
    out
}

/// Propose fields for a page. `existing` are the page's widget rectangles (no candidate
/// overlaps them); names are unique against `taken`.
pub fn detect(words: &[Word], shapes: &Shapes, existing: &[[f64; 4]], taken: &[String]) -> Vec<Candidate> {
    let mut out: Vec<Candidate> = Vec::new();
    let overlaps = |a: [f64; 4], b: [f64; 4]| a[0] < b[2] - 1.0 && b[0] < a[2] - 1.0 && a[1] < b[3] - 1.0 && b[1] < a[3] - 1.0;
    let push = |kind: Kind, rect: [f64; 4], label: String, out: &mut Vec<Candidate>| {
        if existing.iter().chain(out.iter().map(|c| &c.rect)).any(|e| overlaps(*e, rect)) {
            return;
        }
        out.push(Candidate { kind, rect, name: label });
    };
    // Text that isn't underscores (labels), and underscore runs (blanks).
    let (blanks, labels): (Vec<&Word>, Vec<&Word>) = words.iter().partition(|w| w.text.len() >= 3 && w.text.chars().all(|c| c == '_'));
    let labels: Vec<Word> = labels.into_iter().cloned().collect();
    for b in blanks {
        let h = (b.rect[3] - b.rect[1]).max(10.0);
        let rect = [b.rect[0], b.rect[1], b.rect[2], b.rect[1] + (h * 1.4).clamp(12.0, 22.0)];
        let l = label_for(&labels, rect, true);
        push(Kind::Text, rect, l, &mut out);
    }
    // Comb boxes (postcodes, dates, IBANs): one text field per row of cells, found before
    // check boxes so the cells don't each become one.
    for (rect, cells) in combs(&shapes.boxes) {
        if labels.iter().any(|x| overlaps(x.rect, rect)) {
            continue;
        }
        let l = label_for(&labels, rect, false);
        push(Kind::Comb { cells }, rect, l, &mut out);
    }
    // Small squares: check boxes, labelled by the text to their right.
    for r in &shapes.boxes {
        let (w, h) = (r[2] - r[0], r[3] - r[1]);
        if (5.0..=20.0).contains(&w) && (5.0..=20.0).contains(&h) && (w / h - 1.0).abs() < 0.25 {
            let l = check_box_label(&labels, *r);
            push(Kind::CheckBox, *r, l, &mut out);
        }
    }
    // Empty boxes big enough to write in: text fields inside them.
    for r in &shapes.boxes {
        let (w, h) = (r[2] - r[0], r[3] - r[1]);
        let has_text = labels.iter().any(|x| overlaps(x.rect, *r));
        if w >= 40.0 && (12.0..=60.0).contains(&h) && !has_text {
            let inner = [r[0] + 1.0, r[1] + 1.0, r[2] - 1.0, r[3] - 1.0];
            let l = label_for(&labels, *r, false);
            push(Kind::Text, inner, l, &mut out);
        }
    }
    // Tables drawn with lines: a text field in each row's empty stretch, named from the row's
    // label. Their lines are rows' edges, not blanks to write on.
    let rows = rule_rows(&shapes.rules);
    for row in &rows {
        if let Some(rect) = free_in_row(&labels, *row) {
            let l = label_for(&labels, rect, false);
            push(Kind::Text, rect, l, &mut out);
        }
    }
    let edge = |r: &[f64; 4]| rows.iter().any(|row| (row[1] - r[1]).abs() < 0.5 || (row[3] - r[1]).abs() < 0.5) && r[2] - r[0] >= 40.0;
    // Rules: a text field sitting on the line.
    for r in shapes.rules.iter().filter(|r| !edge(r)) {
        let w = r[2] - r[0];
        if w < 40.0 {
            continue;
        }
        // A line that underlines text (or a table border with text on it) isn't a blank.
        let rect = [r[0], r[1] + 1.0, r[2], r[1] + 17.0];
        if labels.iter().any(|x| overlaps(x.rect, rect)) {
            continue;
        }
        let l = label_for(&labels, rect, true);
        push(Kind::Text, rect, l, &mut out);
    }
    // Reading order: top to bottom, and left to right, or right to left on a page whose labels
    // are mostly right to left.
    let (rtl_words, ltr_words) = labels.iter().fold((0usize, 0usize), |(r, l), w| match rtl(&w.text) {
        Some(true) => (r + 1, l),
        Some(false) => (r, l + 1),
        None => (r, l),
    });
    let rtl_page = rtl_words > ltr_words;
    // Rows: fields whose middles are within 8 pt of the row's first.
    out.sort_by(|a, b| mid(b.rect).total_cmp(&mid(a.rect)));
    let mut rows = Vec::with_capacity(out.len());
    let (mut row, mut start) = (0usize, f64::INFINITY);
    for c in &out {
        if start - mid(c.rect) > 8.0 {
            row += 1;
            start = mid(c.rect);
        }
        rows.push(row);
    }
    let mut keyed: Vec<(usize, Candidate)> = rows.into_iter().zip(out).collect();
    keyed.sort_by(|a, b| {
        let across = if rtl_page { b.1.rect[2].total_cmp(&a.1.rect[2]) } else { a.1.rect[0].total_cmp(&b.1.rect[0]) };
        a.0.cmp(&b.0).then(across)
    });
    let mut out: Vec<Candidate> = keyed.into_iter().map(|(_, c)| c).collect();
    // Unique names (Acrobat numbers repeats); unlabelled fields get Text1, Check Box1, …
    let mut used: Vec<String> = taken.to_vec();
    for c in &mut out {
        let base = if c.name.is_empty() {
            match c.kind {
                Kind::Text | Kind::Comb { .. } => "Text".to_string(),
                Kind::CheckBox => "Check Box".to_string(),
            }
        } else {
            c.name.clone()
        };
        let mut name = if c.name.is_empty() { format!("{base}1") } else { base.clone() };
        let mut n = 1;
        while used.contains(&name) {
            n += 1;
            name = format!("{base}{n}");
        }
        used.push(name.clone());
        c.name = name;
    }
    out
}
