namespace Rbt.VideoEdit

open Rbt.Renderer

/// Drawing commands with plain-float parameters. Rbt.Renderer's `Cmd` mixes
/// `float` and `float<px>` (a circle's radius, a stroke's width, a shadow's
/// spread and corner radius are `float<px>`; a shadow's offsets are `float`),
/// which turns hand-written graphics into unit errors one argument at a time.
/// These take `float` everywhere and convert.
[<AutoOpen>]
module Draw =

    /// A plain float as renderer pixels.
    let px (v: float) : float<px> = LanguagePrimitives.FloatWithMeasure v

    /// A stroke of `width` px in one colour, round caps and joins.
    let stroke (width: float) (color: Color) : StrokeStyle =
        { Width = px width; Cap = LineCap.Round; Join = LineJoin.RoundJoin; Paint = Solid color
          DashArray = []; DashOffset = px 0.0; MiterLimit = 4.0 }

    let fillCircle (cx: float) (cy: float) (r: float) (color: Color) = FillCircle(vec2 cx cy, px r, color)

    /// A circle's outline, as a polyline of `segments` (64 is smooth at video sizes).
    let ring (cx: float) (cy: float) (r: float) (width: float) (color: Color) =
        let n = 64
        let pts = [ for i in 0 .. n -> let a = 2.0 * System.Math.PI * float i / float n in vec2 (cx + r * cos a) (cy + r * sin a) ]
        StrokePath(MoveTo pts.Head :: (pts.Tail |> List.map LineTo), stroke width color)

    let strokePath (path: PathSeg list) (width: float) (color: Color) = StrokePath(path, stroke width color)
    let fillPath (path: PathSeg list) (paint: Paint) = FillPath(path, paint, NonZero)

    let fillRoundedRect (r: Rect) (radius: float) (color: Color) = FillRoundedRect(r, px radius, color)

    /// A rounded rectangle as a path (for clipping and stroking).
    let roundedRectPath (r: Rect) (radius: float) : PathSeg list =
        let x, y, w, h = float r.Pos.X, float r.Pos.Y, float r.Size.X, float r.Size.Y
        let k = min radius (min w h / 2.0)
        [ MoveTo(vec2 (x + k) y); LineTo(vec2 (x + w - k) y); QuadTo(vec2 (x + w) y, vec2 (x + w) (y + k))
          LineTo(vec2 (x + w) (y + h - k)); QuadTo(vec2 (x + w) (y + h), vec2 (x + w - k) (y + h))
          LineTo(vec2 (x + k) (y + h)); QuadTo(vec2 x (y + h), vec2 x (y + h - k))
          LineTo(vec2 x (y + k)); QuadTo(vec2 x y, vec2 (x + k) y); Close ]

    /// An outer drop shadow of a (rounded) rectangle.
    let boxShadow (r: Rect) (dx: float) (dy: float) (blur: float) (spread: float) (color: Color) (radius: float) =
        BoxShadow(r, dx, dy, int (round blur), px spread, color, false, (if radius > 0.0 then Some(px radius) else None))

    let private stops (xs: (float * Color) list) = xs |> List.map (fun (o, c) -> { Offset = o; StopColor = c })
    let linearGradient (x0: float) (y0: float) (x1: float) (y1: float) (xs: (float * Color) list) =
        LinearGradient(vec2 x0 y0, vec2 x1 y1, stops xs, SpreadMode.Pad)
    let radialGradient (cx: float) (cy: float) (r: float) (xs: (float * Color) list) =
        RadialGradient(vec2 cx cy, px r, stops xs, SpreadMode.Pad)

    /// Text: its advance width as a plain float, and a run drawn from (x, baseline).
    let textWidth (font: Text.FontData) (size: float) (text: string) : float = float (Text.measureWidth font size text)
    let text (font: Text.FontData) (size: float) (s: string) (x: float) (baseline: float) (paint: Paint) =
        DrawGlyphs(vec2 x baseline, Text.layout font size s, paint)
