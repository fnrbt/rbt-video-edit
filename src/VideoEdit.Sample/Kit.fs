module VideoEdit.Sample.Kit

open Renderer
open VideoEdit

// ════════════════════════════════════════════════════════════════════════
//  A drawing toolkit for the polished demos: font-parameterized text, scrims
//  for legibility over footage, glass panels, tags, dividers, elaborate cards.
//  Pieces return `Cmd list` (geometry) and are positioned/animated by the Scene
//  scenegraph; a few that animate internally take local time.
// ════════════════════════════════════════════════════════════════════════

let private px (v: float) : float<px> = v * 1.0<px>

// ── text ──
let measure (f: Text.FontData) (size: float) (s: string) = Text.measureWidth f size s

/// Baseline y that vertically centers a line of `size` about cy.
let midline (f: Text.FontData) (size: float) (cy: float) : float =
    cy + (Text.ascender f size + Text.descender f size) / 2.0

let textL (f: Text.FontData) (size: float) (col: Color) (x: float) (baseY: float) (s: string) : Cmd =
    DrawGlyphs(vec2 x baseY, Text.layout f size s, Solid col)

let textC (f: Text.FontData) (size: float) (col: Color) (cx: float) (baseY: float) (s: string) : Cmd =
    DrawGlyphs(vec2 (cx - measure f size s / 2.0) baseY, Text.layout f size s, Solid col)

let textR (f: Text.FontData) (size: float) (col: Color) (rightX: float) (baseY: float) (s: string) : Cmd =
    DrawGlyphs(vec2 (rightX - measure f size s) baseY, Text.layout f size s, Solid col)

/// Letter-spaced (tracked) text — draws each glyph with extra advance. Great for
/// spaced uppercase kickers/labels. Returns cmds + the total drawn width.
let trackedAt (f: Text.FontData) (size: float) (col: Color) (spacing: float) (x: float) (baseY: float) (s: string) : Cmd list * float =
    let mutable cx = x
    let cmds =
        [ for ch in s do
            let g = string ch
            yield DrawGlyphs(vec2 cx baseY, Text.layout f size g, Solid col)
            cx <- cx + measure f size g + spacing ]
    cmds, (cx - spacing - x)

let trackedWidth (f: Text.FontData) (size: float) (spacing: float) (s: string) : float =
    (s |> Seq.sumBy (fun ch -> measure f size (string ch))) + spacing * float (max 0 (s.Length - 1))

/// Tracked text centered about cx.
let trackedC (f: Text.FontData) (size: float) (col: Color) (spacing: float) (cx: float) (baseY: float) (s: string) : Cmd list =
    let w = trackedWidth f size spacing s
    fst (trackedAt f size col spacing (cx - w / 2.0) baseY s)

// ── strokes / shapes ──
let stroke (w: float) (col: Color) : StrokeStyle =
    { Width = px w; Cap = Butt; Join = Miter; Paint = Solid col; DashArray = []; DashOffset = px 0.0; MiterLimit = 4.0 }

let line (x0: float) (y0: float) (x1: float) (y1: float) (w: float) (col: Color) : Cmd =
    StrokePath([ MoveTo(vec2 x0 y0); LineTo(vec2 x1 y1) ], stroke w col)

// ── scrims (legibility gradients over footage) ──
/// Bottom-up dark gradient covering the lower `frac` of the frame.
let scrimBottom (frame: Frame) (frac: float) (strength: float) : Cmd =
    let y0 = frame.H * (1.0 - frac)
    let stops =
        [ { Offset = 0.0; StopColor = Color.rgba 0uy 0uy 0uy 0uy }
          { Offset = 1.0; StopColor = Color.rgba 0uy 0uy 0uy (byte (max 0.0 (min 1.0 strength) * 255.0)) } ]
    FillPath(rectPath (rect 0.0 y0 frame.W (frame.H - y0)), LinearGradient(vec2 0.0 y0, vec2 0.0 frame.H, stops, Pad), NonZero)

/// Top-down dark gradient covering the upper `frac`.
let scrimTop (frame: Frame) (frac: float) (strength: float) : Cmd =
    let h = frame.H * frac
    let stops =
        [ { Offset = 0.0; StopColor = Color.rgba 0uy 0uy 0uy (byte (max 0.0 (min 1.0 strength) * 255.0)) }
          { Offset = 1.0; StopColor = Color.rgba 0uy 0uy 0uy 0uy } ]
    FillPath(rectPath (rect 0.0 0.0 frame.W h), LinearGradient(vec2 0.0 0.0, vec2 0.0 h, stops, Pad), NonZero)

/// Flat full-frame wash (e.g. brand color at low alpha).
let wash (frame: Frame) (col: Color) (alpha: float) : Cmd =
    FillRect(rect 0.0 0.0 frame.W frame.H, Color.rgba col.R col.G col.B (byte (max 0.0 (min 1.0 alpha) * 255.0)))

/// Cinematic letterbox bars.
let letterbox (frame: Frame) (barH: float) : Cmd list =
    [ FillRect(rect 0.0 0.0 frame.W barH, Color.rgb 0uy 0uy 0uy)
      FillRect(rect 0.0 (frame.H - barH) frame.W barH, Color.rgb 0uy 0uy 0uy) ]

let vignette (frame: Frame) (strength: float) : Cmd =
    let cx, cy = frame.W / 2.0, frame.H / 2.0
    let r = sqrt (frame.W * frame.W + frame.H * frame.H) / 2.0
    let stops =
        [ { Offset = 0.0; StopColor = Color.rgba 0uy 0uy 0uy 0uy }
          { Offset = 0.6; StopColor = Color.rgba 0uy 0uy 0uy 0uy }
          { Offset = 1.0; StopColor = Color.rgba 0uy 0uy 0uy (byte (max 0.0 (min 1.0 strength) * 255.0)) } ]
    FillPath(rectPath (rect 0.0 0.0 frame.W frame.H), RadialGradient(vec2 cx cy, px r, stops, Pad), NonZero)

// ── panels / tags ──
let shadowRR (r: Rect) (radius: float) (blur: int) : Cmd =
    BoxShadow(r, 0.0, 14.0, blur, px 2.0, Color.rgba 0uy 0uy 0uy 170uy, false, Some (px radius))

/// Real frosted glass: blurs the footage BEHIND the panel (BackdropFilter), tints it, adds a
/// top sheen + hairline border. `blur`=0 falls back to a plain translucent panel (cheaper).
let glassPanel (x: float) (y: float) (w: float) (h: float) (radius: float) (blur: int) (pal: Theme.Palette) : Cmd list =
    let r = rect x y w h
    let rp = Raster.roundedRectPath r (px radius)
    let tint = Color.rgba pal.BgBot.R pal.BgBot.G pal.BgBot.B 130uy
    let sheen =
        [ { Offset = 0.0; StopColor = Color.rgba 255uy 255uy 255uy 30uy }
          { Offset = 0.5; StopColor = Color.rgba 255uy 255uy 255uy 6uy }
          { Offset = 1.0; StopColor = Color.rgba 255uy 255uy 255uy 0uy } ]
    let face =
        [ FillRoundedRect(r, px radius, tint)
          Cmd.Clip(rp, NonZero, [ FillPath(rectPath r, LinearGradient(vec2 x y, vec2 x (y + h), sheen, Pad), NonZero) ])
          StrokeRoundedRect(r, px radius, stroke 1.5 (Color.rgba 255uy 255uy 255uy 50uy)) ]
    [ shadowRR r radius 34 ]
    @ (if blur > 0 then [ Cmd.BackdropFilter(rp, NonZero, [ Blur blur ], face) ]
       else FillRoundedRect(r, px radius, pal.Panel) :: face)

/// Small solid pill tag with tracked uppercase label.
let tag (f: Text.FontData) (x: float) (y: float) (label: string) (fill: Color) (ink: Color) : Cmd list =
    let size = 26.0
    let sp = 3.0
    let w = trackedWidth f size sp label + 44.0
    let h = 46.0
    [ FillRoundedRect(rect x y w h, px (h / 2.0), fill) ]
    @ trackedC f size ink sp (x + w / 2.0) (midline f size (y + h / 2.0)) label

let tagWidth (f: Text.FontData) (label: string) = trackedWidth f 26.0 3.0 label + 44.0

let private clamp01 x = max 0.0 (min 1.0 x)

/// Word-wrapped paragraph. `topBaseY` is the baseline of the first line.
let paragraph (f: Text.FontData) (size: float) (col: Color) (x: float) (topBaseY: float) (maxW: float) (lineH: float) (text: string) : Cmd list =
    let lines = ResizeArray<string>()
    let mutable line = ""
    for wd in text.Split(' ') do
        let trial = if line = "" then wd else line + " " + wd
        if measure f size trial > maxW && line <> "" then lines.Add line; line <- wd
        else line <- trial
    if line <> "" then lines.Add line
    [ for i in 0 .. lines.Count - 1 -> textL f size col x (topBaseY + float i * lineH) lines.[i] ]

let private softShadow = [ DropShadow(0.0, 3.0, 12, Color.rgba 0uy 0uy 0uy 200uy) ]

/// Kicker: short accent rule + tracked uppercase label, left-aligned at (x, centred on cy).
let kicker (f: Text.FontData) (size: float) (col: Color) (accent: Color) (x: float) (cy: float) (s: string) : Cmd list =
    let ruleLen, gap = 46.0, 18.0
    [ line x cy (x + ruleLen) cy 3.0 accent ]
    @ fst (trackedAt f size col (size * 0.2) (x + ruleLen + gap) (midline f size cy) s)

/// Elegant lower-third: accent bar + name (display font) + tracked role, soft drop shadow.
let lowerThird (nameF: Text.FontData) (roleF: Text.FontData) (pal: Theme.Palette) (x: float) (y: float) (name: string) (role: string) : Cmd list =
    let nameSize, roleSize = 60.0, 24.0
    let nameBaseY = y + Text.ascender nameF nameSize
    let roleBaseY = nameBaseY + 16.0 + Text.ascender roleF roleSize
    let blockH = roleBaseY - y + 6.0
    [ FillRect(rect x y 6.0 blockH, pal.Accent)
      Filtered(softShadow, [ textL nameF nameSize pal.Ink (x + 26.0) nameBaseY name ]) ]
    @ fst (trackedAt roleF roleSize pal.Accent2 (roleSize * 0.18) (x + 28.0) roleBaseY role)

/// Glass stat card: frosted panel, accent stripe, tracked label, big value + unit, progress bar.
let statCard (labelF: Text.FontData) (valueF: Text.FontData) (pal: Theme.Palette)
             (x: float) (y: float) (w: float) (h: float) (blur: int)
             (label: string) (value: string) (unit: string) (frac: float) : Cmd list =
    let pad = 28.0
    let labelSize, valueSize, unitSize = 22.0, 76.0, 28.0
    let labelBaseY = y + pad + Text.ascender labelF labelSize
    let valueBaseY = y + h - 64.0
    let vw = measure valueF valueSize value
    let barY = y + h - 34.0
    glassPanel x y w h 18.0 blur pal
    @ [ FillRoundedRect(rect (x + pad) (y + pad - 4.0) 38.0 6.0, px 3.0, pal.Accent) ]
    @ fst (trackedAt labelF labelSize pal.InkDim 3.0 (x + pad + 52.0) labelBaseY label)
    @ [ Filtered(softShadow, [ textL valueF valueSize pal.Ink (x + pad) valueBaseY value ])
        textL labelF unitSize pal.Accent2 (x + pad + vw + 12.0) valueBaseY unit
        FillRoundedRect(rect (x + pad) barY (w - pad * 2.0) 6.0, px 3.0, Color.rgba 255uy 255uy 255uy 40uy)
        FillRoundedRect(rect (x + pad) barY ((w - pad * 2.0) * clamp01 frac) 6.0, px 3.0, pal.Accent) ]

/// HUD corner brackets, inset by `margin`, arms of length `len`.
let cornerBrackets (frame: Frame) (margin: float) (len: float) (wdt: float) (col: Color) : Cmd list =
    let l, r = margin, frame.W - margin
    let t, b = margin, frame.H - margin
    [ line l t (l + len) t wdt col; line l t l (t + len) wdt col
      line r t (r - len) t wdt col; line r t r (t + len) wdt col
      line l b (l + len) b wdt col; line l b l (b - len) wdt col
      line r b (r - len) b wdt col; line r b r (b - len) wdt col ]

/// Frame-counter timecode HH:MM:SS:FF from local time, top-right.
let timecode (f: Text.FontData) (col: Color) (frame: Frame) (fps: int) (t: float) : Cmd list =
    let total = int (t * float fps)
    let ff = total % fps
    let s = total / fps
    let str = sprintf "%02d:%02d:%02d:%02d" (s / 3600) (s / 60 % 60) (s % 60) ff
    [ FillCircle(vec2 (frame.W - 60.0) 64.0, px 7.0, Color.rgb 235uy 64uy 64uy) ]
    @ [ textR f 30.0 col (frame.W - 80.0) (midline f 30.0 64.0) str ]

/// Bottom scrolling ticker with an accent label chip. Time-driven.
let ticker (frame: Frame) (labelF: Text.FontData) (textF: Text.FontData) (pal: Theme.Palette)
           (speed: float) (label: string) (msg: string) (t: float) : Cmd list =
    let barH = 56.0
    let y = frame.H - barH
    let chipW = trackedWidth labelF 24.0 3.0 label + 44.0
    let size = 28.0
    let textBaseY = midline textF size (y + barH / 2.0)
    let one = measure textF size msg
    let gap = 120.0
    let period = one + gap
    let start = frame.W - ((t * speed) % period)
    let scroll =
        [ for k in 0 .. 2 -> textL textF size pal.Ink (start + float k * period) textBaseY msg ]
    [ FillRect(rect 0.0 y frame.W barH, pal.Panel)
      FillRect(rect 0.0 y frame.W 2.0, pal.Accent)
      FillRect(rect 0.0 y chipW barH, pal.Accent) ]
    @ trackedC labelF 24.0 pal.BgBot 3.0 (chipW / 2.0) (midline labelF 24.0 (y + barH / 2.0)) label
    @ [ Cmd.Clip(rectPath (rect chipW y (frame.W - chipW) barH), NonZero, scroll) ]

// ════════════════════════ streaming / OBS-style ════════════════════════

/// Circular "facecam": a second video source cover-cropped into a disc, with a soft drop
/// shadow, an accent ring, and a thin outer ring. Decode the cam source at a SQUARE size so
/// the cover-crop centres the subject. Time-driven (samples the cam at local time t).
let facecam (src: VideoSource) (cx: float) (cy: float) (r: float) (ring: Color) (ring2: Color) (t: float<s>) : Cmd list =
    let fb = Video.frameAt src t
    let dst = rect (cx - r) (cy - r) (r * 2.0) (r * 2.0)
    let srcR = rect 0.0 0.0 (float src.W) (float src.H)
    let mask = Raster.ellipsePath cx cy r r
    [ Filtered([ DropShadow(0.0, 12.0, 30, Color.rgba 0uy 0uy 0uy 205uy) ],
        [ Cmd.Clip(mask, NonZero, [ DrawImage(fb, srcR, dst, true) ])
          StrokePath(mask, stroke 8.0 ring)
          StrokePath(Raster.ellipsePath cx cy (r + 9.0) (r + 9.0), stroke 2.0 ring2) ]) ]

/// Rounded-rect facecam (16:9-ish), same treatment as the circular one.
let facecamRect (src: VideoSource) (x: float) (y: float) (w: float) (h: float) (radius: float) (ring: Color) (t: float<s>) : Cmd list =
    let fb = Video.frameAt src t
    let mask = Raster.roundedRectPath (rect x y w h) (px radius)
    [ Filtered([ DropShadow(0.0, 12.0, 30, Color.rgba 0uy 0uy 0uy 205uy) ],
        [ Cmd.Clip(mask, NonZero, [ DrawImage(fb, rect 0.0 0.0 (float src.W) (float src.H), rect x y w h, true) ])
          StrokeRoundedRect(rect x y w h, px radius, stroke 6.0 ring) ]) ]

/// Pulsing "● LIVE" badge.
let liveBadge (f: Text.FontData) (x: float) (y: float) (t: float) : Cmd list =
    let h = 46.0
    let pulse = 0.5 + 0.5 * sin (t * 4.5)
    let w = trackedWidth f 26.0 3.0 "LIVE" + 72.0
    [ FillRoundedRect(rect x y w h, px (h / 2.0), Color.rgb 230uy 46uy 46uy)
      FillCircle(vec2 (x + 28.0) (y + h / 2.0), px 7.0, Color.rgba 255uy 255uy 255uy (byte (140.0 + 115.0 * pulse))) ]
    @ fst (trackedAt f 26.0 Color.white 3.0 (x + 46.0) (midline f 26.0 (y + h / 2.0)) "LIVE")

/// Nameplate pill: bold name + dim handle, centered under a facecam.
let nameplate (nameF: Text.FontData) (handleF: Text.FontData) (cx: float) (y: float) (name: string) (handle: string) (pal: Theme.Palette) : Cmd list =
    let nameSize, hSize = 30.0, 22.0
    let w = (max (measure nameF nameSize name) (trackedWidth handleF hSize 2.0 handle)) + 64.0
    let h = 84.0
    let x = cx - w / 2.0
    [ shadowRR (rect x y w h) 14.0 24
      FillRoundedRect(rect x y w h, px 14.0, pal.Panel)
      StrokeRoundedRect(rect x y w h, px 14.0, stroke 1.5 (Color.rgba 255uy 255uy 255uy 40uy))
      textC nameF nameSize pal.Ink cx (y + 20.0 + Text.ascender nameF nameSize) name ]
    @ trackedC handleF hSize pal.Accent 2.0 cx (y + 20.0 + nameSize + 8.0 + Text.ascender handleF hSize) handle

// ── font specimen (verification + look selection) ──
let private specimenFonts () =
    [ "Anton", Theme.anton ()
      "Archivo Black", Theme.archivo ()
      "Bebas Neue", Theme.bebas ()
      "Poppins Bold", Theme.poppinsB ()
      "Poppins SemiBold", Theme.poppinsSB ()
      "Poppins Medium", Theme.poppinsM ()
      "Crimson Text Bold", Theme.crimsonB ()
      "Playfair Display", Theme.playfair () ]

let specimen (frame: Frame) : Cmd list =
    let label = Theme.poppinsM ()
    let rows = specimenFonts ()
    let n = List.length rows
    let top = 70.0
    let rowH = (frame.H - top - 40.0) / float n
    let cmds =
        [ for i, (name, f) in List.indexed rows do
            let y = top + float i * rowH
            let baseY = midline f 58.0 (y + rowH / 2.0)
            yield textL label 22.0 (Color.rgb 150uy 160uy 185uy) 60.0 (y + 26.0) name
            yield textL f 58.0 (Color.rgb 245uy 246uy 250uy) 60.0 baseY "Aesthetic Vol. 24 — 1234567890"
            yield line 60.0 (y + rowH - 2.0) (frame.W - 60.0) (y + rowH - 2.0) 1.0 (Color.rgba 255uy 255uy 255uy 26uy) ]
    [ AntiAlias(4, cmds) ]
