namespace Rbt.VideoEdit

open Rbt.Renderer

/// Game-show graphical elements. Each is `cfg -> (Frame -> localTime -> Cmd list)` so it
/// plugs straight into a `graphic` clip. titleCard/infoBox/countdownNumber/bulletList animate
/// internally; the rest are static and rely on clip-level effects (fadeIn, slideIn, …) for motion.
module Components =

    let private strokeStyle (width: float) (color: Color) : StrokeStyle =
        { Width = width * 1.0<px>; Cap = Butt; Join = Miter; Paint = Solid color
          DashArray = []; DashOffset = 0.0<px>; MiterLimit = 4.0 }

    let private centeredBaseline (font: Text.FontData) (size: float) (cy: float) : float =
        cy + (Text.ascender font size + Text.descender font size) / 2.0

    let private centeredText (font: Text.FontData) (size: float) (str: string) (cx: float) (baseY: float) (paint: Paint) : Cmd =
        let w = Text.measureWidth font size str
        DrawGlyphs(vec2 (cx - w / 2.0) baseY, Text.layout font size str, paint)

    let private leftText (font: Text.FontData) (size: float) (str: string) (x: float) (baseY: float) (paint: Paint) : Cmd =
        DrawGlyphs(vec2 x baseY, Text.layout font size str, paint)

    // ── Title card: big bold centered headline (+ optional subtitle), slam-in + fade ──
    type TitleCfg =
        { Headline: string; Subtitle: string option; Size: float; Fill: Color; Shadow: Color }

    let titleCard (cfg: TitleCfg) : Frame -> float<s> -> Cmd list =
        fun frame t ->
            let tt = float t
            let font = Theme.font ()
            let opacity = A.tween 0.0 1.0 0.30 Ease.easeOutCubic tt
            let scale = A.tween 1.6 1.0 0.45 (Ease.easeOutBack 2.2) tt
            let cx, cy = frame.W / 2.0, frame.H / 2.0
            let headBase = centeredBaseline font cfg.Size cy
            let headline = centeredText font cfg.Size cfg.Headline cx headBase (Solid cfg.Fill)
            let subCmds =
                match cfg.Subtitle with
                | Some sub ->
                    let ss = cfg.Size * 0.38
                    [ centeredText font ss sub cx (headBase + cfg.Size * 0.72) (Solid Theme.white) ]
                | None -> []
            aa [ Group(aboutPivot cx cy (Matrix3x2.scale scale scale), opacity,
                       [ Filtered([ DropShadow(0.0, 6.0, 12, cfg.Shadow) ], headline :: subCmds) ]) ]

    // ── Info / score panel: rounded box, bold label + value, shadow, accent border (pop-in) ──
    type InfoBoxCfg =
        { Label: string; Value: string; Fill: Color; Accent: Color; Radius: float; BorderWidth: float }

    let infoBox (cfg: InfoBoxCfg) : Frame -> float<s> -> Cmd list =
        fun frame t ->
            let tt = float t
            let font = Theme.font ()
            let opacity = A.tween 0.0 1.0 0.25 Ease.easeOutCubic tt
            let scale = A.tween 0.82 1.0 0.42 (Ease.easeOutBack 2.0) tt
            let labelSize, valueSize, padX, padY, gap = 36.0, 110.0, 56.0, 36.0, 8.0
            let boxW = (max (Text.measureWidth font labelSize cfg.Label) (Text.measureWidth font valueSize cfg.Value)) + padX * 2.0
            let boxH = padY * 2.0 + labelSize + gap + valueSize
            let cx, boxCy = frame.W / 2.0, frame.H * 0.70
            let bx, by = cx - boxW / 2.0, boxCy - boxH / 2.0
            let r = rect bx by boxW boxH
            let radius = cfg.Radius * 1.0<px>
            aa [ Group(aboutPivot cx boxCy (Matrix3x2.scale scale scale), opacity,
                   [ BoxShadow(r, 0.0, 12.0, 30, 2.0<px>, Theme.shadowStrong, false, Some radius)
                     FillRoundedRect(r, radius, cfg.Fill)
                     StrokeRoundedRect(r, radius, strokeStyle cfg.BorderWidth cfg.Accent)
                     centeredText font labelSize cfg.Label cx (by + padY + Text.ascender font labelSize) (Solid Theme.white)
                     centeredText font valueSize cfg.Value cx (by + padY + labelSize + gap + Text.ascender font valueSize) (Solid Theme.white) ]) ]

    // ── Lower third: name + title bar, bottom-left, accent stripe (static) ──
    type LowerThirdCfg = { Name: string; Title: string; Accent: Color }

    let lowerThird (cfg: LowerThirdCfg) : Frame -> float<s> -> Cmd list =
        fun frame _t ->
            let font = Theme.font ()
            let nameSize, titleSize = 52.0, 30.0
            let x, barH = 90.0, 120.0
            let y = frame.H - 210.0
            let w = (max (Text.measureWidth font nameSize cfg.Name) (Text.measureWidth font titleSize cfg.Title)) + 80.0
            let r = rect x y w barH
            aa [ BoxShadow(r, 0.0, 8.0, 22, 1.0<px>, Theme.shadowStrong, false, Some 10.0<px>)
                 FillRoundedRect(r, 10.0<px>, Color.rgba 12uy 14uy 30uy 235uy)
                 FillRect(rect x y 12.0 barH, cfg.Accent)
                 leftText font nameSize cfg.Name (x + 36.0) (y + 26.0 + Text.ascender font nameSize) (Solid Theme.white)
                 leftText font titleSize cfg.Title (x + 36.0) (y + 26.0 + nameSize + 8.0 + Text.ascender font titleSize) (Solid cfg.Accent) ]

    // ── Full-width top banner ──
    type BannerCfg = { Text: string; Fill: Color; Accent: Color }

    let banner (cfg: BannerCfg) : Frame -> float<s> -> Cmd list =
        fun frame _t ->
            let font = Theme.font ()
            let h, size = 110.0, 54.0
            aa [ FillRect(rect 0.0 0.0 frame.W h, cfg.Fill)
                 FillRect(rect 0.0 (h - 8.0) frame.W 8.0, cfg.Accent)
                 centeredText font size cfg.Text (frame.W / 2.0) (centeredBaseline font size (h / 2.0)) (Solid Theme.white) ]

    // ── Bottom-center caption pill ──
    let caption (text: string) : Frame -> float<s> -> Cmd list =
        fun frame _t ->
            let font = Theme.font ()
            let size = 40.0
            let padX = 40.0
            let w = Text.measureWidth font size text + padX * 2.0
            let h = 72.0
            let x, y = (frame.W - w) / 2.0, frame.H - 150.0
            aa [ FillRoundedRect(rect x y w h, 14.0<px>, Color.rgba 0uy 0uy 0uy 175uy)
                 leftText font size text (x + padX) (centeredBaseline font size (y + h / 2.0)) (Solid Theme.white) ]

    // ── Centered big number (static) ──
    let bigNumber (value: string) (fill: Color) (size: float) : Frame -> float<s> -> Cmd list =
        fun frame _t ->
            let font = Theme.font ()
            let cx, cy = frame.W / 2.0, frame.H / 2.0
            aa [ Filtered([ DropShadow(0.0, 8.0, 16, Theme.shadowStrong) ],
                          [ centeredText font size value cx (centeredBaseline font size cy) (Solid fill) ]) ]

    // ── Centered text label (static) ──
    let centeredLabel (text: string) (size: float) (color: Color) : Frame -> float<s> -> Cmd list =
        fun frame _t ->
            let font = Theme.font ()
            aa [ Filtered([ DropShadow(0.0, 5.0, 10, Theme.shadowStrong) ],
                          [ centeredText font size text (frame.W / 2.0) (centeredBaseline font size (frame.H / 2.0)) (Solid color) ]) ]

    // ── Top-right corner badge/pill ──
    let cornerBadge (text: string) (fill: Color) : Frame -> float<s> -> Cmd list =
        fun frame _t ->
            let font = Theme.font ()
            let size = 32.0
            let padX = 28.0
            let w = Text.measureWidth font size text + padX * 2.0
            let h = 56.0
            let x, y = frame.W - w - 50.0, 50.0
            aa [ FillRoundedRect(rect x y w h, (h / 2.0) * 1.0<px>, fill)
                 leftText font size text (x + padX) (centeredBaseline font size (y + h / 2.0)) (Solid Theme.white) ]

    // ── Bottom progress bar (frac 0..1) ──
    let progressBar (frac: float) (fill: Color) : Frame -> float<s> -> Cmd list =
        fun frame _t ->
            let m, h = 80.0, 18.0
            let y = frame.H - 64.0
            let w = frame.W - m * 2.0
            let f = max 0.0 (min 1.0 frac)
            aa [ FillRoundedRect(rect m y w h, (h / 2.0) * 1.0<px>, Color.rgba 255uy 255uy 255uy 60uy)
                 FillRoundedRect(rect m y (w * f) h, (h / 2.0) * 1.0<px>, fill) ]

    // ── Staggered bullet list (reveals one item at a time) ──
    let bulletList (items: string list) (accent: Color) : Frame -> float<s> -> Cmd list =
        fun frame t ->
            let font = Theme.font ()
            let size, x = 46.0, 150.0
            let startY, lineH, gap = frame.H * 0.30, 92.0, 0.25
            items
            |> List.mapi (fun i item ->
                let appear = float t - float i * gap
                let u = if appear <= 0.0 then 0.0 elif appear >= 0.4 then 1.0 else appear / 0.4
                if u <= 0.0 then []
                else
                    let e = let p = 1.0 - u in 1.0 - p * p * p
                    let yy = startY + float i * lineH
                    [ Group(Matrix3x2.translate ((1.0 - e) * -40.0) 0.0, e,
                            [ FillCircle(vec2 x (yy + 16.0), 12.0<px>, accent)
                              leftText font size item (x + 40.0) (yy + Text.ascender font size) (Solid Theme.white) ]) ])
            |> List.concat
            |> aa

    // ── Countdown number: fromN..1, one per second, pops on each change ──
    let countdownNumber (fromN: int) (fill: Color) : Frame -> float<s> -> Cmd list =
        fun frame t ->
            let tt = float t
            let n = fromN - int (floor tt)
            if n < 1 then []
            else
                let font = Theme.font ()
                let frac = tt - floor tt
                let size = 280.0
                let pop = if frac < 0.25 then 1.0 + 0.3 * (1.0 - frac / 0.25) else 1.0
                let alpha = if frac > 0.85 then 1.0 - (frac - 0.85) / 0.15 else 1.0
                let cx, cy = frame.W / 2.0, frame.H / 2.0
                aa [ Group(aboutPivot cx cy (Matrix3x2.scale pop pop), max 0.0 alpha,
                       [ Filtered([ DropShadow(0.0, 10.0, 20, Theme.shadowStrong) ],
                                  [ centeredText font size (string n) cx (centeredBaseline font size cy) (Solid fill) ]) ]) ]
