module VideoEdit.Preview.Program

open System
open System.Diagnostics
open System.Runtime.InteropServices
open Renderer
open Platform
open Platform.X11Window
open Platform.WaylandWindow
open VideoEdit
open VideoEdit.TimelineView

// ── default clip (when no .sexp doc given) ──
let private defaultClip () : RenderConfig * Clip =
    Fx.install (); Templates.install ()
    let jelly = Video.loadCached "assets/jellyfish.mp4" 0.0 4.0 1280 720 30
    let city = Video.loadCached "assets/city.mp4" 0.0 6.0 1280 720 30
    let intro = layer (videoFor 3.6<s> jelly |> kenBurns |> tint Color.black 0.42) [ graphic 3.6<s> (Components.titleCard { Headline = "LIVE PREVIEW"; Subtitle = Some "GLES"; Size = 96.0; Fill = Theme.gold; Shadow = Theme.shadowStrong }) ]
    let body = layer (video city |> kenBurns) [ graphic (Video.duration city) (Components.lowerThird { Name = "VideoEdit"; Title = "preview"; Accent = Theme.gold }) ]
    { W = 1280; H = 720; Fps = 30; Background = Theme.bg; Output = ""; Crf = 19; Preset = "medium" }, chain (crossfade 0.6<s>) [ intro; body ]

// timeline geometry constants
let private barH = 50.0      // transport bar height
let private tlH = 360.0      // timeline panel height
let private gutter = 244.0   // left label gutter in the timeline
let private rulerH = 26.0
let private rowH = 24.0

let private kindColor (k: TKind) : Color =
    match k with
    | KFootage -> Color.rgb 38uy 96uy 176uy
    | KGraphic -> Color.rgb 196uy 156uy 42uy
    | KAudio -> Color.rgb 40uy 156uy 96uy
    | KBg -> Color.rgb 78uy 84uy 96uy
    | KSection -> Color.rgb 120uy 86uy 196uy
    | KSeq | KLayer -> Color.rgba 90uy 100uy 128uy 90uy
    | _ -> Color.rgb 70uy 74uy 86uy

[<EntryPoint>]
let main argv =
    Fx.install (); Templates.install (); Theme.useBoldFont Theme.defaultBoldFontPath
    let docPath = argv |> Array.tryFind (fun (a: string) -> a.EndsWith ".sexp")
    let cfg, clip = match docPath with Some p -> Sexp.loadDoc (IO.File.ReadAllText p) | None -> defaultClip ()
    let pw, ph = cfg.W, cfg.H
    let frameGeom = { W = float pw; H = float ph }
    let bg = cfg.Background
    let total = duration clip
    let clampT (t: float<s>) = max 0.0<s> (min (total - 0.0005<s>) t)
    let tlRoot = TimelineView.build clip
    let mutable collapsed = TimelineView.initialCollapsed 1 tlRoot

    // frame source: GLES backend → readback; software fallback
    let renderFrame, backendName =
        let tryGl () =
            let pb = GlBackend.createPersistent pw ph
            let r (t: float<s>) = pb.Backend.Init pw ph; pb.Backend.Submit(Clear bg :: eval frameGeom clip t); pb.Backend.Finish() |> ignore; pb.Readback()
            r 0.0<s> |> ignore; r
        try tryGl (), "GLES"
        with ex -> eprintfn "[preview] GL unavailable (%s) → software" ex.Message
                   let bk = Raster.create () in (fun (t: float<s>) -> Renderer.Scene.render bk pw ph (Clear bg :: eval frameGeom clip t)), "software"

    // ── state ──
    let mutable playing = (docPath.IsNone)   // autoplay default clip; sit on a doc's first frame
    let mutable playhead = 0.0<s>
    let mutable winW, winH = 1320.0, 980.0
    let mutable zoom = 0.0       // px per second (0 = uninitialised → fit)
    let mutable pan = 0.0        // seconds at left edge of track
    let mutable panning = false
    let mutable mx, my = 0.0, 0.0
    let sw = Stopwatch.StartNew()
    let mutable lastTick = 0.0

    let trackW () = winW - gutter - 12.0
    let fitZoom () = if total > 0.0<s> then (trackW ()) / float total * 0.98 else 100.0
    let xOfT (t: float<s>) = gutter + (float t - pan) * zoom
    let tOfX (x: float) = pan + (x - gutter) / zoom
    let videoH () = winH - barH - tlH
    let fmt (t: float<s>) = let s = int (float t) in sprintf "%d:%02d" (s / 60) (s % 60)

    // ── timeline rendering ──
    let timelineCmds () : Cmd list =
        if zoom <= 0.0 then zoom <- fitZoom ()
        let tlTop = winH - tlH
        let tx, trkW = gutter, trackW ()
        let f = Theme.font ()
        let rows = TimelineView.flatten collapsed tlRoot
        let trackClip = rectPath (rect tx (tlTop) trkW tlH)
        // ruler ticks
        let interval = [ 0.5; 1.0; 2.0; 5.0; 10.0; 15.0; 30.0; 60.0; 120.0; 300.0 ] |> List.tryFind (fun s -> s * zoom >= 80.0) |> Option.defaultValue 600.0
        let ruler =
            let t0 = floor (pan / interval) * interval
            [ for k in 0 .. 200 do
                let t = t0 + float k * interval
                let x = xOfT (t * 1.0<s>)
                if x >= tx && x <= winW && t >= 0.0 then
                    yield Cmd.StrokePath([ MoveTo(vec2 x (tlTop + 4.0)); LineTo(vec2 x (tlTop + rulerH)) ], { Width = 1.0<px>; Cap = Butt; Join = Miter; Paint = Solid(Color.rgba 255uy 255uy 255uy 50uy); DashArray = []; DashOffset = 0.0<px>; MiterLimit = 4.0 })
                    yield DrawGlyphs(vec2 (x + 4.0) (tlTop + 17.0), Text.layout f 13.0 (fmt (t * 1.0<s>)), Solid(Color.rgb 150uy 158uy 176uy)) ]
        // rows
        let rowCmds =
            [ for i, row in List.indexed rows do
                let y = tlTop + rulerH + float i * rowH
                if y + rowH <= winH then
                    let it = row.Item
                    let dim = Color.rgb 150uy 158uy 176uy
                    // gutter: disclosure + indented label
                    let ind = 12.0 + float row.Depth * 14.0
                    if row.HasChildren then
                        let cx, cy = ind + 4.0, y + rowH / 2.0
                        if Set.contains row.Path collapsed then
                            yield FillTri(vec2 cx (cy - 5.0), vec2 cx (cy + 5.0), vec2 (cx + 7.0) cy, dim)
                        else
                            yield FillTri(vec2 (cx - 1.0) (cy - 3.0), vec2 (cx + 9.0) (cy - 3.0), vec2 (cx + 4.0) (cy + 5.0), dim)
                    let labCol = match it.Kind with KSeq | KLayer -> Color.rgb 120uy 128uy 146uy | _ -> Color.rgb 222uy 228uy 238uy
                    yield DrawGlyphs(vec2 (ind + 18.0) (y + rowH - 8.0), Text.layout f 14.0 it.Label, Solid labCol)
                    // bar (clipped to track)
                    let bx = xOfT it.Start
                    let bw = max 2.0 (float it.Dur * zoom)
                    let isContainer = (match it.Kind with KSeq | KLayer -> true | _ -> false)
                    let barCmds =
                        if isContainer then
                            [ StrokeRoundedRect(rect bx (y + 4.0) bw (rowH - 8.0), 4.0<px>, { Width = 1.5<px>; Cap = Butt; Join = Miter; Paint = Solid(kindColor it.Kind); DashArray = []; DashOffset = 0.0<px>; MiterLimit = 4.0 }) ]
                        else
                            [ FillRoundedRect(rect bx (y + 3.0) bw (rowH - 6.0), 4.0<px>, kindColor it.Kind) ]
                            @ (if bw > 46.0 then [ DrawGlyphs(vec2 (bx + 8.0) (y + rowH - 8.0), Text.layout f 12.0 it.Label, Solid(Color.rgba 255uy 255uy 255uy 235uy)) ] else [])
                    // adornment badges (right of label gutter)
                    let adornStr = String.concat " " it.Adorns
                    let adornCmds = if adornStr <> "" then [ DrawGlyphs(vec2 (gutter - 8.0 - Text.measureWidth f 11.0 adornStr) (y + rowH - 8.0), Text.layout f 11.0 adornStr, Solid(Color.rgba 150uy 160uy 200uy 200uy)) ] else []
                    yield! adornCmds
                    yield Cmd.Clip(trackClip, NonZero, barCmds) ]
        // playhead
        let phx = xOfT (clampT playhead)
        let playhead' = if phx >= tx && phx <= winW then [ Cmd.Clip(trackClip, NonZero, [ FillRect(rect (phx - 1.0) tlTop 2.0 tlH, Color.rgb 255uy 90uy 90uy) ]) ] else []
        [ FillRect(rect 0.0 tlTop winW tlH, Color.rgb 16uy 18uy 24uy)
          FillRect(rect 0.0 tlTop winW rulerH, Color.rgb 22uy 25uy 33uy)
          FillRect(rect (gutter - 1.0) tlTop 1.0 tlH, Color.rgba 255uy 255uy 255uy 30uy) ]
        @ [ AntiAlias(4, ruler @ rowCmds @ playhead') ]

    let win : IWindow =
        let title = "VideoEdit — Preview + Timeline"
        if not (String.IsNullOrEmpty(Environment.GetEnvironmentVariable "WAYLAND_DISPLAY")) then WaylandWindow(title, 1320, 980) :> IWindow
        else X11Window(title, 1320, 980) :> IWindow

    let buildWindow (w: float) (h: float) (fb: Framebuffer) : Cmd list =
        let vh = h - barH - tlH
        let scale = min (w / float pw) (vh / float ph)
        let dw, dh = float pw * scale, float ph * scale
        let vr = rect ((w - dw) / 2.0) ((vh - dh) / 2.0) dw dh
        let f = Theme.font ()
        let bs = 34.0
        let bx, by = 18.0, vh + (barH - bs) / 2.0
        let playIcon =
            if playing then [ FillRect(rect (bx + bs * 0.30) (by + bs * 0.24) (bs * 0.16) (bs * 0.52), Color.rgb 235uy 238uy 245uy); FillRect(rect (bx + bs * 0.56) (by + bs * 0.24) (bs * 0.16) (bs * 0.52), Color.rgb 235uy 238uy 245uy) ]
            else [ FillTri(vec2 (bx + bs * 0.32) (by + bs * 0.22), vec2 (bx + bs * 0.32) (by + bs * 0.78), vec2 (bx + bs * 0.80) (by + bs * 0.5), Color.rgb 235uy 238uy 245uy) ]
        let info = sprintf "%s / %s   ·   %s   ·   %s" (fmt (clampT playhead)) (fmt total) backendName (defaultArg docPath "demo")
        [ FillRect(rect 0.0 0.0 w h, Color.rgb 8uy 9uy 12uy)
          DrawImage(fb, rect 0.0 0.0 (float pw) (float ph), vr, true)
          FillRect(rect 0.0 vh w barH, Color.rgb 20uy 23uy 30uy) ]
        @ [ AntiAlias(4, playIcon @ [ DrawGlyphs(vec2 (bx + bs + 20.0) (by + bs - 11.0), Text.layout f 18.0 info, Solid(Color.rgb 180uy 190uy 208uy)) ]) ]
        @ timelineCmds ()

    // ── headless screenshot: --shot <png> [t] ──
    match Array.tryFindIndex ((=) "--shot") argv with
    | Some si ->
        let outPng = argv.[si + 1]
        let t = (if argv.Length > si + 2 then (match Double.TryParse argv.[si + 2] with true, v -> v | _ -> 4.0) else 4.0) * 1.0<s>
        playhead <- clampT t; playing <- false
        let ww, wh = 1320, 980
        winW <- float ww; winH <- float wh
        let fb = renderFrame (clampT t)
        let winFb = Renderer.Scene.render (Raster.create ()) ww wh (buildWindow (float ww) (float wh) fb)
        let raw = outPng + ".bgra"
        IO.File.WriteAllBytes(raw, MemoryMarshal.AsBytes(winFb.Px.Span).ToArray())
        let psi = ProcessStartInfo("ffmpeg")
        [ "-y"; "-loglevel"; "error"; "-f"; "rawvideo"; "-pixel_format"; "bgra"; "-video_size"; sprintf "%dx%d" ww wh; "-i"; raw; "-frames:v"; "1"; outPng ] |> List.iter psi.ArgumentList.Add
        (Process.Start psi).WaitForExit(); IO.File.Delete raw
        eprintfn "[preview] wrote %s" outPng
        0
    | None ->

    // ── interactive ──
    let onRender (w: int) (h: int) : Cmd list = winW <- float w; winH <- float h; buildWindow (float w) (float h) (renderFrame (clampT playhead))

    let onInput (ev: InputEvent) =
        match ev with
        | MouseMove(x, y) ->
            if panning then pan <- max 0.0 (pan - (x - mx) / zoom); win.RequestRedraw()
            mx <- x; my <- y
        | MouseButton(0, true, x, y) ->                                           // left button (platform reports 0-based)
            mx <- x; my <- y
            let vh = winH - barH - tlH
            let tlTop = winH - tlH
            let bs = 34.0
            let bx, by = 18.0, vh + (barH - bs) / 2.0
            if x >= bx && x <= bx + bs && y >= by && y <= by + bs then           // play/pause
                playing <- not playing; lastTick <- sw.Elapsed.TotalSeconds; win.RequestRedraw()
            elif y >= tlTop + rulerH then                                         // timeline rows
                let i = int ((y - tlTop - rulerH) / rowH)
                let rows = TimelineView.flatten collapsed tlRoot
                if x < gutter then
                    if i >= 0 && i < rows.Length && rows.[i].HasChildren then     // toggle expand/collapse
                        let p = rows.[i].Path
                        collapsed <- (if Set.contains p collapsed then Set.remove p collapsed else Set.add p collapsed)
                        win.RequestRedraw()
                else panning <- true; playhead <- clampT (tOfX x * 1.0<s>); win.RequestRedraw()   // seek + start pan
            elif y >= tlTop then playhead <- clampT (tOfX x * 1.0<s>); win.RequestRedraw()  // ruler → seek
        | MouseButton(0, false, _, _) -> panning <- false
        | MouseScroll(_, dy) ->                                                   // scroll/pinch → zoom the timeline
            if zoom <= 0.0 then zoom <- fitZoom ()
            let ax = if mx >= gutter then mx else gutter + (winW - gutter) / 2.0  // anchor at cursor, else track centre
            let tAt = tOfX ax
            let step = 1.08 ** abs dy
            zoom <- max 2.0 (min 6000.0 (zoom * (if dy > 0.0 then step else 1.0 / step)))
            pan <- max 0.0 (tAt - (ax - gutter) / zoom)
            win.RequestRedraw()
        | KeyDown(kc, _) ->                                                       // platform sends the X11 keycode (evdev)
            match kc with
            | 65 -> playing <- not playing; lastTick <- sw.Elapsed.TotalSeconds; win.RequestRedraw()   // space
            | 113 -> playhead <- clampT (playhead - 1.0<s> / float cfg.Fps); win.RequestRedraw()        // ←
            | 114 -> playhead <- clampT (playhead + 1.0<s> / float cfg.Fps); win.RequestRedraw()        // →
            | 110 -> playhead <- 0.0<s>; win.RequestRedraw()                                            // Home
            | _ -> ()
        | Resize(w, h) -> winW <- float w; winH <- float h; win.RequestRedraw()
        | Close -> win.Close()
        | _ -> ()

    let rec tick () =
        let now = sw.Elapsed.TotalSeconds
        let dt = now - lastTick
        lastTick <- now
        if playing && not panning then
            playhead <- playhead + dt * 1.0<s>
            if playhead >= total then playhead <- 0.0<s>
            win.RequestRedraw()
        win.SetTimeout(1000 / cfg.Fps, tick) |> ignore

    eprintfn "[preview] %s  backend=%s  %.1fs  %dx%d" (defaultArg docPath "demo") backendName (float total) pw ph
    lastTick <- sw.Elapsed.TotalSeconds
    win.SetTimeout(1000 / cfg.Fps, tick) |> ignore
    win.Run(onInput, onRender)
    0
