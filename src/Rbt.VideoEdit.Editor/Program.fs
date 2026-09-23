module Rbt.VideoEdit.Editor.Program

// The video editor shell: Chat (40%) | [Preview/Source tabs over Timeline] (60%).
// AI isn't wired yet — the chat is seeded with example turns so the layout is checkable.

open System
open System.Diagnostics
open System.Runtime.InteropServices
open Renderer
open Platform
open Platform.X11Window
open Platform.WaylandWindow
open Rbt.VideoEdit
open Rbt.VideoEdit.TimelineView

// ── theme: clean dark editor — neutral charcoal, crisp text, one restrained accent ──
let private ink = Color.rgb 226uy 227uy 230uy      // primary text (off-white)
let private dim = Color.rgb 132uy 136uy 142uy      // secondary text
let private faint = Color.rgb 92uy 95uy 101uy       // tertiary (ticks, hints)
let private accent = Color.rgb 122uy 158uy 196uy    // muted slate-blue (the one accent)
let private bg0 = Color.rgb 20uy 21uy 23uy          // window gaps / deepest
let private panelBg = Color.rgb 30uy 31uy 34uy      // chat + main panels
let private barBg = Color.rgb 38uy 39uy 43uy        // header / tab / control bars
let private well = Color.rgb 23uy 24uy 26uy         // sunken areas (input, source, track)
let private screen = Color.rgb 14uy 14uy 16uy       // preview (near-black)
let private hiE = Color.rgb 54uy 56uy 60uy          // subtle top edge (elevation light)
let private loE = Color.rgb 13uy 13uy 15uy          // subtle bottom edge (elevation dark)
let private hair = Color.rgb 48uy 50uy 54uy         // hairline separators
let private playCol = Color.rgb 232uy 233uy 236uy    // playhead (crisp light)

/// Flat panel with subtle horizontal elevation: a 1px top edge + 1px bottom edge.
let private bevel (raised: bool) (x: float) (y: float) (w: float) (h: float) (fill: Color) : Cmd list =
    let top, bot = if raised then hiE, loE else loE, hiE
    [ FillRect(rect x y w h, fill)
      FillRect(rect x y w 1.0, top)
      FillRect(rect x (y + h - 1.0) w 1.0, bot) ]

// muted, cohesive timeline tones on dark (visible but quiet, distinguished by faint hue)
let private kindColor (k: TKind) : Color =
    match k with
    | KFootage -> Color.rgb 62uy 70uy 82uy       // cool slate
    | KGraphic -> Color.rgb 80uy 72uy 64uy       // warm taupe
    | KAudio -> Color.rgb 58uy 78uy 74uy         // teal (echoes accent family)
    | KBg -> Color.rgb 56uy 58uy 64uy
    | KSection -> Color.rgb 72uy 66uy 78uy
    | KSeq | KLayer -> Color.rgba 120uy 124uy 132uy 150uy
    | _ -> Color.rgb 60uy 62uy 68uy

type Msg = { Role: string; Body: string; Diff: string option }
/// A clickable item in the Render dialog (shared by draw + hit-test). Group ∈ res|fps|qual|btn.
type DlgItem = { X: float; Y: float; W: float; H: float; Label: string; Sel: bool; Group: string; Idx: int }

[<EntryPoint>]
let main argv =
    Fx.install (); Templates.install (); Theme.useBoldFont Theme.defaultBoldFontPath
    let docPath = argv |> Array.tryFind (fun (a: string) -> a.EndsWith ".sexp") |> Option.defaultValue "docs/features.sexp"
    let cfg, clip = Sexp.loadDoc (IO.File.ReadAllText docPath)
    let sourceLines = (IO.File.ReadAllText docPath).Replace("\t", "  ").Split('\n')
    // Render-dialog option presets ("Source" = the document's own config).
    let resPresets = [| "Source", (cfg.W, cfg.H); "1080p", (1920, 1080); "720p", (1280, 720); "480p", (854, 480) |]
    let fpsPresets = [| "Source", cfg.Fps; "24", 24; "30", 30; "60", 60 |]
    let qualPresets = [| "High", (18, "slow"); "Medium", (20, "medium"); "Fast", (23, "veryfast") |]
    let pw, ph = cfg.W, cfg.H
    let frameGeom = { W = float pw; H = float ph }
    let total = duration clip
    let clampT (t: float<s>) = max 0.0<s> (min (total - 0.0005<s>) t)
    let tlRoot = TimelineView.build clip
    let mutable collapsed = TimelineView.initialCollapsed 1 tlRoot

    let renderFrame, backendName =
        let tryGl () =
            let pb = GlBackend.createPersistent pw ph
            let r (t: float<s>) = pb.Backend.Init pw ph; pb.Backend.Submit(Clear cfg.Background :: eval frameGeom clip t); pb.Backend.Finish() |> ignore; pb.Readback()
            r 0.0<s> |> ignore; r
        try tryGl (), "GLES"
        with _ -> let bk = Raster.create () in (fun (t: float<s>) -> Renderer.Scene.render bk pw ph (Clear cfg.Background :: eval frameGeom clip t)), "sw"

    // ── state ──
    let mutable winW, winH = 1500.0, 940.0
    let mutable tab = 0                  // 0 = preview, 1 = source
    let mutable playing = false
    let mutable playhead = 0.0<s>
    let mutable zoom = 0.0
    let mutable pan = 0.0
    let mutable panning = false
    let mutable shift = false
    let mutable srcScroll = 0.0
    let mutable chatScroll = 0.0
    let mutable input = ""
    let mutable chatFocused = false      // keyboard → chat box when focused, else → transport
    let mutable mx, my = 0.0, 0.0
    // ── render dialog ──
    let mutable showDialog = false
    let mutable dRes = 0
    let mutable dFps = 0
    let mutable dQual = 0
    let mutable rendering = false
    let mutable renderMsg = ""
    let sw = Stopwatch.StartNew()
    let mutable lastTick = 0.0
    let messages = ResizeArray<Msg>([
        { Role = "you"; Body = "make the opening punchier"; Diff = None }
        { Role = "assistant"; Body = "Tightened the intro crossfade and added a flash on the title card."; Diff = Some "~ sequence: crossfade 0.7s → 0.4s    + feature·THE DEEP fx:flash" }
        { Role = "you"; Body = "the grid section should pop more"; Diff = None }
        { Role = "assistant"; Body = "Bumped its saturation and switched the accent to cyan."; Diff = Some "~ feature·THE GRID.accent #FFD200 → #00EAFF    ~ fx:saturate 1.12 → 1.4" } ])

    // ── geometry ──
    let menuH = 26.0
    let chatW () = winW * 0.40
    let tabBarH = 34.0
    let tlH = 300.0
    let inputH = 64.0
    let tlGutter = 168.0
    let rightX () = chatW ()
    let tlTop () = winH - tlH
    let trackX () = rightX () + tlGutter
    let trackW () = winW - trackX () - 10.0
    let fitZoom () = if total > 0.0<s> then trackW () / float total * 0.98 else 100.0
    let xOfT (t: float<s>) = trackX () + (float t - pan) * zoom
    let tOfX (x: float) = pan + (x - trackX ()) / zoom
    let fmt (t: float<s>) = let s = int (float t) in sprintf "%d:%02d" (s / 60) (s % 60)

    // neutral grotesque for UI (NeXT used Helvetica), mono for code/source/numbers
    let fUi () = Theme.load "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
    let fUiB () = Theme.load "/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf"
    let fMono () = Theme.load "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
    let txt size col x baseY (s: string) = DrawGlyphs(vec2 x baseY, Text.layout (fUiB ()) size s, Solid col)
    let txtm size col x baseY (s: string) = DrawGlyphs(vec2 x baseY, Text.layout (fUi ()) size s, Solid col)
    let txtmono size col x baseY (s: string) = DrawGlyphs(vec2 x baseY, Text.layout (fMono ()) size s, Solid col)
    /// tracked uppercase label (returns a Cmd list)
    let txtc size col spacing x baseY (s: string) : Cmd list =
        let fnt = fUi ()
        [ let mutable cx = x
          for ch in s.ToUpper() do
            let g = string ch
            yield DrawGlyphs(vec2 cx baseY, Text.layout fnt size g, Solid col)
            cx <- cx + Text.measureWidth fnt size g + spacing ]
    let wrap size maxW (s: string) =
        let words = s.Split(' ')
        let lines = ResizeArray<string>()
        let mutable cur = ""
        for w in words do
            let t = if cur = "" then w else cur + " " + w
            if Text.measureWidth (fUi ()) size t > maxW && cur <> "" then lines.Add cur; cur <- w else cur <- t
        if cur <> "" then lines.Add cur
        List.ofSeq lines

    // ── timeline panel (right pane bottom) ──
    let timelineCmds () =
        if zoom <= 0.0 then zoom <- fitZoom ()
        let tx, top = trackX (), tlTop ()
        let rx = rightX ()
        let clipRect = rectPath (rect rx top (winW - rx) tlH)
        let rulerH = 24.0
        let interval = [ 0.5; 1.0; 2.0; 5.0; 10.0; 15.0; 30.0; 60.0 ] |> List.tryFind (fun s -> s * zoom >= 80.0) |> Option.defaultValue 120.0
        let ruler =
            let t0 = floor (pan / interval) * interval
            [ for k in 0 .. 120 do
                let t = t0 + float k * interval
                let x = xOfT (t * 1.0<s>)
                if x >= tx && x <= winW && t >= 0.0 then
                    yield txtmono 10.0 faint (x + 5.0) (top + 16.0) (fmt (t * 1.0<s>)) ]
        let rows = TimelineView.flatten collapsed tlRoot
        let rowH = 24.0
        let rowCmds =
            [ for i, row in List.indexed rows do
                let y = top + rulerH + float i * rowH
                if y + rowH <= winH then
                    let it = row.Item
                    let indent = rx + 10.0 + float row.Depth * 13.0
                    if row.HasChildren then
                        let cy = y + rowH / 2.0
                        if Set.contains row.Path collapsed then yield FillTri(vec2 indent (cy - 4.0), vec2 indent (cy + 4.0), vec2 (indent + 6.0) cy, dim)
                        else yield FillTri(vec2 (indent - 1.0) (cy - 2.0), vec2 (indent + 7.0) (cy - 2.0), vec2 (indent + 3.0) (cy + 4.0), dim)
                    let labCol = match it.Kind with KSeq | KLayer -> dim | _ -> ink
                    yield txtm 12.0 labCol (indent + 14.0) (y + rowH - 7.0) it.Label
                    let bx = xOfT it.Start
                    let bw = max 2.0 (float it.Dur * zoom)
                    let bars =
                        match it.Kind with
                        | KSeq | KLayer -> [ StrokeRoundedRect(rect bx (y + 4.0) bw (rowH - 8.0), 3.0<px>, { Width = 1.0<px>; Cap = Butt; Join = Miter; Paint = Solid(kindColor it.Kind); DashArray = []; DashOffset = 0.0<px>; MiterLimit = 4.0 }) ]
                        | _ -> [ FillRoundedRect(rect bx (y + 3.0) bw (rowH - 6.0), 3.0<px>, kindColor it.Kind)
                                 FillRect(rect (bx + 1.0) (y + 3.0) (max 0.0 (bw - 2.0)) 1.0, Color.rgba 255uy 255uy 255uy 20uy) ]
                               @ (if bw > 44.0 then [ txtm 11.0 (Color.rgb 206uy 210uy 216uy) (bx + 8.0) (y + rowH - 7.0) it.Label ] else [])
                    yield Cmd.Clip(clipRect, NonZero, bars) ]
        let phx = xOfT (clampT playhead)
        let ph' = if phx >= tx && phx <= winW then [ Cmd.Clip(clipRect, NonZero, [ FillRect(rect (phx - 1.0) top 2.0 tlH, playCol) ]) ] else []
        // transport: a raised beveled play/pause button + current/total time
        let bbx, bby, bbw, bbh = rx + 10.0, top + 4.0, 24.0, 18.0
        let gx, gy = bbx + 8.0, bby + 4.0
        let playGlyph =
            if playing then [ FillRect(rect gx gy 3.0 10.0, ink); FillRect(rect (gx + 5.0) gy 3.0 10.0, ink) ]
            else [ FillTri(vec2 gx gy, vec2 gx (gy + 10.0), vec2 (gx + 9.0) (gy + 5.0), ink) ]
        let transport = (bevel true bbx bby bbw bbh barBg) @ playGlyph @ [ txtmono 11.0 ink (bbx + bbw + 10.0) (top + 15.0) (sprintf "%s / %s" (fmt (clampT playhead)) (fmt total)) ]
        [ FillRect(rect rx top (winW - rx) tlH, panelBg)
          FillRect(rect rx top (winW - rx) 1.0, hair) ]
        @ bevel false tx (top + 1.0) (winW - tx - 1.0) (tlH - 1.0) well
        @ [ FillRect(rect (tx + 1.0) (top + 1.0) (winW - tx - 2.0) rulerH, panelBg)
            FillRect(rect (tx + 1.0) (top + rulerH) (winW - tx - 2.0) 1.0, loE)
            FillRect(rect tx top 1.0 tlH, loE) ]
        @ [ AntiAlias(4, transport @ ruler @ rowCmds @ ph') ]

    // ── chat panel (left) ──
    let chatCmds () =
        let cw = chatW ()
        let headerH = 34.0
        let inputY = winH - inputH
        let pad = 16.0
        let msgCmds =
            let mutable y = menuH + headerH + 14.0 - chatScroll
            [ for m in messages do
                let isYou = m.Role = "you"
                yield txtm 11.0 (if isYou then accent else dim) pad (y + 9.0) (if isYou then "You" else "Assistant")
                y <- y + 17.0
                for ln in wrap 13.0 (cw - pad * 2.0) m.Body do
                    yield txtm 13.0 ink pad (y + 11.0) ln
                    y <- y + 18.0
                match m.Diff with
                | Some d ->
                    let dl = wrap 11.0 (cw - pad * 2.0 - 20.0) d
                    let dhh = float (List.length dl) * 16.0 + 14.0
                    y <- y + 6.0
                    yield FillRect(rect pad y (cw - pad * 2.0) dhh, well)
                    yield FillRect(rect pad y 2.0 dhh, accent)
                    let mutable yy = y + 7.0
                    for ln in dl do
                        yield txtmono 11.0 (Color.rgb 150uy 162uy 174uy) (pad + 12.0) (yy + 11.0) ln
                        yy <- yy + 16.0
                    y <- y + dhh
                | None -> ()
                y <- y + 16.0 ]
        let inputText = if input = "" then "Tell the editor what to change…" else input + "▏"
        let inputCol = if input = "" then faint else ink
        [ FillRect(rect 0.0 menuH cw (winH - menuH), panelBg)
          FillRect(rect 0.0 menuH cw headerH, barBg); FillRect(rect 0.0 (menuH + headerH - 1.0) cw 1.0, loE)
          FillRect(rect (cw - 1.0) menuH 1.0 (winH - menuH), loE) ]
        @ [ AntiAlias(4, [ txtm 11.5 dim pad (menuH + 21.0) "Chat" ] @ [ Cmd.Clip(rectPath (rect 0.0 (menuH + headerH) cw (inputY - menuH - headerH)), NonZero, msgCmds) ]) ]
        @ [ FillRect(rect 0.0 inputY cw inputH, panelBg); FillRect(rect 0.0 inputY cw 1.0, hair) ]
        @ bevel false pad (inputY + 12.0) (cw - pad * 2.0) (inputH - 24.0) well
        @ [ AntiAlias(4, [ txtm 13.0 inputCol (pad + 12.0) (inputY + inputH / 2.0 + 4.0) inputText ]) ]

    // ── menu bar (one item: Render) ──
    let menuItemX0, menuItemX1 = 8.0, 72.0
    let menuBar (w: float) : Cmd list =
        [ FillRect(rect 0.0 0.0 w menuH, barBg); FillRect(rect 0.0 (menuH - 1.0) w 1.0, loE) ]
        @ (if showDialog then [ FillRect(rect menuItemX0 0.0 (menuItemX1 - menuItemX0) menuH, panelBg)
                                FillRect(rect menuItemX0 0.0 (menuItemX1 - menuItemX0) 2.0, accent) ] else [])
        @ [ txtm 12.5 ink (menuItemX0 + 10.0) (menuH / 2.0 + 4.5) "Render" ]

    // ── render dialog: option chips + buttons, laid out once and shared by draw + hit-test ──
    let dialogItems (w: float) (h: float) : DlgItem list =
        let dw, dh = 470.0, 312.0
        let dx, dy = floor ((w - dw) / 2.0), floor ((h - dh) / 2.0)
        let chipW, gap, chipX0 = 80.0, 8.0, 112.0
        let mk grp idx label x y ww = { X = x; Y = y; W = ww; H = 28.0; Label = label; Sel = false; Group = grp; Idx = idx }
        let row grp (opts: string[]) (sel: int) (cy: float) =
            [ for j in 0 .. opts.Length - 1 ->
                { mk grp j opts.[j] (dx + chipX0 + float j * (chipW + gap)) (cy - 14.0) chipW with Sel = (sel = j) } ]
        (row "res" (resPresets |> Array.map fst) dRes (dy + 74.0))
        @ (row "fps" (fpsPresets |> Array.map fst) dFps (dy + 116.0))
        @ (row "qual" (qualPresets |> Array.map fst) dQual (dy + 158.0))
        @ [ { mk "btn" 0 "Cancel" (dx + dw - 204.0) (dy + dh - 44.0) 90.0 with H = 30.0 }
            { mk "btn" 1 "Render" (dx + dw - 104.0) (dy + dh - 44.0) 90.0 with H = 30.0 } ]

    let dialogCmds (w: float) (h: float) : Cmd list =
        if not showDialog then [] else
        let dw, dh = 470.0, 312.0
        let dx, dy = floor ((w - dw) / 2.0), floor ((h - dh) / 2.0)
        let outp = "out/" + IO.Path.GetFileNameWithoutExtension docPath + ".mp4"
        let chip (it: DlgItem) =
            if it.Group = "btn" then
                let prim = it.Label = "Render"
                bevel true it.X it.Y it.W it.H (if prim then accent else barBg)
                @ [ txtm 12.5 (if prim then Color.rgb 18uy 18uy 22uy else ink)
                        (it.X + (it.W - Text.measureWidth (fUi ()) 12.5 it.Label) / 2.0) (it.Y + 20.0) it.Label ]
            else
                (if it.Sel then bevel false it.X it.Y it.W it.H accent else bevel true it.X it.Y it.W it.H well)
                @ [ txtm 12.0 (if it.Sel then Color.rgb 18uy 18uy 22uy else ink) (it.X + 10.0) (it.Y + 18.0) it.Label ]
        [ FillRect(rect 0.0 0.0 w h, Color.rgba 0uy 0uy 0uy 150uy) ]
        @ bevel true dx dy dw dh panelBg
        @ [ AntiAlias(4,
              [ txt 15.5 ink (dx + 24.0) (dy + 33.0) "Render"
                FillRect(rect dx (dy + 46.0) dw 1.0, loE)
                txtm 12.0 dim (dx + 24.0) (dy + 77.0) "Resolution"
                txtm 12.0 dim (dx + 24.0) (dy + 119.0) "Frame rate"
                txtm 12.0 dim (dx + 24.0) (dy + 161.0) "Quality"
                txtm 12.0 dim (dx + 24.0) (dy + 200.0) "Output"
                txtmono 11.0 ink (dx + 112.0) (dy + 200.0) outp
                txtm 11.0 (if rendering then accent else dim) (dx + 24.0) (dy + 224.0) renderMsg ]
              @ (dialogItems w h |> List.collect chip)) ]

    // ── right pane: tabs + content + timeline ──
    let buildWindow (w: float) (h: float) (fb: Framebuffer option) =
        winW <- w; winH <- h
        let rx = rightX ()
        let contentTop = menuH + tabBarH
        let tabW = 110.0
        let tabBtn i label =
            let bx = rx + float i * tabW
            let active = (tab = i)
            [ FillRect(rect bx menuH tabW tabBarH, (if active then panelBg else barBg)) ]
            @ (if active then [ FillRect(rect bx menuH tabW 2.0, accent) ] else [ FillRect(rect bx (menuH + tabBarH - 1.0) tabW 1.0, loE) ])
            @ [ txtm 12.5 (if active then ink else dim) (bx + 16.0) (menuH + 22.0) label ]
        let tabs =
            [ FillRect(rect rx menuH (w - rx) tabBarH, barBg); FillRect(rect rx (menuH + tabBarH - 1.0) (w - rx) 1.0, loE) ]
            @ [ AntiAlias(4, tabBtn 0 "Preview" @ tabBtn 1 "Source"
                             @ [ txtmono 11.0 faint (w - 234.0) (menuH + 22.0) (sprintf "%s   %s   %s" (fmt (clampT playhead)) backendName (IO.Path.GetFileName docPath)) ]) ]
        // tab 0 = the projections (preview + timeline); tab 1 = the source (the truth), full height
        let content =
            if tab = 0 then
                let pH = (tlTop ()) - contentTop
                let frame =
                    match fb with
                    | Some f ->
                        let scale = min ((w - rx) / float pw) (pH / float ph)
                        let dw, dh = float pw * scale, float ph * scale
                        let dx, dy = rx + ((w - rx) - dw) / 2.0, contentTop + (pH - dh) / 2.0
                        (bevel false rx contentTop (w - rx) pH screen) @ [ DrawImage(f, rect 0.0 0.0 (float pw) (float ph), rect dx dy dw dh, true) ]
                    | None -> bevel false rx contentTop (w - rx) pH screen
                frame @ timelineCmds ()
            else
                let sH = h - contentTop
                let lineH = 16.0
                let lines =
                    [ let mutable y = contentTop + 22.0 - srcScroll
                      for ln in sourceLines do
                          if y > contentTop - lineH && y < h then yield txtmono 12.0 (Color.rgb 198uy 202uy 208uy) (rx + 18.0) y (ln.TrimEnd())
                          y <- y + lineH ]
                (bevel false rx contentTop (w - rx) sH well)
                @ [ Cmd.Clip(rectPath (rect rx contentTop (w - rx) sH), NonZero, [ AntiAlias(4, lines) ]) ]
        [ FillRect(rect 0.0 0.0 w h, panelBg) ] @ chatCmds () @ tabs @ content @ menuBar w @ dialogCmds w h

    let win : IWindow =
        let title = "VideoEdit — Editor"
        if not (String.IsNullOrEmpty(Environment.GetEnvironmentVariable "WAYLAND_DISPLAY")) then WaylandWindow(title, 1500, 940) :> IWindow
        else X11Window(title, 1500, 940) :> IWindow

    // ── headless shot ──
    match Array.tryFindIndex ((=) "--shot") argv with
    | Some si ->
        let outPng = argv.[si + 1]
        let t = (if argv.Length > si + 2 then (match Double.TryParse argv.[si + 2] with | true, v -> v | _ -> 6.0) else 6.0) * 1.0<s>
        playhead <- clampT t
        if Array.contains "--dialog" argv then showDialog <- true
        let ww, wh = 1500, 940
        let fb = renderFrame (clampT t)
        let winFb = Renderer.Scene.render (Raster.create ()) ww wh (buildWindow (float ww) (float wh) (Some fb))
        let raw = outPng + ".bgra"
        IO.File.WriteAllBytes(raw, MemoryMarshal.AsBytes(winFb.Px.Span).ToArray())
        let psi = ProcessStartInfo("ffmpeg")
        [ "-y"; "-loglevel"; "error"; "-f"; "rawvideo"; "-pixel_format"; "bgra"; "-video_size"; sprintf "%dx%d" ww wh; "-i"; raw; "-frames:v"; "1"; outPng ] |> List.iter psi.ArgumentList.Add
        (Process.Start psi).WaitForExit(); IO.File.Delete raw
        eprintfn "[editor] wrote %s" outPng
        0
    | None ->

    let onRender (w: int) (h: int) : Cmd list =
        let fb = if tab = 0 then Some(renderFrame (clampT playhead)) else None
        buildWindow (float w) (float h) fb

    let send () =
        if input.Trim() <> "" then
            messages.Add { Role = "you"; Body = input.Trim(); Diff = None }
            messages.Add { Role = "assistant"; Body = "(AI isn't connected yet — this is the editor shell. Wiring chatkit's agent next.)"; Diff = None }
            input <- ""; win.RequestRedraw()

    /// Render the loaded document to MP4 with the dialog's chosen options (off the UI thread).
    let doRender () =
        if not rendering then
            let _, (rw, rh) = resPresets.[dRes]
            let _, fps = fpsPresets.[dFps]
            let _, (crf, preset) = qualPresets.[dQual]
            let outp = "out/" + IO.Path.GetFileNameWithoutExtension docPath + ".mp4"
            let rc : RenderConfig = { W = rw; H = rh; Fps = fps; Background = cfg.Background; Output = outp; Crf = crf; Preset = preset }
            rendering <- true
            renderMsg <- sprintf "Rendering  %d×%d  %dfps …" rw rh fps
            win.RequestRedraw()
            IO.Directory.CreateDirectory "out" |> ignore
            System.Threading.Tasks.Task.Run(fun () ->
                (try renderToMp4 rc clip; renderMsg <- "✓  wrote " + outp
                 with e -> renderMsg <- "✗  " + e.Message)
                rendering <- false
                win.RequestRedraw())
            |> ignore

    let onInput (ev: InputEvent) =
        match ev with
        | MouseMove(x, y) -> (if panning then pan <- max 0.0 (pan - (x - mx) / zoom); win.RequestRedraw()); mx <- x; my <- y
        | MouseButton(0, true, x, y) when showDialog ->
            mx <- x; my <- y
            match dialogItems winW winH |> List.tryFind (fun it -> x >= it.X && x <= it.X + it.W && y >= it.Y && y <= it.Y + it.H) with
            | Some it ->
                (match it.Group with
                 | "res" -> dRes <- it.Idx
                 | "fps" -> dFps <- it.Idx
                 | "qual" -> dQual <- it.Idx
                 | _ -> if it.Label = "Render" then doRender () else (showDialog <- false))
                win.RequestRedraw()
            | None -> ()                                         // click outside controls: stay open (modal)
        | MouseButton(0, true, x, y) when y < menuH ->
            mx <- x; my <- y
            if x >= menuItemX0 && x <= menuItemX1 then showDialog <- true; win.RequestRedraw()
        | MouseButton(0, true, x, y) ->
            mx <- x; my <- y
            let rx = rightX ()
            chatFocused <- (x < rx)                              // click chat → type; click right pane → space plays
            if y >= menuH && y < menuH + tabBarH && x >= rx then
                tab <- (if x < rx + 110.0 then 0 elif x < rx + 220.0 then 1 else tab); win.RequestRedraw()
            elif tab = 0 && y >= tlTop () && x >= rx then
                if y < tlTop () + 24.0 && x < rx + 32.0 then     // play/pause button
                    playing <- not playing; lastTick <- sw.Elapsed.TotalSeconds; win.RequestRedraw()
                else
                    let i = int ((y - tlTop () - 24.0) / 24.0)
                    let rows = TimelineView.flatten collapsed tlRoot
                    if x < trackX () then
                        (if i >= 0 && i < rows.Length && rows.[i].HasChildren then
                            let p = rows.[i].Path in collapsed <- (if Set.contains p collapsed then Set.remove p collapsed else Set.add p collapsed); win.RequestRedraw())
                    else panning <- true; playhead <- clampT (tOfX x * 1.0<s>); win.RequestRedraw()
        | MouseButton(0, false, _, _) -> panning <- false
        | MouseScroll(_, dy) ->
            if tab = 0 && mx >= rightX () && my >= tlTop () then
                if zoom <= 0.0 then zoom <- fitZoom ()
                let ax = if mx >= trackX () then mx else trackX ()
                let tAt = tOfX ax
                zoom <- max 2.0 (min 6000.0 (zoom * (1.08 ** (if dy > 0.0 then abs dy else -abs dy))))
                pan <- max 0.0 (tAt - (ax - trackX ()) / zoom); win.RequestRedraw()
            elif mx >= rightX () && tab = 1 then srcScroll <- max 0.0 (srcScroll - dy * 40.0); win.RequestRedraw()
            else chatScroll <- max 0.0 (chatScroll - dy * 40.0); win.RequestRedraw()
        | KeyDown(kc, _) when showDialog ->
            if kc = 9 then showDialog <- false; win.RequestRedraw()          // Escape closes the dialog
            elif kc = 36 then doRender ()                                    // Enter renders
        | KeyDown(kc, _) ->
            if kc = 50 || kc = 62 then shift <- true
            elif chatFocused then
                match Chatkit.KeyMap.translate kc shift with
                | Some "Enter" -> send ()
                | Some "Backspace" -> (if input.Length > 0 then input <- input.Substring(0, input.Length - 1)); win.RequestRedraw()
                | Some s when s.Length = 1 -> input <- input + s; win.RequestRedraw()
                | _ -> ()
            else
                match kc with
                | 65 -> playing <- not playing; lastTick <- sw.Elapsed.TotalSeconds; win.RequestRedraw()     // space
                | 113 -> playhead <- clampT (playhead - 1.0<s> / float cfg.Fps); win.RequestRedraw()         // ←
                | 114 -> playhead <- clampT (playhead + 1.0<s> / float cfg.Fps); win.RequestRedraw()         // →
                | 110 -> playhead <- 0.0<s>; win.RequestRedraw()                                             // Home
                | _ -> ()
        | KeyUp(kc, _) -> if kc = 50 || kc = 62 then shift <- false
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

    eprintfn "[editor] %s  backend=%s  %.1fs" docPath backendName (float total)
    lastTick <- sw.Elapsed.TotalSeconds
    win.SetTimeout(1000 / cfg.Fps, tick) |> ignore
    win.Run(onInput, onRender)
    0
