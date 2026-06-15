module VideoEdit.Sample.Scenes

open Renderer
open VideoEdit

// ── shared config ──
let asset n = "assets/" + n
let outp n = "out/" + n

let private cfg w h fps name : RenderConfig =
    { W = w; H = h; Fps = fps; Background = Theme.bg
      Output = outp name; Crf = 19; Preset = "medium" }

// loud card palette for the transition showcase
let private palette =
    [| Color.rgb 224uy 49uy 49uy;  Color.rgb 33uy 102uy 224uy; Color.rgb 245uy 159uy 0uy
       Color.rgb 20uy 160uy 90uy;  Color.rgb 156uy 54uy 224uy; Color.rgb 224uy 49uy 132uy
       Color.rgb 0uy 150uy 160uy;  Color.rgb 120uy 90uy 30uy |]

let private card (col: Color) (label: string) : Clip =
    layer (solidBg 1.4<s> col) [ graphic 1.4<s> (Components.centeredLabel label 92.0 Theme.white) ]

// ── inline drawing helpers for the scenegraph demo ──
let private textC (cx: float) (baseY: float) (size: float) (col: Color) (s: string) : Cmd =
    let f = Theme.font ()
    DrawGlyphs(vec2 (cx - Text.measureWidth f size s / 2.0) baseY, Text.layout f size s, Solid col)

let private vcenter (size: float) (cy: float) : float =
    let f = Theme.font ()
    cy + (Text.ascender f size + Text.descender f size) / 2.0

let private cardBg (x: float) (y: float) (w: float) (h: float) : Cmd list =
    let r = rect x y w h
    let stroke : StrokeStyle =
        { Width = 6.0<px>; Cap = Butt; Join = Miter; Paint = Solid Theme.gold
          DashArray = []; DashOffset = 0.0<px>; MiterLimit = 4.0 }
    aa [ BoxShadow(r, 0.0, 16.0, 34, 2.0<px>, Theme.shadowStrong, false, Some 22.0<px>)
         FillRoundedRect(r, 22.0<px>, Color.rgba 16uy 20uy 56uy 235uy)
         StrokeRoundedRect(r, 22.0<px>, stroke) ]

let private badge (cx: float) (cy: float) (label: string) (col: Color) : Cmd list =
    let f = Theme.font ()
    let size = 34.0
    let bw = Text.measureWidth f size label + 48.0
    let bh = 60.0
    let x, y = cx - bw / 2.0, cy - bh / 2.0
    aa [ FillRoundedRect(rect x y bw bh, (bh / 2.0) * 1.0<px>, col)
         textC cx (vcenter size cy) size Theme.white label ]

// count-up number that eases 0→100 over `dur` seconds of local time
let private countUp (cx: float) (cy: float) (size: float) (col: Color) (dur: float) : Frame -> float<s> -> Cmd list =
    fun _ t ->
        let u = min 1.0 (max 0.0 (float t / dur))
        let e = let p = 1.0 - u in 1.0 - p * p * p
        aa [ Filtered([ DropShadow(0.0, 6.0, 12, Theme.shadowStrong) ],
                      [ textC cx (vcenter size cy) size col (string (int (round (100.0 * e)))) ]) ]

// ════════════════════ 1. TRANSITION SHOWCASE (vector) ════════════════════
let transitions () =
    let trans : (string * (Clip -> Clip -> Clip)) list =
        [ "CROSSFADE", crossfade 0.6<s>
          "SLIDE",     slide "left" 0.6<s>
          "PUSH UP",   push "up" 0.6<s>
          "WIPE",      wipe "left" 0.6<s>
          "IRIS",      iris 0.7<s>
          "ZOOM",      zoomTrans 0.6<s>
          "SPIN",      spinTrans 0.7<s>
          "WHIP",      whip "left" 0.45<s>
          "DIP BLACK", dipBlack 0.6<s>
          "SCALE FADE", scaleFade 0.6<s> ]
    let n = List.length trans
    // card i is labeled with the transition that FOLLOWS it; last card = END
    let cards =
        [ for i in 0 .. n ->
            let label = if i < n then fst trans.[i] else "END"
            card palette.[i % palette.Length] label ]
    // fold cards together using trans.[i] between card i and i+1
    let mutable acc = cards.[0]
    for i in 0 .. n - 1 do
        acc <- (snd trans.[i]) acc cards.[i + 1]
    renderToMp4 (cfg 1280 720 30 "transitions.mp4") acc

// ════════════════════ 2. EFFECT SHOWCASE (footage) ════════════════════
let effects () =
    let w, h, fps = 854, 480, 24
    let src = Video.load (asset "bunny.mp4") w h fps
    let fxs : (string * (Clip -> Clip)) list =
        [ "ORIGINAL", id
          "ZOOM",      zoom 1.0 1.3
          "KEN BURNS", kenBurns
          "PULSE",     pulse
          "SHAKE",     shake 16.0
          "GRAYSCALE", grayscale
          "SEPIA",     sepia
          "SATURATE",  saturate 1.9
          "CONTRAST",  contrast 1.5
          "HUE SHIFT", hueShift
          "BLUR",      blurFx 9
          "TINT RED",  tint (Color.rgb 255uy 40uy 40uy) 0.35
          "VIGNETTE",  vignette
          "FLASH",     flash ]
    let seg (name, fx) =
        layer (videoFor 1.3<s> src |> fx)
              [ graphic 1.3<s> (Components.caption name)
                graphic 1.3<s> (Components.cornerBadge "FX" (Color.rgb 224uy 49uy 49uy)) ]
    renderToMp4 (cfg w h fps "effects.mp4") (concat (List.map seg fxs))

// ════════════════════ 3. NATURE MINI-DOC (coherent) ════════════════════
let nature () =
    let w, h, fps = 960, 540, 24
    let jelly  = Video.loadClip (asset "jellyfish.mp4") 0.0 4.0 w h fps
    let bunny  = Video.loadClip (asset "bunny.mp4")     2.0 4.0 w h fps
    let sintel = Video.loadClip (asset "sintel.mp4")    1.0 4.0 w h fps

    let intro =
        layer (videoFor 2.8<s> jelly |> tint Color.black 0.45 |> blurFx 2)
              [ graphic 2.8<s> (Components.titleCard
                  { Headline = "NATURE'S WONDERS"; Subtitle = Some "a short showcase"
                    Size = 96.0; Fill = Theme.gold; Shadow = Theme.shadowStrong }) ]

    let scene src name title =
        let d = Video.duration src
        layer (video src |> kenBurns |> saturate 1.12)
              [ graphic d (Components.lowerThird { Name = name; Title = title; Accent = Theme.gold }) |> fadeInOut 0.4<s> 0.4<s>
                graphic d (Components.cornerBadge "HD" (Color.rgb 33uy 102uy 224uy)) |> fadeIn 0.3<s> ]

    let outro =
        layer (solidBg 2.2<s> (Color.rgb 8uy 10uy 30uy))
              [ graphic 2.2<s> (Components.centeredLabel "THE END" 110.0 Theme.gold) |> fadeInOut 0.5<s> 0.5<s> ]

    let doc =
        chain (crossfade 0.7<s>)
            [ intro
              scene jelly  "JELLYFISH"  "Pelagic drifters"
              scene bunny  "BIG BUCK BUNNY" "Blender open movie"
              scene sintel "SINTEL"     "Durian open movie" ]
    renderToMp4 (cfg w h fps "nature.mp4") (dipBlack 0.6<s> doc outro)

// ════════════════════ 4. TOP 3 COUNTDOWN (energetic cuts) ════════════════════
let top3 () =
    let w, h, fps = 960, 540, 24
    let clips =
        [ "#3", "JELLYFISH", "ocean ambience", Video.loadClip (asset "jellyfish.mp4") 1.0 3.0 w h fps
          "#2", "SINTEL",    "epic fantasy",   Video.loadClip (asset "sintel.mp4")    3.0 3.0 w h fps
          "#1", "BIG BUCK BUNNY", "comedy classic", Video.loadClip (asset "bunny.mp4") 4.0 3.0 w h fps ]
    let intro =
        layer (solidBg 2.0<s> (Color.rgb 16uy 20uy 56uy))
              [ graphic 2.0<s> (Components.titleCard
                  { Headline = "TOP 3 CLIPS"; Subtitle = Some "the countdown"
                    Size = 110.0; Fill = Theme.gold; Shadow = Theme.shadowStrong }) ]
    let seg (rank, name, title, src) =
        let d = Video.duration src
        layer (video src |> kenBurns |> saturate 1.1)
              [ graphic d (Components.bigNumber rank (Color.rgb 255uy 210uy 0uy) 200.0) |> fadeInOut 0.3<s> 0.3<s> |> shake 8.0
                graphic d (Components.lowerThird { Name = name; Title = title; Accent = Color.rgb 224uy 49uy 49uy }) |> slideInFx "left" 0.4<s> ]
    // energetic: whip between ranks
    let body = chain (whip "left" 0.45<s>) (List.map seg clips)
    renderToMp4 (cfg w h fps "top3.mp4") (push "up" 0.5<s> intro body)

// ════════════════════ 5. QUIZ INTRO (countdown + score) ════════════════════
let quiz () =
    let w, h, fps = 960, 540, 24
    let bg = Video.loadClip (asset "city.mp4") 0.0 9.0 w h fps
    let title =
        layer (videoFor 2.4<s> bg |> tint (Color.rgb 20uy 20uy 80uy) 0.4 |> blurFx 2)
              [ graphic 2.4<s> (Components.titleCard
                  { Headline = "QUIZ NIGHT"; Subtitle = Some "ROUND 1"
                    Size = 150.0; Fill = Theme.gold; Shadow = Theme.shadowStrong }) ]
    let count =
        layer (videoFor 3.2<s> bg |> zoom 1.0 1.15 |> tint Color.black 0.35)
              [ graphic 3.2<s> (Components.countdownNumber 3 (Color.rgb 255uy 210uy 0uy)) ]
    let go =
        layer (videoFor 2.4<s> bg |> saturate 1.3)
              [ graphic 2.4<s> (Components.centeredLabel "GO!" 240.0 Theme.gold) |> scaleIn |> flash
                graphic 2.4<s> (Components.infoBox
                  { Label = "SCORE"; Value = "0"; Fill = Color.rgb 224uy 49uy 49uy
                    Accent = Theme.gold; Radius = 26.0; BorderWidth = 8.0 }) ]
    renderToMp4 (cfg w h fps "quiz.mp4")
        (chain (dipBlack 0.45<s>) [ title; count; go ])

// ════════════════════ 6. COMPONENT SHOWCASE (one static frame, 4s) ════════════════════
let components () =
    let w, h, fps = 1280, 720, 30
    let d = 4.0<s>
    let bgc = gradientBg d Theme.royalBlue
    let stack =
        layer bgc
            [ graphic d (Components.banner { Text = "COMPONENT SHOWCASE"; Fill = Color.rgb 224uy 49uy 49uy; Accent = Theme.gold }) |> slideInFx "up" 0.5<s>
              graphic d (Components.bulletList [ "Big bold text"; "Info & score panels"; "Lower thirds"; "Captions & badges" ] Theme.gold)
              graphic d (Components.infoBox { Label = "SCORE"; Value = "42"; Fill = Color.rgb 33uy 102uy 224uy; Accent = Theme.gold; Radius = 26.0; BorderWidth = 8.0 })
              graphic d (Components.lowerThird { Name = "ANNOUNCER"; Title = "your host"; Accent = Theme.gold }) |> fadeIn 0.5<s>
              graphic d (Components.cornerBadge "LIVE" (Color.rgb 224uy 49uy 49uy))
              graphic d (Components.caption "…and a caption bar") |> fadeIn 0.8<s>
              graphic d (Components.progressBar 0.66 Theme.gold) ]
    renderToMp4 (cfg w h fps "components.mp4") stack

// ════════════════════ 7. SCENEGRAPH (hierarchical motion) ════════════════════
// Demonstrates the Scene module: composed objects whose transforms compose down the tree.
let scenegraph () =
    let w, h, fps = 1280, 720, 30
    let dur = 5.0<s>

    // title: rests near the top, flies down + fades in (entrance layered on its resting pos)
    let title =
        Scene.leaf (Components.centeredLabel "SCENEGRAPH" 96.0 Theme.gold)
        |> Scene.at 0.0 -250.0
        |> Scene.enterSlide 0.0 -180.0 0.6
        |> Scene.enterFade 0.6

    // a composite SCORE card: a GROUP that slides in from the right as ONE unit and gently
    // bobs; its children (bg, label, count-up, badge) ride along. The badge ALSO pops on its
    // own delay+pivot — so it inherits the card's slide AND adds its own motion.
    let cx, cy, cw, ch = 770.0, 240.0, 410.0, 250.0
    let bx, by = cx + cw - 30.0, cy + 16.0
    let card =
        Scene.group
            [ Scene.still (cardBg cx cy cw ch)
              Scene.leaf (fun _ _ -> [ textC (cx + cw / 2.0) (cy + 80.0) 40.0 Theme.white "SCORE" ])
              Scene.leaf (countUp (cx + cw / 2.0) (cy + ch - 55.0) 130.0 Theme.gold 2.5)
              Scene.leaf (fun _ _ -> badge bx by "x2" (Color.rgb 224uy 49uy 49uy))
              |> Scene.pivot bx by
              |> Scene.after 1.0<s>
              |> Scene.enterPop 0.5 ]
        |> Scene.at 0.0 0.0
        |> Scene.enterSlide (float w) 0.0 0.8
        |> Scene.bob 10.0 0.4
        |> Scene.after 0.4<s>

    let root = Scene.group [ title; card ]
    renderToMp4 (cfg w h fps "scenegraph.mp4")
        (layer (gradientBg dur Theme.royalBlue) [ Scene.clip dur root ])

// shared helpers for the polished demos
let private aaw (cmds: Cmd list) : Cmd list = match cmds with [] -> [] | _ -> [ AntiAlias(4, cmds) ]
// eased count-up: returns (formatted string, eased fraction 0..1) at local time t
let private countStr (target: float) (dur: float) (decimals: int) (t: float<s>) : string * float =
    let u = min 1.0 (max 0.0 (float t / dur))
    let e = let p = 1.0 - u in 1.0 - p * p * p
    let v = target * e
    let s =
        if decimals = 0 then string (int (round v))
        else v.ToString("F" + string decimals, System.Globalization.CultureInfo.InvariantCulture)
    s, e

// ════════════════════ WANDERLUST — cinematic travel (serif, gold, letterbox) ════════════════════
let wanderlust () =
    let w, h, fps = 1280, 720, 24
    let pal = Theme.midnightGold
    let serif = Theme.playfair ()
    let body = Theme.poppinsSB ()
    let bodyM = Theme.poppinsM ()
    let shadow = [ DropShadow(0.0, 6.0, 18, Color.rgba 0uy 0uy 0uy 210uy) ]

    // title open over darkened jellyfish
    let openDur = 4.4<s>
    let jelly0 = Video.loadClip (asset "jellyfish.mp4") 0.0 5.5 w h fps
    let titleNode =
        Scene.group
            [ Scene.leaf (fun fr _ -> aaw (Kit.trackedC bodyM 26.0 pal.Accent2 9.0 (fr.W / 2.0) (fr.H * 0.36) "A SHORT FILM"))
              |> Scene.enterFade 0.9 |> Scene.after 0.4<s>
              Scene.leaf (fun fr _ -> aaw [ Filtered(shadow, [ Kit.textC serif 150.0 pal.Ink (fr.W / 2.0) (fr.H * 0.52) "WANDERLUST" ]) ])
              |> Scene.enterSlide 0.0 46.0 1.0 |> Scene.enterFade 0.9 |> Scene.after 0.6<s>
              Scene.group
                  [ Scene.leaf (fun fr _ -> aaw [ Kit.line (fr.W / 2.0 - 130.0) (fr.H * 0.60) (fr.W / 2.0 + 130.0) (fr.H * 0.60) 2.0 pal.Accent ])
                    Scene.leaf (fun fr _ -> aaw (Kit.trackedC bodyM 23.0 pal.InkDim 7.0 (fr.W / 2.0) (fr.H * 0.66) "NATURE IN MOTION · VOL. 24")) ]
              |> Scene.enterFade 1.0 |> Scene.after 1.4<s> ]
    let openClip =
        layer (videoFor openDur jelly0 |> kenBurns |> tint Color.black 0.45 |> saturate 1.05)
              [ Scene.clip openDur titleNode ]

    // a chapter: footage + animated lower-third + coordinate tag
    let chapter (src: VideoSource) (name: string) (chap: string) (coord: string) : Clip =
        let d = Video.duration src
        let overlay =
            Scene.group
                [ Scene.leaf (fun fr _ -> aaw (Kit.lowerThird serif body pal 84.0 (fr.H - 208.0) name chap))
                  |> Scene.enterSlide -50.0 0.0 0.7 |> Scene.enterFade 0.6 |> Scene.after 0.5<s>
                  Scene.leaf (fun _ _ -> aaw (Kit.tag bodyM 84.0 120.0 coord pal.Accent pal.BgBot))
                  |> Scene.enterFade 0.6 |> Scene.after 0.9<s> ]
        layer (video src |> kenBurns |> saturate 1.12) [ Scene.clip d overlay ]

    let ch1 = chapter (Video.loadClip (asset "sintel.mp4") 1.0 5.0 w h fps)    "The Valley"   "CHAPTER ONE"   "46.5°N  7.9°E"
    let ch2 = chapter (Video.loadClip (asset "bunny.mp4")  2.0 5.0 w h fps)    "Meadowlands"  "CHAPTER TWO"   "47.4°N  8.5°E"
    let ch3 = chapter (Video.loadClip (asset "jellyfish.mp4") 4.5 5.0 w h fps) "The Deep"     "CHAPTER THREE" "00.0°  0.0°"

    let outroDur = 3.6<s>
    let outro =
        layer (solidBg outroDur pal.BgBot)
            [ Scene.clip outroDur
                (Scene.group
                    [ Scene.leaf (fun fr _ -> aaw [ Kit.textC serif 132.0 pal.Accent (fr.W / 2.0) (fr.H * 0.46) "FIN" ]) |> Scene.enterFade 1.0
                      Scene.leaf (fun fr _ -> aaw (Kit.trackedC bodyM 22.0 pal.InkDim 7.0 (fr.W / 2.0) (fr.H * 0.58) "FILMED ON LOCATION · MMXXIV"))
                      |> Scene.enterFade 1.0 |> Scene.after 0.7<s> ]) ]

    let film = chain (crossfade 0.8<s>) [ openClip; ch1; ch2; ch3 ]
    let withOutro = dipBlack 0.7<s> film outro
    let total = duration withOutro
    let furniture =
        graphic total (fun fr t ->
            Kit.letterbox fr 72.0
            @ [ Kit.vignette fr 0.45 ]
            @ aaw (Kit.timecode bodyM pal.InkDim fr fps (float t)
                   @ (if float t > 4.8 then Kit.kicker body 18.0 pal.Ink pal.Accent 72.0 102.0 "WANDERLUST" else [])))
    renderToMp4 (cfg w h fps "wanderlust.mp4") (layer withOutro [ furniture ])

// ════════════════════ MOMENTUM — synthwave telemetry HUD (over city footage) ════════════════════
let momentum () =
    let w, h, fps = 1280, 720, 24
    let pal = Theme.synthwave
    let display = Theme.anton ()
    let hud = Theme.bebas ()
    let bodyM = Theme.poppinsM ()
    let glow c cmds = [ Filtered([ DropShadow(0.0, 0.0, 16, c) ], cmds) ]

    // intro: neon title + brackets over darkened city
    let introDur = 3.6<s>
    let city0 = Video.loadClip (asset "city.mp4") 0.0 4.5 w h fps
    let neon (fr: Frame) =
        let size = 165.0
        let cx, by = fr.W / 2.0, Kit.midline display size (fr.H * 0.46)
        [ Kit.textC display size pal.Accent2 (cx + 4.0) (by + 4.0) "MOMENTUM" ]      // magenta offset
        @ glow pal.Accent [ Kit.textC display size pal.Ink cx by "MOMENTUM" ]         // cyan-glow white
    let introNode =
        Scene.group
            [ Scene.leaf (fun fr _ -> aaw (Kit.cornerBrackets fr 46.0 60.0 3.0 pal.Accent)) |> Scene.enterFade 0.5
              Scene.leaf (fun fr _ -> aaw (Kit.trackedC bodyM 24.0 pal.Accent 8.0 (fr.W / 2.0) (fr.H * 0.34) "SYSTEM ONLINE"))
              |> Scene.enterFade 0.7 |> Scene.after 0.3<s>
              Scene.leaf (fun fr _ -> aaw (neon fr)) |> Scene.enterSlide 0.0 30.0 0.7 |> Scene.enterFade 0.7 |> Scene.after 0.5<s>
              // underline wipes in
              Scene.leaf (fun fr t ->
                  let e = min 1.0 (float t / 0.6)
                  let half = 230.0 * (let p = 1.0 - e in 1.0 - p * p * p)
                  aaw [ Kit.line (fr.W / 2.0 - half) (fr.H * 0.58) (fr.W / 2.0 + half) (fr.H * 0.58) 3.0 pal.Accent2 ])
              |> Scene.after 1.1<s> ]
    let intro =
        layer (videoFor introDur city0 |> zoom 1.0 1.08 |> tint pal.BgBot 0.5 |> saturate 1.15)
              [ Scene.clip introDur introNode ]

    // stats: three frosted stat cards slide up, numbers count, over moving city
    let statsDur = 7.5<s>
    let city1 = Video.loadClip (asset "city.mp4") 3.0 (float statsDur) w h fps
    let cardW, cardH, gap = 360.0, 240.0, 30.0
    let totalW = cardW * 3.0 + gap * 2.0
    let x0 = (float w - totalW) / 2.0
    let cardY = 250.0
    let mkCard i (label: string) (target: float) (dec: int) (unit: string) =
        let cx = x0 + float i * (cardW + gap)
        Scene.leaf (fun _ t ->
            let v, e = countStr target 1.8 dec t
            aaw (Kit.statCard bodyM display pal cx cardY cardW cardH 8 label v unit (0.25 + 0.7 * e)))
        |> Scene.enterSlide 0.0 70.0 0.7 |> Scene.enterFade 0.6 |> Scene.after (0.3<s> + float i * 0.18<s>)
    let header (fr: Frame) =
        aaw ([ FillCircle(vec2 86.0 84.0, 7.0<px>, Color.rgb 255uy 64uy 168uy) ]
             @ Kit.kicker hud 30.0 pal.Ink pal.Accent 104.0 84.0 "LIVE TELEMETRY · GRID 07")
    let statsNode =
        Scene.group
            [ Scene.leaf (fun fr _ -> Kit.cornerBrackets fr 46.0 60.0 3.0 pal.Accent |> aaw) |> Scene.alpha 1.0
              Scene.leaf (fun fr _ -> header fr) |> Scene.enterFade 0.6
              mkCard 0 "VELOCITY" 312.0 0 "KM/H"
              mkCard 1 "OUTPUT"   98.6 1 "%"
              mkCard 2 "G-FORCE"  4.8  1 "G"
              Scene.leaf (fun fr t -> aaw (Kit.timecode bodyM pal.InkDim fr fps (float t)))
              Scene.leaf (fun fr t -> Kit.ticker fr hud bodyM pal 160.0 "LIVE" "TELEMETRY NOMINAL  ·  ALL SYSTEMS GREEN  ·  SECTOR 7 CLEAR  ·  THROUGHPUT 1.2 TB/S  ·  " (float t) |> aaw) ]
    let stats = layer (video city1 |> zoom 1.05 1.18 |> tint pal.BgBot 0.4 |> saturate 1.2) [ Scene.clip statsDur statsNode ]

    // hero stat: one huge number
    let heroDur = 3.0<s>
    let city2 = Video.loadClip (asset "city.mp4") 7.0 (float heroDur) w h fps
    let heroNode =
        Scene.group
            [ Scene.leaf (fun fr _ -> aaw (Kit.trackedC bodyM 26.0 pal.Accent 9.0 (fr.W / 2.0) (fr.H * 0.30) "PEAK PERFORMANCE INDEX"))
              |> Scene.enterFade 0.6
              Scene.leaf (fun fr t ->
                  let v, _ = countStr 99.9 1.6 1 t
                  aaw (glow pal.Accent [ Kit.textC display 230.0 pal.Ink (fr.W / 2.0) (Kit.midline display 230.0 (fr.H * 0.56)) v ]))
              |> Scene.enterFade 0.5 ]
    let hero = layer (videoFor heroDur city2 |> zoom 1.1 1.25 |> tint pal.BgBot 0.55 |> saturate 1.25) [ Scene.clip heroDur heroNode ]

    let outroDur = 2.6<s>
    let outro =
        layer (solidBg outroDur pal.BgBot)
            [ Scene.clip outroDur
                (Scene.group
                    [ Scene.leaf (fun fr _ -> aaw (glow pal.Accent [ Kit.textC display 120.0 pal.Ink (fr.W / 2.0) (Kit.midline display 120.0 (fr.H * 0.46)) "MOMENTUM" ])) |> Scene.enterFade 0.8
                      Scene.leaf (fun fr _ -> aaw (Kit.trackedC bodyM 22.0 pal.Accent2 8.0 (fr.W / 2.0) (fr.H * 0.58) "ENGINEERED FOR SPEED")) |> Scene.enterFade 0.9 |> Scene.after 0.5<s> ]) ]

    let body' = chain (cut) [ intro; stats; hero ]
    let full = dipBlack 0.5<s> body' outro
    renderToMp4 (cfg w h fps "momentum.mp4") full

// ════════════════════ SEASONS — warm editorial lookbook (side panel, serif) ════════════════════
let seasons () =
    let w, h, fps = 1280, 720, 24
    let pal = Theme.sunsetCoral
    let serif = Theme.playfair ()
    let bodyR = Theme.poppinsR ()
    let bodyM = Theme.poppinsM ()
    let display = Theme.archivo ()
    let titleShadow = [ DropShadow(0.0, 6.0, 16, Color.rgba 0uy 0uy 0uy 200uy) ]

    // intro: left-aligned lockup over warm footage
    let introDur = 3.8<s>
    let m0 = Video.loadClip (asset "bunny.mp4") 1.0 4.6 w h fps
    let introNode =
        Scene.group
            [ Scene.leaf (fun fr _ -> aaw (fst (Kit.trackedAt bodyM 24.0 pal.Accent 8.0 96.0 (fr.H * 0.40) "AN EDITORIAL")))
              |> Scene.enterFade 0.8 |> Scene.after 0.3<s>
              Scene.leaf (fun fr _ -> aaw [ Filtered(titleShadow, [ Kit.textL serif 150.0 pal.Ink 90.0 (fr.H * 0.60) "SEASONS" ]) ])
              |> Scene.enterSlide -44.0 0.0 0.9 |> Scene.enterFade 0.8 |> Scene.after 0.5<s>
              Scene.leaf (fun fr _ -> aaw (fst (Kit.trackedAt bodyM 22.0 pal.InkDim 6.0 96.0 (fr.H * 0.68) "A STUDY IN LIGHT · NO. 24")))
              |> Scene.enterFade 1.0 |> Scene.after 1.2<s> ]
    let intro = layer (videoFor introDur m0 |> kenBurns |> tint pal.BgBot 0.4 |> saturate 1.1) [ Scene.clip introDur introNode ]

    // an editorial entry: footage + frosted side panel with index, headline, copy, tag
    let entry (src: VideoSource) (idx: string) (head: string) (copy: string) (tagTxt: string) : Clip =
        let d = Video.duration src
        let x0, y0, pw, ph = 72.0, 128.0, 470.0, 474.0
        let panel (fr: Frame) =
            Kit.glassPanel x0 y0 pw ph 22.0 10 pal
            @ aaw
                ([ Kit.textL display 116.0 pal.Accent (x0 + 40.0) (y0 + 150.0) idx
                   Kit.textL serif 56.0 pal.Ink (x0 + 44.0) (y0 + 232.0) head
                   Kit.line (x0 + 44.0) (y0 + 258.0) (x0 + pw - 44.0) (y0 + 258.0) 1.5 (Color.rgba 255uy 255uy 255uy 60uy) ]
                 @ Kit.paragraph bodyR 24.0 pal.InkDim (x0 + 44.0) (y0 + 304.0) (pw - 88.0) 38.0 copy
                 @ Kit.tag bodyM (x0 + 44.0) (y0 + ph - 74.0) tagTxt pal.Accent pal.BgBot)
        layer (video src |> kenBurns |> saturate 1.15 |> tint pal.BgBot 0.12)
              [ Scene.clip d
                  (Scene.leaf (fun fr _ -> panel fr)
                   |> Scene.enterSlide -60.0 0.0 0.8 |> Scene.enterFade 0.6 |> Scene.after 0.4<s>) ]

    let e1 = entry (Video.loadClip (asset "sintel.mp4") 2.0 4.6 w h fps) "01" "The Valley"
                   "Carved by centuries of meltwater, the valley wakes slowly under a low amber sun." "WINTER LIGHT"
    let e2 = entry (Video.loadClip (asset "jellyfish.mp4") 0.5 4.6 w h fps) "02" "The Deep"
                   "Drifting far below the surface, these pelagic travellers pulse to a current of their own." "BLUE HOUR"
    let e3 = entry (Video.loadClip (asset "bunny.mp4") 3.0 4.6 w h fps) "03" "The Meadow"
                   "When the warmth returns the meadow answers in colour, unhurried and impossibly green." "FIRST BLOOM"

    let outroDur = 3.2<s>
    let outro =
        layer (solidBg outroDur pal.BgBot)
            [ Scene.clip outroDur
                (Scene.leaf (fun fr _ -> aaw (fst (Kit.trackedAt bodyM 26.0 pal.Accent 8.0 (fr.W / 2.0 - Kit.trackedWidth bodyM 26.0 8.0 "TO BE CONTINUED" / 2.0) (fr.H * 0.5) "TO BE CONTINUED")))
                 |> Scene.enterFade 1.2) ]

    let film = chain (crossfade 0.7<s>) [ intro; e1; e2; e3 ]
    let withOutro = dipBlack 0.6<s> film outro
    let total = duration withOutro
    let furniture = graphic total (fun fr _ -> Kit.letterbox fr 56.0 @ [ Kit.vignette fr 0.4 ])
    renderToMp4 (cfg w h fps "seasons.mp4") (layer withOutro [ furniture ])

// ════════════════════ STREAM — OBS / streaming-style facecam package ════════════════════
let stream () =
    let w, h, fps = 1280, 720, 24
    let pal = Theme.synthwave
    let hud = Theme.bebas ()
    let display = Theme.anton ()
    let nameF = Theme.poppinsB ()
    let bodyM = Theme.poppinsM ()
    let bodyR = Theme.poppinsR ()
    let bodySB = Theme.poppinsSB ()
    let glow c cmds = [ Filtered([ DropShadow(0.0, 0.0, 16, c) ], cmds) ]
    let alpha (c: Color) (a: float) = Color.rgba c.R c.G c.B (byte (max 0.0 (min 1.0 a) * 255.0))

    // cam decoded SQUARE so the cover-crop centres the face; content + standby bg full-frame
    let camSoon = Video.loadClip (asset "sintel.mp4") 0.0 4.2 440 440 fps
    let camLive = Video.loadClip (asset "sintel.mp4") 4.0 9.5 440 440 fps
    let camBrb  = Video.loadClip (asset "sintel.mp4") 1.0 4.0 440 440 fps
    let main    = Video.loadClip (asset "city.mp4")   2.0 9.5 w h fps
    let stbyBg  = Video.loadClip (asset "jellyfish.mp4") 0.0 4.4 w h fps

    // ── STANDBY (STARTING SOON / BE RIGHT BACK) ──
    let standby (dur: float<s>) (big: string) (sub: string) (cam: VideoSource) : Clip =
        let dots (fr: Frame) (t: float) =
            [ for i in 0 .. 2 ->
                let a = 0.3 + 0.7 * (0.5 + 0.5 * sin (t * 3.0 - float i * 0.9))
                FillCircle(vec2 (118.0 + float i * 34.0) (fr.H * 0.685) , 9.0<px>, alpha pal.Accent a) ]
        let node =
            Scene.group
                [ Scene.leaf (fun fr t -> aaw (Kit.facecam cam (fr.W * 0.74) (fr.H * 0.47) 158.0 pal.Accent pal.Accent2 t))
                  |> Scene.enterSlide 44.0 0.0 0.7 |> Scene.enterFade 0.6
                  Scene.leaf (fun fr _ -> aaw (Kit.kicker bodyM 24.0 pal.Accent2 pal.Accent 110.0 (fr.H * 0.30) "ALEX RIVERS  ·  LIVE STREAM"))
                  |> Scene.enterFade 0.7 |> Scene.after 0.2<s>
                  Scene.leaf (fun fr _ -> aaw (glow pal.Accent [ Kit.textL display 116.0 pal.Ink 104.0 (fr.H * 0.50) big ]))
                  |> Scene.enterSlide -40.0 0.0 0.8 |> Scene.enterFade 0.7 |> Scene.after 0.4<s>
                  Scene.leaf (fun fr _ -> aaw [ Kit.textL bodyR 26.0 pal.InkDim 110.0 (fr.H * 0.61) sub ])
                  |> Scene.enterFade 1.0 |> Scene.after 0.9<s>
                  Scene.leaf (fun fr t -> aaw (dots fr (float t))) |> Scene.enterFade 1.0 |> Scene.after 1.0<s>
                  Scene.leaf (fun fr _ -> aaw (fst (Kit.trackedAt bodyM 22.0 pal.InkDim 5.0 110.0 (fr.H - 64.0) "twitch.tv/alexrivers   ·   @alexrivers")))
                  |> Scene.enterFade 1.0 |> Scene.after 1.1<s> ]
        layer (videoFor dur stbyBg |> kenBurns |> blurFx 6 |> tint pal.BgBot 0.55 |> saturate 1.1)
              [ Scene.clip dur node ]

    // ── LIVE scene ──
    let liveDur = 8.6<s>
    let camCx, camCy, camR = float w - 178.0, float h - 232.0, 132.0
    let badgeW = Kit.trackedWidth hud 26.0 3.0 "LIVE" + 72.0
    let furniture =
        graphic liveDur (fun fr t ->
            let tt = float t
            // top-left title bar with live badge
            let title, cat = "EXPLORING THE OLD TOWN", "· JUST CHATTING"
            let bar =
                let bx, by, bh = 40.0, 36.0, 58.0
                let titleX = bx + 14.0 + badgeW + 24.0
                let bw = titleX + Kit.measure bodySB 28.0 title + 18.0 + Kit.measure bodyM 24.0 cat + 30.0 - bx
                [ Kit.shadowRR (rect bx by bw bh) 12.0 24
                  FillRoundedRect(rect bx by bw bh, 12.0<px>, pal.Panel)
                  StrokeRoundedRect(rect bx by bw bh, 12.0<px>, Kit.stroke 1.5 (Color.rgba 255uy 255uy 255uy 45uy)) ]
                @ Kit.liveBadge hud (bx + 14.0) (by + 6.0) tt
                @ [ Kit.textL bodySB 28.0 pal.Ink titleX (Kit.midline bodySB 28.0 (by + bh / 2.0)) title
                    Kit.textL bodyM 24.0 pal.Accent (titleX + Kit.measure bodySB 28.0 title + 18.0) (Kit.midline bodyM 24.0 (by + bh / 2.0)) cat ]
            // top-right viewer count + uptime
            let stats =
                let vtxt = "1,248  WATCHING"
                let vw = Kit.measure bodySB 26.0 vtxt
                let up = let s = 872 + int tt in sprintf "UPTIME  %02d:%02d:%02d" (s / 3600) (s / 60 % 60) (s % 60)
                [ FillCircle(vec2 (float w - 40.0 - vw - 22.0) 56.0, 7.0<px>, pal.Accent2)
                  Kit.textR bodySB 26.0 pal.Ink (float w - 40.0) (Kit.midline bodySB 26.0 56.0) vtxt
                  Kit.textR bodyM 22.0 pal.InkDim (float w - 40.0) 96.0 up ]
            // chat box (messages appear over time, newest at bottom)
            let chat =
                let cx0, cy0, cw, ch = 40.0, 322.0, 432.0, 326.0
                let msgs =
                    [ 0.3, "runner_42", "this view is unreal"
                      1.3, "mae_b", "hi alex!! first time live"
                      2.4, "k_dev", "what lens are you on?"
                      3.6, "zaraa", "the colours here wow"
                      5.0, "tomtom", "greetings from berlin o/"
                      6.3, "pixel", "followed! keep it up"
                      7.3, "nova_", "so chill, love this" ]
                let shown = msgs |> List.filter (fun (a, _, _) -> a <= tt)
                let vis = if List.length shown > 6 then List.skip (List.length shown - 6) shown else shown
                let line i (u: string) (m: string) =
                    let yy = cy0 + 84.0 + float i * 40.0
                    let uw = Kit.measure bodySB 21.0 (u + "  ")
                    [ Kit.textL bodySB 21.0 pal.Accent2 (cx0 + 26.0) yy u
                      Kit.textL bodyR 21.0 pal.Ink (cx0 + 26.0 + uw) yy m ]
                Kit.glassPanel cx0 cy0 cw ch 16.0 8 pal
                @ Kit.kicker hud 26.0 pal.Ink pal.Accent (cx0 + 24.0) (cy0 + 36.0) "STREAM CHAT"
                @ (vis |> List.mapi (fun i (_, u, m) -> line i u m) |> List.concat)
            bar @ stats @ chat
            @ Kit.facecam camLive camCx camCy camR pal.Accent pal.Accent2 t
            @ Kit.nameplate nameF bodyM camCx (camCy + camR + 16.0) "ALEX RIVERS" "@alexrivers" pal
            |> aaw)
    let alert =
        Scene.clip liveDur
            (Scene.leaf (fun fr _ ->
                let x, y, wd, hh = fr.W - 472.0, 124.0, 432.0, 94.0
                aaw (Kit.glassPanel x y wd hh 14.0 8 pal
                     @ [ FillRoundedRect(rect x y 8.0 hh, 4.0<px>, pal.Accent2) ]
                     @ fst (Kit.trackedAt bodyM 20.0 pal.Accent2 4.0 (x + 30.0) (y + 36.0) "NEW FOLLOWER")
                     @ [ Kit.textL nameF 36.0 pal.Ink (x + 30.0) (y + 74.0) "@pixelwytch" ]))
             |> Scene.after 4.0<s> |> Scene.lasting 3.2<s>
             |> Scene.enterSlide 480.0 0.0 0.55 |> Scene.enterFade 0.4)
    let live = layer (videoFor liveDur main |> tint pal.BgBot 0.18 |> saturate 1.12) [ furniture; alert ]

    let soon = standby 3.8<s> "STARTING SOON" "settling in — we go live in a moment" camSoon
    let brb  = standby 3.6<s> "BE RIGHT BACK" "grabbing a coffee, back in two" camBrb
    renderToMp4 (cfg w h fps "stream.mp4") (chain (crossfade 0.4<s>) [ soon; live; brb ])

// ════════════════════ SCORED — audio model check (clip sound + music bed) ════════════════════
let scored () =
    let w, h, fps = 960, 540, 30
    let city = Video.loadClip (asset "city.mp4") 0.0 6.0 w h fps
    let v =
        layer (video city |> kenBurns |> saturate 1.1 |> withSound city)
              [ graphic (Video.duration city) (Components.lowerThird { Name = "AUDIO TEST"; Title = "clip sound + music bed"; Accent = Theme.gold })
                |> fadeInOut 0.4<s> 0.4<s> ]
    let bed = music (asset "bed.m4a") (duration v) |> gain 0.5 |> afadeIn 1.0<s> |> afadeOut 1.0<s>
    renderToMp4 (cfg w h fps "scored.mp4") (layer v [ bed ])

// ════════════════════ COMPOSE — full compositionality (a sub-timeline moved as a unit) ════════════════════
let compose () =
    let w, h, fps = 1280, 720, 30
    // inner sub-timeline: talking head → slide → screencap, with INTERNAL cuts
    let th     = videoFor 2.5<s> (Video.loadClip (asset "sintel.mp4") 4.0 2.5 w h fps)
    let slide  = layer (solidBg 2.5<s> (Color.rgb 16uy 20uy 56uy))
                       [ graphic 2.5<s> (Components.titleCard
                            { Headline = "THE THESIS"; Subtitle = Some "one slide"; Size = 76.0; Fill = Theme.gold; Shadow = Theme.shadowStrong }) ]
    let screen = videoFor 3.0<s> (Video.loadClip (asset "city.mp4") 2.0 3.0 w h fps)
    let block  = chain cut [ th; slide; screen ]                                  // 8s, internal cuts
    // ONE voice track across the whole block — bound in, travels with it
    let segment = layer block [ music (asset "bed.m4a") (duration block) |> gain 0.6 ]

    // full-frame background to composite the segment over
    let bg = videoFor (duration segment) (Video.loadClip (asset "jellyfish.mp4") 0.0 8.0 w h fps)
             |> tint Color.black 0.45 |> blurFx 5 |> saturate 1.05

    // transform the ENTIRE segment (cuts + voice) as ONE unit: PiP, slide in, gently drift
    let pipBlock =
        segment
        |> pip 300.0 140.0 0.46                          // shrink + position lower-right
        |> enterFrom 760.0 0.0 0.8<s>                    // whole block slides in from the right
        |> animRot (fun t -> 0.015 * sin (t * 0.6))      // subtle drift — animated transform of the unit

    let label =
        graphic (duration segment) (Components.lowerThird
            { Name = "COMPOSITION"; Title = "one sub-timeline · moved as a unit"; Accent = Theme.gold }) |> fadeIn 0.5<s>

    renderToMp4 (cfg w h fps "compose.mp4") (layer bg [ pipBlock; label ])

// ════════════════════ CROP / MASK — clip-native PiP & facecam ════════════════════
let cropmask () =
    let w, h, fps = 1280, 720, 30
    let dur = 6.0<s>
    let bg = videoFor dur (Video.loadClip (asset "jellyfish.mp4") 0.0 6.0 w h fps) |> tint Color.black 0.4 |> blurFx 4

    // circular facecam, built purely from combinators (footage → centred circle mask → PiP)
    let facecam =
        videoFor dur (Video.loadClip (asset "sintel.mp4") 4.0 6.0 w h fps) |> saturate 1.05
        |> circleMask
        |> scale 0.34 |> place 360.0 150.0
        |> enterFrom 0.0 360.0 0.7<s>

    // punch-in + rounded PiP: crop a 16:9 region of the city, round its corners, place it
    let punch =
        videoFor dur (Video.loadClip (asset "city.mp4") 2.0 6.0 w h fps) |> saturate 1.15
        |> reframe (rect 520.0 240.0 711.0 400.0)
        |> maskRound (rect 0.0 0.0 (float w) (float h)) 48.0
        |> scale 0.4 |> place -360.0 -120.0
        |> enterFrom 0.0 -360.0 0.7<s>

    let label =
        graphic dur (Components.lowerThird { Name = "CROP · MASK"; Title = "clip-native PiP & facecam"; Accent = Theme.gold }) |> fadeIn 0.5<s>
    renderToMp4 (cfg w h fps "cropmask.mp4") (layer bg [ punch; facecam; label ])

// ════════════════════ TEMPLATES + DIFF — name+params widgets/clips, AST diff ════════════════════
let difftest () =
    Templates.install ()
    // a CLIP template registered with the DSL (params: text, color, bg)
    registerClipTemplate "titledCard" (fun ps ->
        let d = 3.0<s>
        layer (solidBg d (pcolor ps "bg" (Color.rgb 16uy 20uy 56uy)))
              [ comp "centeredLabel" [ "text", PStr(pstr ps "text" ""); "size", PFloat 96.0; "color", PColor(pcolor ps "color" Theme.gold) ] d ])

    // prove the templated path renders (Component + Inst resolve through the registries)
    let probe =
        layer (inst "titledCard" [ "text", PStr "TEMPLATED"; "color", PColor Theme.gold ])
              [ comp "lowerThird" [ "name", PStr "Inst + Component"; "title", PStr "registry-resolved · diffable"; "accent", PColor Theme.gold ] 3.0<s> ]
    renderToMp4 (cfg 1280 720 30 "templates.mp4") probe

    // a "video program" as data, then the same program after an edit
    let lower nm ti ac = comp "lowerThird" [ "name", PStr nm; "title", PStr ti; "accent", PColor ac ] 3.0<s>
    let before =
        concat [ inst "titledCard" [ "text", PStr "INTRO"; "color", PColor Theme.gold ]
                 lower "ALEX" "host" Theme.gold
                 lower "SAM" "guest" Theme.gold ]
    let after =
        concat [ inst "titledCard" [ "text", PStr "WELCOME"; "color", PColor Theme.gold ]   // param edit
                 lower "ALEX" "co-host" Theme.red                                            // 2 param edits
                 lower "SAM" "guest" Theme.gold
                 lower "JO" "guest" Theme.gold ]                                             // added clip
    printfn "\n=== AST diff (before → after) ==="
    match diff before after with
    | [] -> printfn "  (no changes)"
    | changes -> for c in changes do printfn "  %s" c
    printfn "=== end diff ===\n"

// ════════════════════ FONT SPECIMEN (verification) ════════════════════
let fonts () =
    let w, h = 1280, 720
    let bg = solidBg 4.0<s> (Color.rgb 14uy 16uy 24uy)
    renderToMp4 (cfg w h 30 "fonts.mp4")
        (layer bg [ graphic 4.0<s> (fun frame _ -> Kit.specimen frame) ])

let all : (string * (unit -> unit)) list =
    [ "transitions", transitions
      "effects", effects
      "nature", nature
      "top3", top3
      "quiz", quiz
      "components", components
      "scenegraph", scenegraph
      "fonts", fonts
      "wanderlust", wanderlust
      "momentum", momentum
      "seasons", seasons
      "stream", stream
      "scored", scored
      "compose", compose
      "cropmask", cropmask
      "difftest", difftest ]
