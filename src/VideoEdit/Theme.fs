namespace VideoEdit

open Renderer

/// A loud, high-contrast "Japanese game show" palette + bold-font loading.
module Theme =

    let gold         = Color.rgb 255uy 210uy 0uy
    let goldLight    = Color.rgb 255uy 235uy 120uy
    let red          = Color.rgb 255uy 56uy 56uy
    let blue         = Color.rgb 30uy 40uy 120uy
    let bg           = Color.rgb 16uy 20uy 56uy
    let white        = Color.rgb 255uy 255uy 255uy
    let cyan         = Color.rgb 0uy 200uy 255uy
    let shadow       = Color.rgba 0uy 0uy 0uy 140uy
    let shadowStrong = Color.rgba 0uy 0uy 0uy 200uy

    /// Background gradient stops, top → bottom.
    let royalBlue : GradientStop list =
        [ { Offset = 0.0; StopColor = Color.rgb 36uy 48uy 130uy }
          { Offset = 1.0; StopColor = Color.rgb 8uy 10uy 36uy } ]

    // ── font cache + named families (downloaded OFL fonts live in ./fonts) ──
    let private fontCache = System.Collections.Generic.Dictionary<string, Text.FontData>()

    /// Load (and cache) a font by path. Safe to call per-frame.
    let load (path: string) : Text.FontData =
        match fontCache.TryGetValue path with
        | true, f -> f
        | _ ->
            let f = Text.loadFontFile path
            fontCache.[path] <- f
            f

    let private fdir = "fonts/"
    let anton ()     = load (fdir + "Anton-Regular.ttf")            // heavy condensed display
    let archivo ()   = load (fdir + "ArchivoBlack-Regular.ttf")     // ultra-bold geometric
    let bebas ()     = load (fdir + "BebasNeue-Regular.ttf")        // tall condensed (HUD/tickers)
    let poppinsB ()  = load (fdir + "Poppins-Bold.ttf")
    let poppinsSB () = load (fdir + "Poppins-SemiBold.ttf")
    let poppinsM ()  = load (fdir + "Poppins-Medium.ttf")
    let poppinsR ()  = load (fdir + "Poppins-Regular.ttf")
    let crimsonB ()  = load (fdir + "CrimsonText-Bold.ttf")         // elegant serif
    let crimsonSB () = load (fdir + "CrimsonText-SemiBold.ttf")
    let playfair ()  = load (fdir + "PlayfairDisplay-Var.ttf")      // variable serif (default master)

    let mutable private boldFont : Text.FontData option = None

    /// Default heavy display font used by the original Components (now Poppins Bold).
    let defaultBoldFontPath = fdir + "Poppins-Bold.ttf"

    /// Choose the bold/heavy font used by all big-text components. Call once at startup.
    let useBoldFont (path: string) = boldFont <- Some (load path)

    let font () : Text.FontData =
        match boldFont with
        | Some f -> f
        | None ->
            let f = load defaultBoldFontPath
            boldFont <- Some f
            f

    // ── curated palettes (graphics over footage: gradients for scrims/cards, accents for ink) ──
    type Palette =
        { BgTop: Color; BgBot: Color            // vertical gradient for scrims/cards
          Ink: Color; InkDim: Color             // primary / secondary text
          Accent: Color; Accent2: Color         // stripes, strokes, highlights
          Panel: Color }                        // translucent panel base

    let bgStops (p: Palette) : GradientStop list =
        [ { Offset = 0.0; StopColor = p.BgTop }; { Offset = 1.0; StopColor = p.BgBot } ]

    /// deep navy → black, warm gold — cinematic / premium
    let midnightGold =
        { BgTop = Color.rgb 18uy 24uy 46uy;  BgBot = Color.rgb 4uy 6uy 14uy
          Ink = Color.rgb 245uy 246uy 250uy;  InkDim = Color.rgb 168uy 178uy 200uy
          Accent = Color.rgb 232uy 191uy 102uy; Accent2 = Color.rgb 255uy 226uy 170uy
          Panel = Color.rgba 10uy 14uy 28uy 200uy }

    /// purple/indigo with cyan + magenta — synthwave HUD
    let synthwave =
        { BgTop = Color.rgb 38uy 18uy 64uy;  BgBot = Color.rgb 10uy 8uy 30uy
          Ink = Color.rgb 245uy 244uy 255uy;  InkDim = Color.rgb 178uy 168uy 214uy
          Accent = Color.rgb 0uy 234uy 255uy;  Accent2 = Color.rgb 255uy 64uy 168uy
          Panel = Color.rgba 16uy 10uy 34uy 205uy }

    /// warm plum → ember — sunset / food / travel
    let sunsetCoral =
        { BgTop = Color.rgb 60uy 22uy 40uy;  BgBot = Color.rgb 14uy 8uy 16uy
          Ink = Color.rgb 255uy 248uy 240uy;  InkDim = Color.rgb 226uy 188uy 178uy
          Accent = Color.rgb 255uy 122uy 86uy; Accent2 = Color.rgb 255uy 196uy 120uy
          Panel = Color.rgba 28uy 12uy 18uy 200uy }

    /// dark teal → black, emerald + mint — fresh / outdoors
    let emeraldNoir =
        { BgTop = Color.rgb 12uy 40uy 40uy;  BgBot = Color.rgb 4uy 12uy 12uy
          Ink = Color.rgb 240uy 252uy 248uy;  InkDim = Color.rgb 156uy 200uy 188uy
          Accent = Color.rgb 64uy 230uy 170uy; Accent2 = Color.rgb 180uy 255uy 220uy
          Panel = Color.rgba 6uy 20uy 18uy 205uy }
