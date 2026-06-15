namespace VideoEdit

open Renderer

/// A small scenegraph designed for VIDEO, not UI. The differences that matter:
///   • no reactivity, no events, no per-frame state — a node is a pure function of time;
///   • transform is DECOMPOSED into animatable channels (x/y/scale/rotation/opacity), the
///     After-Effects model, rather than raw matrices — far nicer to author and keyframe;
///   • transforms COMPOSE down the tree via the renderer's `Group`, so moving a parent moves
///     its whole subtree, while each child may add its own motion on top.
/// A `Node` lowers to a `Clip` via `Scene.clip`, so it slots straight into the clip algebra
/// (concat / over / transitions all still apply on top of it).
///
/// Coordinate convention: leaf `Content` is the same `Frame -> localTime -> Cmd list` shape as
/// `Components`, i.e. it places itself in frame pixels. A node's x/y/scale/rotation are applied
/// ON TOP of that via `Group`. Scale/rotation pivot defaults to the frame centre; for an object
/// that lives off-centre (a corner badge), set an explicit `pivot` so it scales about itself.
module Scene =

    /// One animatable channel: local time (seconds) → value. `A.constant`/`A.tween` build these.
    type Channel = Anim<float>

    type Node =
        { X: Channel                                    // translate, px
          Y: Channel
          Scale: Channel                                // uniform
          Rotation: Channel                             // radians
          Opacity: Channel
          Pivot: (Frame -> float * float) option        // scale/rotate centre; None = frame centre
          Delay: float<s>                               // shifts this subtree's local clock (choreography)
          Life: float<s> option                         // visible window length after Delay; None = unbounded
          Content: (Frame -> float<s> -> Cmd list) option   // a leaf (Component, footage frame, raw cmds)
          Children: Node list }

    /// Identity node: no transform, no content, no children. Build everything from this.
    let empty : Node =
        { X = A.constant 0.0; Y = A.constant 0.0; Scale = A.constant 1.0
          Rotation = A.constant 0.0; Opacity = A.constant 1.0; Pivot = None
          Delay = 0.0<s>; Life = None; Content = None; Children = [] }

    // ── constructors ──
    let group (children: Node list) : Node = { empty with Children = children }
    let leaf (content: Frame -> float<s> -> Cmd list) : Node = { empty with Content = Some content }
    let still (cmds: Cmd list) : Node = { empty with Content = Some (fun _ _ -> cmds) }

    // ── transform setters (pipe-friendly; each replaces one channel) ──
    let x (a: Channel) (n: Node) = { n with X = a }
    let y (a: Channel) (n: Node) = { n with Y = a }
    let move (ax: Channel) (ay: Channel) (n: Node) = { n with X = ax; Y = ay }
    let at (px: float) (py: float) (n: Node) = { n with X = A.constant px; Y = A.constant py }
    let scale (a: Channel) (n: Node) = { n with Scale = a }
    let scaleTo (v: float) (n: Node) = { n with Scale = A.constant v }
    let rotate (a: Channel) (n: Node) = { n with Rotation = a }
    let opacity (a: Channel) (n: Node) = { n with Opacity = a }
    let alpha (v: float) (n: Node) = { n with Opacity = A.constant v }
    let pivot (px: float) (py: float) (n: Node) = { n with Pivot = Some(fun _ -> px, py) }
    let after (d: float<s>) (n: Node) = { n with Delay = d }
    let lasting (d: float<s>) (n: Node) = { n with Life = Some d }

    // ── additive layering: stack motion ON TOP of a node's resting position ──
    // (set the base with at/move FIRST, then layer entrances / continuous motion on top.)
    let private plus (a: Channel) (b: Channel) : Channel = fun t -> a t + b t
    let addX (a: Channel) (n: Node) = { n with X = plus n.X a }
    let addY (a: Channel) (n: Node) = { n with Y = plus n.Y a }
    /// Continuous vertical bob: amplitude px at the given frequency (Hz).
    let bob (amp: float) (freq: float) (n: Node) =
        addY (fun t -> amp * sin (t * freq * 2.0 * System.Math.PI)) n

    // ── entrance presets (pure A.tween channels; translations layer additively) ──
    /// Slide in from offset (dx,dy) → rest, over d seconds (easeOutCubic). Composes with `at`.
    let enterSlide (dx: float) (dy: float) (d: float) (n: Node) =
        n |> addX (A.tween dx 0.0 d Ease.easeOutCubic)
          |> addY (A.tween dy 0.0 d Ease.easeOutCubic)
    /// Pop in: scale 0.6→1 with overshoot + quick fade, over d seconds.
    let enterPop (d: float) (n: Node) =
        { n with Scale = A.tween 0.6 1.0 d (Ease.easeOutBack 2.2)
                 Opacity = A.tween 0.0 1.0 (d * 0.5) Ease.easeOutCubic }
    /// Fade in over d seconds.
    let enterFade (d: float) (n: Node) = { n with Opacity = A.tween 0.0 1.0 d Ease.easeOutCubic }

    /// Apply increasing delays across a list of nodes (sequential reveals).
    let stagger (gap: float<s>) (nodes: Node list) : Node list =
        nodes |> List.mapi (fun i n -> { n with Delay = n.Delay + float i * gap })

    // ── evaluation: fold the tree into a Cmd list at parent-local time ──
    let rec private evalNode (frame: Frame) (parentT: float<s>) (n: Node) : Cmd list =
        let t = parentT - n.Delay
        let visible = t >= 0.0<s> && (match n.Life with Some l -> t < l | None -> true)
        if not visible then [] else
        let own = match n.Content with Some f -> f frame t | None -> []
        let body = own @ (n.Children |> List.collect (evalNode frame t))
        if List.isEmpty body then [] else
        let tt = float t
        let s = n.Scale tt
        let px, py = match n.Pivot with Some f -> f frame | None -> frame.W / 2.0, frame.H / 2.0
        let core = aboutPivot px py (Matrix3x2.multiply (Matrix3x2.scale s s) (Matrix3x2.rotate (n.Rotation tt)))
        let m = Matrix3x2.multiply core (Matrix3x2.translate (n.X tt) (n.Y tt))
        [ Group(m, max 0.0 (min 1.0 (n.Opacity tt)), body) ]

    /// Lower a node tree into a clip of the given duration (plugs into the clip algebra).
    let clip (dur: float<s>) (root: Node) : Clip =
        Graphic(dur, fun frame t -> evalNode frame t root)
