; A lower-thirds reel — templates + components + seq with crossfades. Vector only (no footage).
(video (1280 720) :fps 30 :bg #0a0e1c :out "out/sx_reel.mp4"

  (template card (label (color #FFD200))
    (layer
      (solidBg 2.5s #101438)
      (component centeredLabel :text $label :size 96 :color $color)))

  (template name3 (who role (accent #FFD200))
    (component lowerThird :dur 2.5s :name $who :title $role :accent $accent))

  (timeline
    (seq :gap (crossfade 0.5s)
      (inst card :label "S-EXPRESSIONS")
      (layer (solidBg 2.5s #16143a) (inst name3 :who "ALEX RIVERS" :role "host"))
      (layer (solidBg 2.5s #1a2e1a) (inst name3 :who "SAM CHEN" :role "guest" :accent #40E6AA))
      (inst card :label "THE END" :color #FF7A56))))
