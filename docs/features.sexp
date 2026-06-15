; One involved "feature scene" template, instantiated 3× with different footage/text/accent.
; Editing the template once restyles all three — that's the point of templates.
(video (1280 720) :fps 30 :bg #05070e :out "out/sx_features.mp4"

  (assets
    (jelly "assets/jellyfish.mp4" :len 4.5s)
    (city  "assets/city.mp4"  :from 2s :len 4.5s)
    (bunny "assets/bunny.mp4" :from 1s :len 4.5s)
    (bed   "assets/bed.m4a"))

  ; footage (ken-burns + graded) under a slam-in title, a corner index badge, and a caption
  (template feature (src idx title kicker (accent #FFD200))
    (layer
      (-> (video $src :len 4.5s) (kenBurns) (tint :color #05070e :amount 0.42) (saturate :amount 1.12))
      (component titleCard :headline $title :subtitle $kicker :size 104 :fill #ffffff)
      (component cornerBadge :text $idx :fill $accent)
      (component caption :text "shot on location")))

  (timeline
    (layer
      (seq :gap (crossfade 0.7s)
        (layer (gradient 2.4s (#243082 #08081f)) (component centeredLabel :text "FEATURES" :size 120 :color #FFD200))
        (feature :src jelly :idx "01" :title "THE DEEP"   :kicker "OCEAN AMBIENCE")
        (feature :src city  :idx "02" :title "THE GRID"   :kicker "CITY MOTION"  :accent #00EAFF)
        (feature :src bunny :idx "03" :title "THE MEADOW" :kicker "OPEN WILD"    :accent #40E6AA)
        (layer (solidBg 2.2s #05070e) (component centeredLabel :text "FIN" :size 130 :color #FFD200)))
      (music bed :gain 0.4 :fadeIn 1s :fadeOut 1.5s))))
