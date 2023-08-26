(ns birdsong-as-code.song
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]))

(definst organ [freq 440 dur 1 volume 1.0 prev 220]
  (let [freq (line:kr (or prev freq) freq 0.06)]
    (-> (sin-osc freq)
        (+ (* 1/2 (sin-osc (* 2 freq))))
        (+ (* 1/6 (sin-osc (* 3 freq))))
        (+ (* 1/8 (sin-osc (* 4 freq))))
        (* (env-gen (adsr 0.3 0.2 0.1 0.05) (line:kr 1 0 dur) :action FREE))
        (* 1/4 volume))))

(defmethod live/play-note :default [{hertz :pitch seconds :duration previous :previous}]
  (organ hertz seconds (or previous hertz)))

(def melody
  (->>
    [8 9 11 16 13 14 12 16 12 11 17 15 14 16]
    (phrase [1/4 1/4 1/7 1/5 1/4 1/2 1 1/4 1/4 1/16 1/4 1/6 1/2 1/7 1/3])))

(def species-call
  (->> [14 18 16]
       (phrase [1/3 1/3 1])))

(defn absolute-harmonic-scale [root]
  (fn [pitch] (* root pitch)))

(defn join-up [[prev curr & notes]]
  (when curr
    (let [curr' (assoc curr :previous (:pitch prev))]
      (cons prev (join-up (cons curr' notes))))))

(def harmonic
  (let [root 110]
    (->>
      species-call
      (then melody)
      (where :pitch (absolute-harmonic-scale root))
      (all :previous (* 16 root))
      join-up
      (tempo (bpm 130)))))

(def diatonic
  (let [root 110]
    (->>
      species-call
      (then melody)
      (where :pitch (comp temperament/equal scale/A scale/major))
      (all :previous (* 16 root))
      join-up
      (tempo (bpm 130)))))

(comment
  ; Loop the track, allowing live editing.
  (live/play harmonic)
  (live/jam (var harmonic))
  (live/play harmonic)
  (live/jam (var diatonic))
  (live/play diatonic)
  (live/stop)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Birdsong is music  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Play some birdsong

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frequency is pitch ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(definst tone [frequency 440] (sin-osc frequency))

(definst beep [frequency 440 volume 1.0]
  (let [envelope (env-gen (perc 0.01 0.9) :action FREE)]
          (* envelope volume (sin-osc frequency))))

(comment
  (tone 300)
  (beep 300)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Harmonics          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; Interval
(comment
  (do
    (beep 300)
    (beep 500)))

; Tone with equally-loud harmonics
(comment
  (let [harmonics (range 1 6)
        freqs (map #(* % 100) harmonics) ]
    (map beep freqs)))

; Tone with diminishing harmonics
(comment
  (let [harmonics (range 1 6)
        freqs (fn [root] (map #(* % root) harmonics))
        volumes (map #(/ 3 %) harmonics)]
    (map beep (freqs 100) volumes)))

; Major triad
(comment
  (let [harmonics (range 1 6)
        freqs (fn [root] (map #(* % root) harmonics))
        volumes (map #(/ 3 %) harmonics)
        boop (fn [root] (map beep (freqs root) volumes))]
    (map boop [300 400 500])))

; Octave normalisation
(defn normalise [x]
  (if (< x 1) (normalise (* x 2))
    (if (< 2 x) (normalise (/ x 2))
      x)))

; Generate by multiplying 5-limit intervals
(comment
  (let [leaps (for [x (range 1 5)]
                (/ (inc x) x))]
    (->>
      (for [x leaps y leaps]
        (* x y))
        (map normalise)
        set
        sort)))

