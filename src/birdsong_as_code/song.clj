(ns birdsong-as-code.song
  (:require [overtone.live :refer :all :exclude [stop]]
            [leipzig.melody :refer :all]
            [leipzig.scale :as scale]
            [leipzig.live :as live]
            [leipzig.live :refer [stop]]
            [leipzig.chord :as chord]
            [leipzig.temperament :as temperament]))

; Instruments
(definst bass [freq 110 dur 1.0 volume 1.0]
  (-> (saw freq)
      (* 1/8)
      (* (env-gen (adsr 0.5 0.3 0.8 0.05) (line:kr 1 0 dur) :action FREE))
      (* volume)))

(definst organ [freq 440 dur 1 volume 1.0]
  (-> (sin-osc freq)
      (+ (* 1/2 (sin-osc (* 2 freq))))
      (+ (* 1/6 (sin-osc (* 3 freq))))
      (+ (* 1/8 (sin-osc (* 4 freq))))
      (+ (* 1/18 (sin-osc (* 5 freq))))
      (* (env-gen (adsr 0.3 0.2 0.8 0.05) (line:kr 1 0 dur) :action FREE))
      (* 1/4 volume)))

(defmethod live/play-note :bass [{hertz :pitch seconds :duration}] (bass hertz seconds))
(defmethod live/play-note :default [{hertz :pitch seconds :duration}] (organ hertz seconds))

(def melody
  (->>
    [8 9 11 16 13 14 12 16 12 11 17 15 14]
    (phrase [1/4 1/4 1/7 1/5 1/4 1/2 1 1/4 1/4 1/16 1/4 1/6 1/2])))

(def harmonic
  (let [root 150]
    (->>
    melody
    (where :pitch (partial * root))
    (with (->> (phrase [6] [root]) (all :part :bass)))
    (tempo (bpm 130)))))

(def diatonic
  (let [root 150]
    (->>
    melody
    (where :pitch (comp temperament/equal scale/high scale/E scale/major))
    (with (->> (phrase [6] [root]) (all :part :bass)))
    (tempo (bpm 130)))))

(comment
  ; Loop the track, allowing live editing.
  (live/play harmonic-series)
  (live/jam (var harmonic))
  (live/jam (var diatonic))
  (live/stop)
)
