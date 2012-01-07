(ns yardbird.sounds
  (:use [overtone.core]))

(definst clarinet [note 60 amp 1.3 dur 0.5]
  (let [freq (midicps note)
        src (saw [freq (* freq 1.01) (* 0.99 freq)])
        low (sin-osc (/ freq 2))
        filt (lpf src (line:kr (* 10 freq) freq 10))
        env (env-gen (perc 0.1 dur) :action FREE)]
    (* amp low env filt)))

(definst beep [note 60 amp 0.5 dur 0.5]
  (let [freq (midicps note)
        src (sin-osc freq)
        env (env-gen (perc dur 2) :action FREE)]
    (* amp src env)))

(definst overpad [note 60 amp 0.7 attack 0.001 release 2]
  (let [freq  (midicps note)
        env   (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                     (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)]))
                             (lpf (saw [freq (* freq 1.01)]) f-env)))
        audio (* amp env sig)]
    audio))
;(overpad :note 66 :attack 2 :release 2)

(definst kick [amp 1.0]
  (let [src (sin-osc 80)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* amp src env)))
;(kick :amp 2.5)

(definst pedestrian-crossing
  "Street crossing in Britain."
  []
  (* 0.2 (sin-osc 2500) (lf-pulse 5)))
;(pedestrian-crossing)

(definst trancy-waves 
  "" 
  [freq 200 amp 0.2]
  (* amp
     (+ (sin-osc freq) 
        (saw freq) 
        (saw (+ freq 3)) 
        (sin-osc (* 2 freq)))))
;(trancy-waves :freq 400 :amp 0.5)
;(stop)
  
(defsynth scratch-pendulum 
  []
  (let [kon (sin-osc:kr (* 10 (mouse-x)))
        k2 (sin-osc:kr (* 5 (mouse-x)))
        lpk (lin-lin:kr kon -1 1 0 1000)
        foo (poll:kr (impulse:kr 20) lpk)
        src (lpf (white-noise) lpk)
        src (pan2 src k2)
        bak (* 0.5 (lpf (white-noise 500)))]
    (out 0 (+ src [bak bak]))))

;(scratch-pendulum)

(definst sizzle [bus 0 amp 0.4 depth 10 freq 220 lfo 8]
  (out bus (* amp (saw (+ freq (* depth (sin-osc:kr lfo)))))))

;(beep)

;(mine)
;(stop)

