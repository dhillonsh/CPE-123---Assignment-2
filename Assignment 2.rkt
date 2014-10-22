;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; volume changer next to each box
;; reverse track toggle
;; start//stop position changer for each track

(require rsound)
(require 2htdp/image)
(require 2htdp/universe)

(define SR 44100)
(define (s sec) (* SR sec))
(define (both a b) b)

; Sounds
(define sosa (rs-read/clip "sosa.wav" (s 0) (s 40)))
(define carnage (rs-read/clip "carnage.wav" (s 0) (s 40)))
(define tsunami (rs-read/clip "tsunami.wav" (s 0) (s 40)))
(define floss (rs-read/clip "floss.wav" (s 0) (s 40)))

;; Graphics

; The distance between the individual playback boxes
(define BOX-DIVISION 25)

; The height of the individual playback boxes
(define BOX-WIDTH 600)

; The width of the individual playback boxes
(define BOX-HEIGHT 100)

(define WORLD-WIDTH 1000)
(define WORLD-HEIGHT 700)
(define TRIANGLE-SIDE 100)
(define SLIDER-WIDTH (- BOX-WIDTH TRIANGLE-SIDE))

;; the "play" triangle
(define PLAY-IMG
  (rotate
   -90
   (triangle TRIANGLE-SIDE "solid" "pink")))

;; the "pause" rectangles
(define PAUSE-IMG
  (beside
   (rectangle (/ TRIANGLE-SIDE 4)
              TRIANGLE-SIDE
              "solid"
              "red")
   (rectangle (/ TRIANGLE-SIDE 8)
              1
              "solid"
              "white")
   (rectangle (/ TRIANGLE-SIDE 4)
              TRIANGLE-SIDE
              "solid"
              "red")))

;; ------------------


;; World structures
(define-struct world (play-head next-start playing? sound))
(define-struct container [world1 world2 world3 world4])

(define INITIAL-WORLD (make-container (make-world 0 (s 0.5) true sosa) (make-world 0 (s 0.5) true carnage) (make-world 0 (s 0.5) true tsunami) (make-world 0 (s 0.5) true floss)))

;; how much of the song to play each time?
(define PLAY-CHUNK (round (s 0.1)))
;; how far ahead of time should we queue sounds?
(define LEAD-FRAMES (round (s 0.05)))

;; the pstream that we're going to use:
(define ps (make-pstream))


;; given the current pstream time and the next
;; time to play, return true if it's time to play
;; frame frame -> boolean
(define (time-to-play? cur next)
  (< (- next cur) LEAD-FRAMES))


;; queue a sound if it's time, and advance the
;; world and the playhead
;; number world -> world
(define (playSong cur 1w)
  (local [(define next-start (world-next-start 1w))]
    (cond [(time-to-play? cur next-start)
           (local [(define playhead 
                      (cond
                        [(>= (world-play-head 1w) (s 40)) 0]
                        [else (world-play-head 1w)]
                      )
                     )
                   (define next-playhead 
                      (cond
                        [(> (+ playhead PLAY-CHUNK) (s 40)) (s 40)]
                        [else (+ playhead PLAY-CHUNK)]
                     )
                   )
                 ]
             
             (both (pstream-queue ps
                                  (clip (world-sound 1w) 
                                        playhead next-playhead
                                   )
                                  next-start)
                   
                   
                   (make-world next-playhead (+ next-start PLAY-CHUNK)
                               (world-playing? 1w) (world-sound 1w))
                   )
                   
                   )]
          [else (make-world 
                     (world-play-head 1w)
                     (world-next-start 1w)
                     (world-playing? 1w)
                     (world-sound 1w)
                    )
           ]
          )
    )
)


;; call checkPlayingWorlds if song is not paused
(define (checkPlayingWorlds cur w)
  (make-container 
    (cond [(world-playing? (container-world1 w)) (playSong cur (container-world1 w))]
        [else (container-world1 w)])
   (cond [(world-playing? (container-world2 w)) (playSong cur (container-world2 w))]
        [else (container-world2 w)])
   (cond [(world-playing? (container-world3 w)) (playSong cur (container-world3 w))]
        [else (container-world3 w)])
   (cond [(world-playing? (container-world4 w)) (playSong cur (container-world4 w))]
        [else (container-world4 w)])
   )
)


;; the on-tick function. calls maybe-play-chunk.
;; world -> world
(define (tock w)
  (checkPlayingWorlds (pstream-current-frame ps) w)
)


(define (drawScene worldnum 1w scene)
  (local
    [(define HEIGHT (+ (* worldnum 50) (* (- worldnum 1) 50) (* BOX-DIVISION worldnum)))]
    
  (draw-play worldnum 1w
             (place-image (rectangle 10 BOX-HEIGHT "solid" "black")
                (+ TRIANGLE-SIDE
                   (* SLIDER-WIDTH (/ (world-play-head 1w) (rs-frames (world-sound 1w)))))
                HEIGHT
                (place-image (rectangle BOX-WIDTH BOX-HEIGHT "solid" "purple") 300 HEIGHT scene)) 
     )
    )
)
;; draw a blank scene with a play head and a play/pause button
;; world -> scene
(define (draw-world w)
    ;(draw-play world1 (drawScene 1 world1))
    (drawScene 1 (container-world1 w) 
               (drawScene 2 (container-world2 w)
                                    (drawScene 3 (container-world3 w)
                                                        (drawScene 4 (container-world4 w) (empty-scene WORLD-WIDTH WORLD-HEIGHT)))))
    
)


;; draw the appropriate play/pause shape on a scene
;; world scene -> scene
(define (draw-play worldnum 1w scene)
  (local
    [(define HEIGHT (+ (/ BOX-HEIGHT 2) (* (- worldnum 1) 100) (* BOX-DIVISION worldnum)))]
    
  (cond [(world-playing? 1w)
         (place-image
          PLAY-IMG
          (/ TRIANGLE-SIDE 2)
          HEIGHT
          scene)]
        [else 
         (place-image
          PAUSE-IMG
          (/ TRIANGLE-SIDE 2)
          HEIGHT
          scene)]))
)
;; change the world when the mouse moves
;; world number number event frame -> world
(define (mouse-move 1w worldnum x y evt cur-time)
  (local
    [(define HEIGHT-MIN (+ (* (- worldnum 1) 100) (* BOX-DIVISION worldnum)))
     (define HEIGHT-MAX (+ (* (- worldnum 1) 100) 100 (* BOX-DIVISION worldnum)))
    ]
  (cond [(and (string=? evt "button-down") 
              (and (<= x BOX-WIDTH)
                   (and (>= y HEIGHT-MIN)
                   (<= y HEIGHT-MAX)
                 )     
              )
         )
         (make-world (world-play-head 1w)
                     (max (world-next-start 1w) cur-time)
                     (not (world-playing? 1w))
                     (world-sound 1w))
        ]
        
       [(string=? evt "move")
          (cond
            [(and (< x BOX-WIDTH) (and (<= x BOX-WIDTH)
                   (and (>= y HEIGHT-MIN)
                   (<= y HEIGHT-MAX)
                 )     
              )) 
             (make-world (round (* (rs-frames (world-sound 1w))
                               (/ (min 
                                   (max 0 (- x TRIANGLE-SIDE))
                                   SLIDER-WIDTH) 
                                  SLIDER-WIDTH)))
                         (max (world-next-start 1w) cur-time)
                         (world-playing? 1w)
                         (world-sound 1w)
                   )
           ]
           [else 1w]
           )
         ]
        ;; some other kind of event:
        [else 1w])
    )
)


;; deliver the current time to the mouse handler along
;; with its other arguments
;; -- this function exists to isolate the inner function
;; from the effects of a hidden input, the current time.
;; world number number event -> world
(define (mouse-move-wrapper w x y evt)
  (make-container
   (mouse-move (container-world1 w) 1 x y evt (pstream-current-frame ps))
   (mouse-move (container-world2 w) 2 x y evt (pstream-current-frame ps))
   (mouse-move (container-world3 w) 3 x y evt (pstream-current-frame ps))
   (mouse-move (container-world4 w) 4 x y evt (pstream-current-frame ps))
   )
)

(define (keyPress w k)
  (cond
    [(key=? k " ") INITIAL-WORLD]
    [else w]
    )
)

(big-bang INITIAL-WORLD
          [to-draw draw-world]
          [on-tick tock]
          [on-key keyPress]
          [on-mouse mouse-move-wrapper]
)