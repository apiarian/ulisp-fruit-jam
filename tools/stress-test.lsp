;;; PSRAM Stress Test for uLisp Fruit Jam
;;;
;;; Exercises PSRAM data integrity alongside all hardware subsystems:
;;; graphics (HSTX DMA), audio (I2S DMA), keyboard, mouse, and GC.
;;; Runs continuously until Escape is pressed.
;;; Returns the number of data integrity errors detected (0 = pass).
;;;
;;; Usage:
;;;   (stress-test)       ; run until Escape
;;;   (stress-test 100)   ; run 100 rounds then stop
;;;
;;; Load via: (package-load "stress-test.lsp")
;;; Or paste individual functions at the REPL.
;;;
;;; Array sizes can be adjusted — larger arrays stress PSRAM bandwidth
;;; more but take longer per round (~7s at 5K/5K/1K).

;; Helper: fill array with a deterministic pattern based on index and seed
(defun stress-fill (arr n seed)
  (dotimes (i n)
    (setf (aref arr i) (logand (+ (* i 31) seed) #xFFFF))))

;; Helper: verify array pattern, return number of mismatches
(defun stress-check (arr n seed)
  (let ((errs 0))
    (dotimes (i n)
      (unless (= (aref arr i) (logand (+ (* i 31) seed) #xFFFF))
        (incf errs)))
    errs))

;; Helper: draw a progress bar
(defun stress-bar (x y w h val maxv col)
  (fill-rect x y w h 0)
  (draw-rect x y w h 64)
  (let ((fw (truncate (* w val) maxv)))
    (when (> fw 0)
      (fill-rect (+ x 1) (+ y 1) (min fw (- w 2)) (- h 2) col))))

;; Helper: draw status text at position
(defun stress-text (x y msg col)
  (set-cursor x y)
  (set-text-color col 0)
  (with-gfx (s) (princ msg s)))

;; Helper: log a message in the scrolling activity area
(defun stress-log (msg col)
  (when (> *log-y* 270)
    (fill-rect 0 238 399 42 0)
    (setq *log-y* 240))
  (set-cursor 8 *log-y*)
  (set-text-color col 0)
  (with-gfx (s) (princ msg s))
  (setq *log-y* (+ *log-y* 10)))

;; Main stress test
(defun stress-test (&optional (rounds 0))
  (let ((arr1 (make-array 5000))
        (arr2 (make-array 5000))
        (arr3 (make-array 1000))
        (errors 0)
        (round 0)
        (max-rounds rounds)
        (gc-count 0)
        (start-time (millis))
        (last-gc-time 0))
    (graphics-mode)
    (fill-screen 0)
    (mouse-show)
    ;; Title
    (set-text-size 2)
    (stress-text 40 8 "PSRAM Stress Test" 223)
    (set-text-size 1)
    (draw-line 0 30 399 30 64)
    ;; Labels
    (stress-text 8 40 "Round:" 150)
    (stress-text 8 55 "Errors:" 150)
    (stress-text 8 70 "GC runs:" 150)
    (stress-text 8 85 "Elapsed:" 150)
    (stress-text 8 105 "Array 1 (5K):" 150)
    (stress-text 8 120 "Array 2 (5K):" 150)
    (stress-text 8 135 "Array 3 (1K):" 150)
    (stress-text 8 155 "GC pressure:" 150)
    (stress-text 8 175 "Audio:" 150)
    (stress-text 8 195 "Graphics:" 150)
    (draw-line 0 220 399 220 64)
    (stress-text 8 228 "Activity log:" 150)
    (draw-line 0 280 399 280 64)
    (stress-text 8 288 "Esc=quit  Mouse+KB active" 150)
    ;; Setup audio + LED
    (audio-wave 0 1) (audio-vol 0 60)
    (audio-wave 1 2) (audio-vol 1 40)
    (pinmode 29 :output)
    (keyboard-flush)
    ;; Log position tracking
    (defvar *log-y* 240)
    (setq *log-y* 240)
    ;; Main loop
    (loop
      (incf round)
      ;; Blink LED + beep so we know the loop is running
      (digitalwrite 29 (if (oddp round) nil t))
      (audio-note 0 (+ 60 (mod round 12)) 100)
      ;; Update round display
      (fill-rect 80 40 100 8 0)
      (stress-text 80 40 round 223)
      ;; Phase 1: Fill arrays with different seeds
      (stress-log (format nil "R~a: fill" round) 28)
      (stress-fill arr1 5000 (* round 7))
      (stress-fill arr2 5000 (* round 13))
      (stress-fill arr3 1000 (* round 29))
      (stress-bar 130 103 200 12 1 3 28)
      ;; Phase 2: GC pressure — create and discard lots of garbage
      (stress-log "gc-pressure" 220)
      (let ((gc-start (millis)))
        (dotimes (i 500)
          (list i (* i i) (+ i 1) (logand i 255)))
        (incf gc-count)
        (setq last-gc-time (- (millis) gc-start)))
      (fill-rect 80 70 100 8 0)
      (stress-text 80 70 gc-count 223)
      (stress-bar 130 153 200 12 (min last-gc-time 500) 500 220)
      ;; Phase 3: Verify arrays — the critical PSRAM integrity check
      (stress-log "verify" 3)
      (let ((e1 (stress-check arr1 5000 (* round 7)))
            (e2 (stress-check arr2 5000 (* round 13)))
            (e3 (stress-check arr3 1000 (* round 29))))
        (stress-bar 130 103 200 12 2 3 28)
        (stress-bar 130 118 200 12 2 3 28)
        (stress-bar 130 133 200 12 2 3 28)
        (let ((round-errs (+ e1 e2 e3)))
          (incf errors round-errs)
          (fill-rect 80 55 150 8 0)
          (stress-text 80 55 errors (if (= errors 0) 28 192))))
      ;; Phase 4: Cross-verify — write arr3 while reading arr1/arr2
      (stress-fill arr3 1000 (+ (* round 29) 1))
      (let ((e1 (stress-check arr1 5000 (* round 7)))
            (e2 (stress-check arr2 5000 (* round 13)))
            (e3 (stress-check arr3 1000 (+ (* round 29) 1))))
        (incf errors (+ e1 e2 e3))
        (stress-bar 130 103 200 12 3 3 28)
        (stress-bar 130 118 200 12 3 3 28)
        (stress-bar 130 133 200 12 3 3 28))
      ;; Phase 5: Audio — play a tone (DMA + I2S concurrent with PSRAM)
      (audio-note 0 (+ 48 (mod round 36)) 200)
      (stress-bar 130 173 200 12 1 2 195)
      ;; Phase 6: Graphics stress — draw random shapes (HSTX DMA concurrent)
      (let ((gx (+ 200 (mod (* round 17) 180)))
            (gy (+ 105 (mod (* round 23) 100)))
            (gr (+ 3 (mod round 15)))
            (gc (mod (* round 37) 256)))
        (fill-circle gx gy gr gc))
      (stress-bar 130 193 200 12 (mod round 20) 20 3)
      ;; Phase 7: Verify after graphics+audio DMA activity
      (let ((e1 (stress-check arr1 5000 (* round 7)))
            (e2 (stress-check arr2 5000 (* round 13))))
        (let ((late-errs (+ e1 e2)))
          (incf errors late-errs)
          (when (> late-errs 0)
            (stress-log (format nil "!!! ~a errors in round ~a" late-errs round) 192))))
      ;; Update elapsed time
      (fill-rect 80 85 150 8 0)
      (stress-text 80 85
        (format nil "~as" (truncate (- (millis) start-time) 1000)) 223)
      ;; Update error display color
      (fill-rect 80 55 150 8 0)
      (stress-text 80 55
        (if (= errors 0) "0 - CLEAN" (format nil "~a - FAILURES" errors))
        (if (= errors 0) 28 192))
      ;; Check keyboard for Escape
      (let ((k (keyboard)))
        (when (and k (= (key-code k) 27))
          (digitalwrite 29 t)
          (mouse-hide)
          (audio-stop-all)
          (text-mode)
          (format t "~%Stress test complete: ~a rounds, ~a errors, ~as~%"
            round errors (truncate (- (millis) start-time) 1000))
          (return errors)))
      ;; Check for max rounds
      (when (and (> max-rounds 0) (>= round max-rounds))
        (digitalwrite 29 t)
        (mouse-hide)
        (audio-stop-all)
        (text-mode)
        (format t "~%Stress test complete: ~a rounds, ~a errors, ~as~%"
          round errors (truncate (- (millis) start-time) 1000))
        (return errors))
      ;; Brief delay to keep display responsive
      (delay 10))))
