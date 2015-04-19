(defpackage :gattling
  (:use :cl :xelf)
  (:export :gattling-main))

(in-package :gattling)

(let ((unit-size 16))
  (defun units (n)
    "Convert a magnitude into game units."
    (* unit-size n)))

(defparameter *width* 640)
(defparameter *height* 480)
(defparameter *rotation-radius* (units 6))

(defun center-game ()
  (values (/ *width* 2)
	  (/ *height* 2)))

(defun holding-right-arrow-p ()
  (or (keyboard-down-p :kp6)
      (keyboard-down-p :right)))

(defun holding-left-arrow-p ()
  (or (keyboard-down-p :kp4)
      (keyboard-down-p :left)))

(defun holding-up-arrow-p ()
  (or (keyboard-down-p :kp8)
      (keyboard-down-p :up)))

(defun holding-down-arrow-p ()
  (or (keyboard-down-p :kp2)
      (keyboard-down-p :down)))

(defun holding-fire-button-p ()
  (keyboard-down-p :z))

(defclass player-gun (node)
  ((width :initform (units 1))
   (height :initform (units 1))
   (angle :initform 0)
   (color :initform "red")
   (flipped :initform nil)
   (speed :initform 0.10)))

(defmethod rotate ((player-gun player-gun) delta)
  (with-slots (angle) player-gun
    (incf angle delta)
    (loop while (>= angle (* 2 pi))
       do  (decf angle (* 2 pi)))
    (loop while (< angle 0)
       do (incf angle (* 2 pi)))))

(defmethod rotate-counter ((player-gun player-gun) delta)
  (rotate player-gun (- delta)))
		     
      

(defun angle-top-p (angle)
  (<= 0 angle pi))

(defun angle-bottom-p (angle)
  (<= pi angle (* 2 pi)))

(defun angle-right-p (angle)
  (or (<= 0 angle (/ pi 2))
      (<= (* pi 3/2) angle (* 2 pi))))

(defun angle-left-p (angle)
  (<= (/ pi 2) angle (* pi 3/2)))

(defun firing-p ()
  (keyboard-pressed-p :z))

(defun flip-p ()
  (keyboard-pressed-p :x))

(defclass player-bullet (node)
  ((width :initform (units 1))
   (height :initform (units 1))
   (color :initform "orange")
   (angle :initarg :angle)
   (speed :initform 5)))

(defmethod update ((player-bullet player-bullet))
  (with-slots (angle speed) player-bullet
    (multiple-value-bind (x y) (location player-bullet)
      (move-to player-bullet
	       (+ x (* speed (cos angle)))
	       (+ y (* speed (sin angle)))))))

(defmethod update ((player-gun player-gun))
  (with-slots (flipped color) player-gun
    (when (flip-p)
      (setf flipped (not flipped)))
    (if flipped
	(setf color "purple")
	(setf color "red")))
  (with-slots (angle flipped) player-gun
    (cond ((firing-p)
	   (let ((new-bullet (make-instance 'player-bullet
					    :angle (if flipped
						       (+ angle pi)
						       angle))))
	     (insert new-bullet)
	     (multiple-value-bind (x y) (location player-gun)
	       (move-to new-bullet x y))))))
  (with-slots (angle) player-gun
    (move-to player-gun
	     (+ (/ *width* 2)
		(* *rotation-radius* (cos angle)))
	     (+ (/ *height* 2)
		(* *rotation-radius* (sin angle)))))
  (with-slots (angle speed) player-gun
    (flet ((rotate ()
	     (rotate player-gun speed))
	   (rotate-counter ()
	     (rotate-counter player-gun speed)))
      (cond ((holding-up-arrow-p)
	     (cond ((angle-left-p angle)
		    (rotate))
		   ((angle-right-p angle)
		    (rotate-counter))))
	    ((holding-down-arrow-p)
	     (cond ((angle-left-p angle)
		    (rotate-counter))
		   ((angle-right-p angle)
		    (rotate))))
	    ((holding-left-arrow-p)
	     (cond ((angle-top-p angle)
		    (rotate))
		   ((angle-bottom-p angle)
		    (rotate-counter))))
	    ((holding-right-arrow-p)
	     (cond ((angle-top-p angle)
		    (rotate-counter))
		   ((angle-bottom-p angle)
		    (rotate))))))))   

	 

(defclass planet (node)
  ((radius :initform (units 10))
   (color :initform "green")))

(defclass gattling (buffer)
  ((player-gun :initform (make-instance 'player-gun))
   (planet :initform (make-instance 'planet))
   (background-color :initform "white")
   (width :initform *width*)
   (height :initform *height*)))

(defmethod start-game ((gattling gattling))
  (with-slots (player-gun) gattling
    (with-buffer gattling
      (insert player-gun)
      (multiple-value-bind (center-x center-y) (center-game)
	(move-to player-gun center-x (- center-y *rotation-radius*))))))

(defun gattling-main ()
  (setf *screen-height* *height*
	*screen-width* *width*
	*resizable* t
	*scale-output-to-window* t)
  (with-session
    (open-project :gattling)
    (index-pending-resources)
    (let ((gattling (make-instance 'gattling)))
      (switch-to-buffer gattling)
      (start-game gattling))))
     

