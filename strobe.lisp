(require :asdf)
(require :cl-opengl)
(require :cl-glut)

(defclass strobe (glut:window)
  ()
  (:default-initargs :mode '(:double)))

(defvar white t)

(defun hsv-to-rgb (h s v)
  (let* ((h1 (/ h 60))
	 (c (* v s))
	 (x (* c (- 1 (abs (- (mod (/ h 60) 2) 1)))))
	 (m (- v c)))
    (multiple-value-bind (r g b)
	(case (floor h1)
	  (0 (values c x 0))
	  (1 (values x c 0))
	  (2 (values 0 c x))
	  (3 (values 0 x c))
	  (4 (values x 0 c))
	  (5 (values c 0 x)))
      (values (+ r m) (+ g m) (+ b m)))))

(defvar bright nil)

(defmethod glut:display ((win strobe))
  (setf bright (not bright))
  (if bright
      (multiple-value-call #'gl:color (hsv-to-rgb (random 360) 1 1))
      (gl:color 0 0 0))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:with-primitive :polygon
    (gl:vertex -1 -1)
    (gl:vertex 1 -1)
    (gl:vertex 1 1)
    (gl:vertex -1 1))
  (glut:swap-buffers))

(defmethod glut:idle ((win strobe))
  (glut:post-redisplay))

;(glut:display-window (make-instance 'strobe))

