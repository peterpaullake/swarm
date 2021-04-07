;;;; swarm.lisp

(in-package #:swarm)

(setf *js-string-delimiter* #\")

(when (boundp '*server*)
  (stop *server*))

(defparameter *js*
  (ps
    ;; MISC

    (defmacro push (x lst)
      "Extend LST by adding X to the end."
      `((chain ,lst push) ,x))

    (defun float->str (x &optional (n-digits 4))
      ((chain (-number x) to-fixed) n-digits))

    (defun gen-alphabet ()
      "Generate a list of letters from a to z."
      (map-index (lambda (i) ((chain -string from-char-code) (+ 97 i))) 26))

    (defun gen-rand-str (&optional (n-chars 8))
      "Generate a string made from random letters from the alphabet."
      (let ((alphabet (gen-alphabet))
	    (s ""))
	(dotimes (i n-chars s)
	  (incf s (elt alphabet (floor (* (random) n-chars)))))))

    (defmacro do-range ((var start end delta &optional result) &body body)
      "Evaluate BODY with VAR bound to numbers from
       START to END, incremented by DELTA. Similar
       to Python's range(start, stop, step)."
      `(do ((,var ,start (+ ,var ,delta)))
	   ((> ,var ,end) ,result)
	 ,@body))

    (defmacro let-xy (x y form &body body)
      (let ((form-sym (ps-gensym 'form)))
	`(let* ((,form-sym ,form)
		(,x (elt ,form-sym 0))
		(,y (elt ,form-sym 1)))
	   ,@body)))

    (defun map-index (fn count)
      (let ((lst (list)))
	(dotimes (i count lst)
	  (push (funcall fn i) lst))))

    (defun msg (&rest args)
      "Print ARGS to the console."
      (labels ((butlast (lst)
		 ((chain lst slice) 0 -1))
	       (last (lst)
		 (elt lst (1- (length lst)))))
	(let ((str ""))
	  (dolist (arg (butlast args))
	    (setf str (+ str arg " ")))
	  (setf str (+ str (last args)))
	  ((chain console log) str))))

    (defun get-time ()
      "Return the current time in milliseconds."
      ((chain performance now)))

    (defmacro with-timed-exec (&body body)
      "Return how long in milliseconds it takes to execute BODY."
      (let ((start-time (ps-gensym 'start-time)))
	`(let ((,start-time (get-time)))
	   ,@body
	   (msg "Execution time:"
		(- (get-time) ,start-time)
		"ms."))))

    ;; VECTOR

    (defmacro defun-vectorized (name op)
      "Vectorize a binary operation OP."
      (let ((v1 (ps-gensym 'v1))
	    (v2 (ps-gensym 'v2)))
	`(defun ,name (,v1 ,v2)
	   (mapcar ,op ,v1 ,v2))))

    (defun-vectorized vec+ (lambda (a b) (+ a b)))
    (defun-vectorized vec- (lambda (a b) (- a b)))
    (defun-vectorized vec* (lambda (a b) (* a b)))

    (defun scale-vec (a v)
      (mapcar (lambda (b) (* a b)) v))

    (defun sum (lst)
      (let ((s 0))
	(dolist (x lst s)
	  (incf s x))))

    (defmacro do-sum (var count &body body)
      (let ((s (ps-gensym 's))
	    (delta (ps-gensym 'delta)))
	`(let ((,s 0))
	   (dotimes (,var ,count ,s)
	     (let ((,delta (progn ,@body)))
	       (incf ,s ,delta))))))

    (defun vec-dot (v1 v2)
      (sum (mapcar (lambda (a b) (* a b)) v1 v2)))

    (defun vec-len2 (v)
      (vec-dot v v))

    (defmacro vec-incf (vec delta)
      `(setf ,vec (vec+ ,vec ,delta)))

    (defmacro do-vec-sum (var count &body body)
      (let ((s (ps-gensym 's))
	    (delta (ps-gensym 'delta)))
	`(let ((,s nil))
	   (when (= ,count 0)
	     (throw "Empty vector sum"))
	   (dotimes (,var ,count ,s)
	     (let ((,delta (progn ,@body)))
	       (if (eq ,s nil)
		   (setf ,s ,delta)
		   (vec-incf ,s ,delta)))))))

    ;; MODEL

    (defun compute-car-dxdts (coords)
      HERE)

    ;; CANVAS

    (defun fit-canvas-to-container (canvas &optional height)
      (setf (chain canvas style width) "100%")
      (setf (chain canvas width) (chain canvas offset-width))
      (if (eq height undefined)
	  (progn
	    (setf (chain canvas style height) "100%")
	    (setf (chain canvas height) (chain canvas offset-height)))
	  (setf (chain canvas height) height)))

    (defun color->str (color)
      "Convert a 3d color with components
       ranging from 0 to 1 to CSS rgb notation."
      (let ((comps (mapcar (lambda (x) ((chain -math round) (* 255 x)))
			   color)))
	(+ "rgb(" (elt comps 0) "," (elt comps 1) "," (elt comps 2) ")")))

    (defmacro canvas-cmd (cmd &rest args)
      `((chain ctx ,cmd) ,@args))

    (defun clear-canvas (ctx &optional (color '(0.1 0.1 0.1)))
      (setf (chain ctx fill-style) (color->str color))
      (canvas-cmd fill-rect 0 0 (chain ctx canvas width)
		  (chain ctx canvas height)))

    ;; USER-ADJUSTABLE PARAMETERS

    (defun extend-params-table (description type callback
				&optional value min max)
      "Add a user-adjustable parameter to the page."
      (let ((input ((chain document create-element) "input"))
	    (range-label ((chain document create-element) "p"))
	    (p ((chain document create-element)
		(if (eq type :link) "a" "p")))
	    (td-left ((chain document create-element) "td"))
	    (td-right ((chain document create-element) "td"))
	    (tr ((chain document create-element) "tr"))
	    (params-table ((chain document get-element-by-id) "params-table")))
	;; Set the input type
	(cond ((eq type :checkbox)
	       ((chain input set-attribute) "type" "checkbox"))
	      ((eq type :range)
	       ((chain input set-attribute) "type" "range")))
	;; Set the default value
	(cond ((eq type :checkbox)
	       (when value
		 ((chain input set-attribute) "checked" "")))
	      ((eq type :range)
	       ((chain input set-attribute) "value"
		(round (* 100 (/ (- value min) (- max min)))))))
	;; Set the callback
	(cond ((eq type :checkbox)
	       ((chain input add-event-listener) "change"
		(lambda (e)
		  (funcall callback (chain e target checked)))))
	      ((eq type :link)
	       ((chain p set-attribute) "href" "javascript:;")
	       ((chain p add-event-listener) "click"
		(lambda (e)
		  (funcall callback))))
	      ((eq type :range)
	       ((chain input add-event-listener) "change"
		(lambda (e)
		  (let* ((slider-value (chain e target value))
			 (value (+ min (* (/ slider-value 100) (- max min)))))
		    (setf (inner-html range-label) (float->str value))
		    (funcall callback value))))
	       ((chain input set-attribute) "step" 0.01)
	       ((chain range-label set-attribute) "class" "text zero-top-margin")
	       (setf (inner-html range-label) (float->str value))))
	((chain p set-attribute) "class" "text")
	(setf (inner-html p) description)
	;; Link the elements together
	(unless (eq type :link)
	  ((chain td-left append-child) input))
	;; For range type, add the label under the slider
	(when (eq type :range)
	  ((chain td-left append-child) range-label))
	((chain td-right append-child) p)
	((chain tr append-child) td-left)
	((chain tr append-child) td-right)
	((chain params-table append-child) tr)))

    (defmacro def-param ((name description type &key var value min max)
			 &body body)
      "Add a user-adjustable parameter to the page, and
       make it accessible by the rest of the program."
      (cond ((eq type :checkbox)
	     `(progn
		(defparameter ,name ,value)
		(extend-params-table ,description ,type (lambda (,var)
							  (setf ,name ,var)
							  ,@body)
				     ,value)))
	    ((eq type :link)
	     `(progn
		(extend-params-table ,description ,type (lambda () ,@body))))
	    ((eq type :range)
	     `(progn
		(defparameter ,name ,value)
		(extend-params-table ,description ,type (lambda (,var)
							  (setf ,name ,var)
							  ,@body)
				     ,value ,min ,max)))
	    (t `(throw (+ "User-adjustable parameter "
			  ',name
			  " has an unrecognized type: "
			  ',type)))))

    (def-param (*paused* "Paused" :checkbox :var value :value t))

    (def-param (*reset-sim* "Reset simulation" :link)
	(init-coords *game-state*))

    (def-param (*sim-speed* "Simulation speed" :range :var value
						      :value 0.001
						      :min 0.0001
						      :max 0.0012))

    (def-param (*n-cars* "N (number of cars)" :range :var value
						     :value 6
						     :min 1
						     :max 500)
	(init-coords *game-state*))
    
    ;; GAME

    (defun get-view-left (view-width view-xy)
      (- (elt view-xy 0) (/ view-width 2)))

    (defun get-view-bottom (view-height view-xy)
      (- (elt view-xy 1) (/ view-height 2)))

    (defmacro with-game-state (state &body body)
      `(let* ((state ,state)
	      (canvas (getprop state 'canvas))
	      (ctx ((chain canvas get-context) "2d"))
	      (view-width (getprop state 'view-width))
	      (view-xy (getprop state 'view-xy))
	      (canvas-aspect (/ (chain canvas width) (chain canvas height)))
	      (view-height (/ view-width canvas-aspect))
	      (view-left (get-view-left view-width view-xy))
	      (view-bottom (get-view-bottom view-height view-xy)))
	 ,@body))

    (defun draw-grid (state &optional (color '(0.05 0.05 0.05)) (width 0.2))
      (with-game-state state
	(labels ((draw-seg (x &optional (vertical? t))
		   (setf (chain ctx stroke-style) (color->str color))
		   (canvas-cmd begin-path)
		   (if vertical?
		       (progn (canvas-cmd move-to x 0)
			      (canvas-cmd line-to x (chain canvas height)))
		       (progn (canvas-cmd move-to 0 x)
			      (canvas-cmd line-to (chain canvas width) x)))
		   (canvas-cmd stroke)))
	  (let ((x0 (* (ceiling (/ view-left width)) width))
		(y0 (* (ceiling (/ view-bottom width)) width)))
	    (do ((x x0 (+ x width))
		 (y y0 (+ y width)))
		((and (> x (+ view-left view-width))
		      (> y (+ view-bottom view-height))))
	      (let-xy canvas-x canvas-y
		      (game-xy->canvas-xy state (list x y))
		      (draw-seg canvas-x)
		      (draw-seg canvas-y f)))))))

    (defun canvas-xy->game-xy (state xy)
      (with-game-state state
	(list (+ view-left
		 (* (/ (elt xy 0) (chain canvas width)) view-width))
	      (+ view-bottom
		 (* (/ (elt xy 1) (chain canvas height)) view-height)))))

    (defun game-xy->canvas-xy (state xy)
      (with-game-state state
	(list (* (/ (- (elt xy 0) view-left) view-width)
		 (chain canvas width))
	      (* (/ (- (elt xy 1) view-bottom) view-height)
		 (chain canvas height)))))

    (defun draw-circle (state xy &optional (color '(0.5 0.5 0.5)) (radius 3))
      (with-game-state state
	(let-xy x y (game-xy->canvas-xy state xy)
		(setf (chain ctx fill-style) (color->str color))
		(canvas-cmd begin-path)
		(canvas-cmd arc x y radius 0 (* 2 pi))
		(canvas-cmd fill))))

    (defun make-prey-coords (n)
      (let ((prey-coords (list)))
	(dotimes (i n prey-coords)
	  (push (list (random) (random))
		prey-coords))))

    (defun init-coords (state &optional (n-cars (round *n-cars*)))
      ;; Initialize the cars in a line with no speed.
      (setf (chain state coords) (list))
      (dotimes (i (* n-cars))
	(push (* 0.5 i) (chain state coords))
	(push 0 (chain state coords))))

    (defun init-game (state canvas &optional (height 300))
      (setf (chain state canvas) canvas)
      (fit-canvas-to-container canvas height)
      (setf (chain state view-width) 8)
      (setf (chain state view-xy) (list 0.5 0.5))
      (init-coords state)

      ((chain document add-event-listener) "keydown"
       (lambda (e)
	 (cond ((equal (chain e code) "KeyH")
		(msg "Left")
		(setf (chain *game-state* left?) t))
	       ((equal (chain e code) "KeyL")
		(msg "Right")
		(setf (chain *game-state* right?) t)))))

      ((chain canvas add-event-listener) "mousedown"
       (lambda (e)
	 (setf (getprop state 'last-click-pos)
	       (list (chain e client-x) (chain e client-y)))))

      ((chain canvas add-event-listener) "mouseleave"
       (lambda (e)
	 (delete (getprop state 'last-click-pos))))

      ((chain canvas add-event-listener) "mouseup"
       (lambda (e)
	 (delete (getprop state 'last-click-pos))))

      ((chain canvas add-event-listener) "mousemove"
       (lambda (e)
	 ;; Record the mouse position
	 (let* ((rect ((chain canvas get-bounding-client-rect)))
		(x (- (chain e client-x) (chain rect left)))
		(y (- (chain e client-y) (chain rect top))))
	   (setf (chain state mouse-xy) (list x y)))
	 ;; Adjust the view position
	 (unless (eq (getprop state 'last-click-pos) undefined)
	   (let-xy x y (getprop state 'last-click-pos)
		   (let ((delta-view-xy
			   (list (- x (chain e client-x))
				 (- y (chain e client-y)))))
		     (with-game-state state
		       (vec-incf (getprop state 'view-xy)
				 (vec* (list (/ view-width
						(getprop canvas 'width))
					     (/ view-height
						(getprop canvas 'height)))
				       delta-view-xy)))
		     (setf (getprop state 'last-click-pos)
			   (list (chain e client-x)
				 (chain e client-y))))))))

      ((chain canvas add-event-listener) "wheel"
       (lambda (e)
	 ((chain e prevent-default))
	 (if (< (chain state view-width) 0.01)
	     (setf (chain state view-width) 0.01)
	     (incf (chain state view-width) (* 0.01 (chain e delta-y)))))))

    (defun step-game (state dt)
      (unless *paused*
	(let ((speed *sim-speed*)
	      (coords (chain state coords)))
	  ;; Update the car positions and velocities
	  nil)))

    (defun draw-game (state)
      (with-game-state state
	(clear-canvas ctx)
	(draw-grid state)
	(let ((n-cars (/ (length (chain state coords)) 2)))
	  (dotimes (i n-cars)
	    (let ((pos (elt (chain state coords) (* 2 i))))
	      (draw-circle state (list pos 0)
			   (if (= i (1- n-cars)) '(1 0 0) '(0 1 0))))))))

    (defun run-game (state)
      (labels ((do-frame ()
		 (let ((dt (if (eq (chain state prev-time) undefined)
			       0
			       (- (get-time) (getprop state 'prev-time)))))
		   (setf (chain state prev-time) (get-time))
		   (step-game state dt)
		   (draw-game state)
		   ((chain window request-animation-frame) #'do-frame))))
	((chain window request-animation-frame) #'do-frame)))

    (defparameter *game-state* (create))
    (init-game *game-state* ((chain document get-element-by-id) "game-canvas"))
    (run-game *game-state*)

    ;; SLIDES

    (defun make-slide (title points)
      (let ((slide (create)))
	(setf (chain slide title) title)
	(setf (chain slide points) points)
	slide))

    (defun append-child (type classes body parent)
      (let ((child ((chain document create-element) type)))
	((chain child set-attribute) "class" classes)
	(setf (inner-html child) body)
	((chain parent append-child) child)
	child))

    (defun show-slide (i slides &optional (div-id "slides-div"))
      (let* ((slide (elt slides i))
	     (title (chain slide title))
	     (points (chain slide points))
	     (ul ((chain document create-element) "ul"))
	     (div ((chain document get-element-by-id) div-id)))
	;; Clear the slide
	(setf (inner-html div) "")
	;; Add the slide counter
	(append-child "p" "text small-font-size" (+ (1+ i) "/" (length slides))
		      div)
	;; Add the title
	(append-child "h1" "text" title div)
	;; Add the points
	(dolist (point points)
	  (append-child "p" "text large-font-size" point
			(append-child "li" "text" "" ul)))
	((chain div append-child) ul)
	;; Call mathjax to do math typesetting
	(unless (eq (typeof -math-jax) "undefined")
	  ((chain -math-jax typeset)))))

    (defparameter *slide-i* 0)
    (defparameter *slides* (list (make-slide "..." '("..."))))
    (show-slide *slide-i* *slides*)))

(defparameter *html*
  (with-html-output-to-string (s nil :indent t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:title "Predator-swarm")
      (str (uiop:read-file-string "fonts.html"))
      (:style (str (uiop:read-file-string "style.css"))))
     ;; (str (uiop:read-file-string "mathjax.html")))
     (:body :class "dark-bg"
	    (:div :class "row light-bg"
		  (:div :id "slides-div" :class "column")
		  (:div :id "media-div" :class "column"
			(:canvas :id "game-canvas")
			(:table :id "params-table"))))
     (:script :type "text/javascript" 
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :type "text/javascript" (str *js*)))))

(defun save-str-to-file (str path)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-sequence str s)))

;; Save the html to the disk
(save-str-to-file *html* "www/index.html")

(define-easy-handler (home :uri "/") () *html*)

(defparameter *server*
  (start (make-instance 'easy-acceptor
			:address "0.0.0.0" ;; localhost
			:document-root #p"www/"
			:port 8080)))

"
write code for the model that just steps
  the lead car according to the arrow keys
write code for the model (continue writing from HERE)
step the cars
"
