;;;; swarm.lisp

(in-package #:swarm)

(setf *js-string-delimiter* #\")

(when (boundp '*server*)
  (stop *server*))

(defparameter *js*
  (ps
    ;; MISC

    (defmacro push (x lst)
      "Extend LST by the element X."
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

    (defun compute-pred-dxdt (pred-xy prey-coords &optional (c *c*) (p *p*))
      (let ((n (length prey-coords))
	    (z pred-xy))
	(scale-vec (/ c n)
		   (do-vec-sum k n
		     (let ((xk (elt prey-coords k)))
		       (scale-vec (/ 1 (expt (vec-len2 (vec- xk z)) (/ p 2)))
				  (vec- xk z)))))))

    (defun compute-prey-dxdts (pred-xy prey-coords &optional (a *a*) (b *b*))
      (let ((n (length prey-coords)))
	(labels ((compute-prey-dxdt (j)
		   (let ((z pred-xy)
			 (xj (elt prey-coords j)))
		     (vec+ (scale-vec (/ 1 n)
				      (do-vec-sum k n
					(if (= k j)
					    '(0 0)
					    (let ((xk (elt prey-coords k)))
					      (vec- (scale-vec (/ 1 (vec-len2
								     (vec- xj
									   xk)))
							       (vec- xj xk))
						    (scale-vec a
							       (vec- xj xk)))))))
			   (scale-vec (/ b (vec-len2 (vec- xj z)))
				      (vec- xj z))))))
	  (map-index #'compute-prey-dxdt (length prey-coords)))))

    ;; CANVAS

    (defun color->str (color)
      "Convert a 3d color with components
       ranging from 0-1 to CSS rgb notation."
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

    (defun extend-params-table (description type callback-name
				&optional value min max)
      "Add a user-adjustable parameter to the page."
      (let ((input ((chain document create-element) "input"))
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
	;; If the type is range, generate an id for its indicator
	(when (eq type :range)
	  (setf label-id (gen-rand-str)))
	;; Set the callback
	(cond ((eq type :checkbox)
	       ((chain input set-attribute) "onchange"
		(+ callback-name "(this.checked)")))
	      ((eq type :link)
	       ((chain p set-attribute) "href" "javascript:;")
	       ((chain p set-attribute) "onclick"
		(+ callback-name "()")))
	      ((eq type :range)
	       ((chain input set-attribute) "onchange"
		(+ "x = " min " + " "this.value / 100 * " (- max min) "; "
		   "document.getElementById(\""
		   label-id
		   "\").innerHTML = "
		   (lisp (ps (float->str x))) "; "
		   callback-name
		   "(x);"))
	       ((chain input set-attribute) "step" 0.01)))
	((chain p set-attribute) "class" "text")
	(setf (inner-html p) description)
	;; Link the elements together
	(unless (eq type :link)
	  ((chain td-left append-child) input))
	;; For range type, add an indicator under the slider
	(when (eq type :range)
	  (let ((label ((chain document create-element) "p")))
	    ((chain label set-attribute) "id" label-id)
	    ((chain label set-attribute) "class" "text no-top-margin")
	    (setf (inner-html label) (float->str value))
	    ((chain td-left append-child) label)))
	((chain td-right append-child) p)
	((chain tr append-child) td-left)
	((chain tr append-child) td-right)
	((chain params-table append-child) tr)))

    (defmacro def-param ((name description type &key var value min max)
			 &body body)
      "Add a user-adjustable parameter to the page
       that is accessible by the rest of the program."
      (let ((callback-name (ps-gensym 'callback-name)))
	(cond ((eq type :checkbox)
	       `(progn
		  (defparameter ,name ,value)
		  (defun ,callback-name (,var)
		    (setf ,name ,var)
		    ,@body)
		  (extend-params-table ,description ,type ',callback-name
				       ,value)))
	      ((eq type :link)
	       `(progn
		  (defun ,callback-name ()
		    ,@body)
		  (extend-params-table ,description ,type ',callback-name)))
	      ((eq type :range)
	       `(progn
		  (defparameter ,name ,value)
		  (defun ,callback-name (,var)
		    (setf ,name ,var)
		    ,@body)
		  (extend-params-table ,description ,type ',callback-name
				       ,value ,min ,max)))
	      (t `(throw (+ "User-adjustable parameter "
			    ',name
			    " has an unrecognized type: "
			    ',type))))))

    (def-param (*reset-sim* "Reset simulation" :link)
	(init-coords *game-state*))

    (def-param (*lock-view-to-pred* "Lock view to predator" :link)
	(setf (chain *game-state* lock-view-to-pred?) t)
      (setf (chain *game-state* lock-view-to-prey?) f))

    (def-param (*lock-view-to-pred* "Lock view to prey" :link)
	(setf (chain *game-state* lock-view-to-pred?) f)
      (setf (chain *game-state* lock-view-to-prey?) t))

    (def-param (*unlock-view* "Unlock view" :link)
	(setf (chain *game-state* lock-view-to-pred?) f)
      (setf (chain *game-state* lock-view-to-prey?) f))

    (def-param (*sim-speed* "Simulation speed" :range :var value
						      :value 0.001
						      :min 0.0001
						      :max 0.0012))

    (def-param (*n-prey* "N (number of prey)" :range :var value
						     :value 40
						     :min 1
						     :max 500)
	(init-coords *game-state*))

    (def-param (*a* "a (swarm->swarm attraction)" :range :var value
							 :value 0.796
							 :min 0
							 :max 10))


    (def-param (*b* "b (prey->predator repulsion)" :range :var value
							  :value 0.708
							  :min 0
							  :max 10))

    (def-param (*c* "c (predator->prey attraction)" :range :var value
							   :value 15
							   :min 0
							   :max 20))

    (def-param (*p* "p (predator->prey attraction falloff)" :range :var value
								   :value 2.1985
								   :min 0
								   :max 5))

    (def-param (*show-help* "Show help" :checkbox :var value :value f)
	(let ((help-table ((chain document get-element-by-id) "help-table")))
	  (setf (chain help-table style visibility)
		(if value "visible" "hidden"))))
    
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
		      (xy->canvas-xy state (list x y))
		      (draw-seg canvas-x)
		      (draw-seg canvas-y nil)))))))

    (defun canvas-xy->xy (state xy)
      (with-game-state state
	(list (+ view-left
		 (* (/ (elt xy 0) (chain canvas width)) view-width))
	      (+ view-bottom
		 (* (/ (elt xy 1) (chain canvas height)) view-height)))))

    (defun xy->canvas-xy (state xy)
      (with-game-state state
	(list (* (/ (- (elt xy 0) view-left) view-width)
		 (chain canvas width))
	      (* (/ (- (elt xy 1) view-bottom) view-height)
		 (chain canvas height)))))

    (defun draw-circle (state xy &optional (color '(0.5 0.5 0.5)) (radius 3))
      (with-game-state state
	(let-xy x y (xy->canvas-xy state xy)
		(setf (chain ctx fill-style) (color->str color))
		(canvas-cmd begin-path)
		(canvas-cmd arc x y radius 0 (* 2 pi))
		(canvas-cmd fill))))

    (defun make-prey-coords (n)
      (let ((prey-coords (list)))
	(dotimes (i n prey-coords)
	  (push (list (random) (random))
		prey-coords))))

    (defun init-coords (state &optional (n-prey (round *n-prey*)))
      (setf (getprop state 'pred-xy)
	    (list (random) (random)))
      (setf (getprop state 'prey-coords)
	    (make-prey-coords n-prey)))

    (defun init-game (state canvas &optional (width 800) (height 400))
      (setf (getprop state 'canvas) canvas)
      (setf (chain canvas width) width)
      (setf (chain canvas height) height)
      (setf (getprop state 'view-width) 8)
      (setf (getprop state 'view-xy) (list 0.5 0.5))
      (setf (chain state lock-view-to-pred?) f)
      (setf (chain state lock-view-to-prey?) f)
      (init-coords state)

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

      ((chain document add-event-listener) "keydown"
       (lambda (e)
	 (unless (eq (chain state mouse-xy) undefined)
	   (cond ((equal (chain e code) "ControlLeft")
		  (unless (chain state lock-prey?)
		    (setf (chain state lock-pred?) t)))
		 ((equal (chain e code) "ShiftLeft")
		  (unless (chain state lock-pred?)
		    (setf (chain state lock-prey?) t)))))))

      ((chain document add-event-listener) "keyup"
       (lambda (e)
	 (unless (eq (chain state mouse-xy) undefined)
	   (cond ((equal (chain e code) "ControlLeft")
		  (setf (chain state lock-pred?) f))
		 ((equal (chain e code) "ShiftLeft")
		  (setf (chain state lock-prey?) f))))))

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

    (defun get-time ()
      ((chain performance now)))

    (defun step-game (state dt)
      (let* ((speed *sim-speed*)
	     (pred-xy (getprop state 'pred-xy))
	     (prey-coords (getprop state 'prey-coords))
	     (prey-dxdts (compute-prey-dxdts pred-xy prey-coords)))
	;; Update the predator position
	(if (chain state lock-pred?)
	    (setf (chain state pred-xy)
		  (canvas-xy->xy state (chain state mouse-xy)))
	    (vec-incf (getprop state 'pred-xy)
		      (scale-vec (* speed dt)
				 (compute-pred-dxdt pred-xy prey-coords))))
	;; Update the prey positions
	(dotimes (i (length prey-coords))
	  (if (and (= i 0) (chain state lock-prey?))
	      (setf (elt prey-coords i)
		    (canvas-xy->xy state (chain state mouse-xy)))
	      (vec-incf (elt prey-coords i)
			(scale-vec (* speed dt) (elt prey-dxdts i)))))
	;; If the view is locked, update the view position
	(cond ((chain state lock-view-to-pred?)
	       (setf (chain state view-xy)
		     (setf (chain state view-xy)
			   (chain state pred-xy))))
	      ((and (chain state lock-view-to-prey?)
		    (>= (length (chain state prey-coords)) 1))
	       (setf (chain state view-xy)
		     (setf (chain state view-xy)
			   (elt (chain state prey-coords) 0)))))))

    (defun draw-game (state)
      (with-game-state state
	(clear-canvas ctx)
	(draw-grid state)
	(draw-circle state (getprop state 'pred-xy) '(1 0 0))
	(dotimes (i (length (chain state prey-coords)))
	  ;; If the view is locked to the prey, draw the first prey in blue
	  (if (and (= i 0) (chain state lock-view-to-prey?))
	      (draw-circle state (elt (chain state prey-coords) i) '(0 0 1))
	      (draw-circle state (elt (chain state prey-coords) i))))))

    (defun run-game (state)
      (labels ((do-frame ()
		 (let ((dt (if (eq (getprop state 'prev-time) undefined)
			       0
			       (- (get-time) (getprop state 'prev-time)))))
		   (setf (getprop state 'prev-time) (get-time))
		   (step-game state dt)
		   (draw-game state)
		   ((chain window request-animation-frame) #'do-frame))))
	((chain window request-animation-frame) #'do-frame)))

    (defparameter *game-state* (create))
    (init-game *game-state* ((chain document get-element-by-id) "game-canvas"))
    (run-game *game-state*)))

(defparameter *html*
  (with-html-output-to-string (s nil :indent t)
    (:html
     (:head
      (:meta :charset "utf-8")
      (:title "Predator-swarm interactions")
      (str (uiop:read-file-string "fonts.html"))
      (:style (str (uiop:read-file-string "style.css"))))
     ;; (:link :href "style.css" :rel "stylesheet"))
     (:body :class "dark-bg"
	    (:h1 :class "text" "Predator-swarm interactions")
	    (:a :class "text"
		:href "https://arxiv.org/abs/1403.3250"
		"Paper")
	    (:br)
	    (:a :class "text"
		:href "https://github.com/peterpaullake/swarm"
		"GitHub repo")
	    ;; (:br)
	    ;; (:a :class "text"
	    ;; 	:href "javascript:;"
	    ;; 	"Slides")
	    ;; (:br)
	    ;; (:a :class "text"
	    ;; 	:href "javascript:;"
	    ;; 	"Live demo")
	    (:br)
	    (:br)
	    (:canvas :id "game-canvas")
	    (:table :id "params-table")
	    (:table :id "help-table" :style "visibility: hidden;"
		    (:tr
		     (:td (:h2 :class "text" "Help"))
		     (:td))
		    (:tr
		     (:td (:p :class "text" "Control predator"))
		     (:td (:p :class "text" "Hold ctrl")))
		    (:tr
		     (:td (:p :class "text" "Control prey"))
		     (:td (:p :class "text" "Hold shift")))
		    (:tr
		     (:td (:p :class "text" "Zoom in/out"))
		     (:td (:p :class "text" "Scroll")))
		    (:tr
		     (:td (:p :class "text" "Move viewport"))
		     (:td (:p :class "text" "Click and drag")))))
     (:script :type "text/javascript" 
	      (str (ps (lisp *ps-lisp-library*))))
     (:script :type "text/javascript"
	      (str *js*)))))

(with-open-file (s "www/index.html" :direction :output :if-exists :supersede)
  (write-sequence *html* s))

(define-easy-handler (home :uri "/") ()
  *html*)

(defparameter *server*
  (start (make-instance 'easy-acceptor
			:address "0.0.0.0" ;; localhost
			:document-root #p"www/"
			:port 8080)))

"
You shouldn't be able to control the prey while the
  view is locked to the prey. Similarly for predator.
use this (rk45, which has adaptive step size):
  https://github.com/imiRemy/RK45js
add a toggle for the lock view
add a play/pause button
add presets to look at the various regimes
"