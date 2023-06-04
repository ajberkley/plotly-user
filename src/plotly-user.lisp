(defpackage #:plotly-user
  (:use #:cl #:clog-plotly #:plotly)
  (:import-from #:clog)
  (:import-from #:clog-gui)
  (:import-from #:shasht)
  (:import-from #:alexandria #:compose)
  (:documentation
   "An interactive data exploration workspace.  The underlying plotting is handled by
 plotly in a browser which we talk to via CLOG.

 Goals: reproduce much of the functionality that matlab provides in
 terms of interactive plotting and plot editing.

 Matlab provides dock-able plots, it's kind of nice to be able to drag
 a plot to a dock and have them either be cyclable or tileable.  So
 that's our first thing.  We want windows which can be resized which
 can be containers for plots.  We want to be able to save the
 graphical workspace and load it later.  We want to export things to
 say emacs org mode directly, or to confluence or other systems.

 It would be good to integrate this with lisp-stat or with some of the
 other data analysis systems.")
  (:export
   #:maybe-start-workbench
   #:get-active-plot
   ))

(in-package :plotly-user)

(defclass plot (plotly-plot)
  ((id :reader plot-id :initarg :id :documentation "A unique id")
   (name :accessor plot-name :initarg :name :documentation "A user set id")
   (plotly :accessor clog-plotly :initarg :clog-plotly-element)
   (parent :accessor parent :initarg :parent)
   (hold :accessor hold :initform nil :type (member nil :on :all))
   (last-touched :accessor last-touched :initform (get-universal-time) :type fixnum)
   (data :accessor data :initform nil)
   (closing :accessor closing :initform nil))
  (:documentation "Represents a plotly plot."))

(defclass plots ()
  ((plots :accessor plots :initform nil :documentation "A list of `plotly::plot's")
   (current-plot :accessor current-plot :initform nil))
  (:documentation "A representation of all the active plots on the GUI.  There is an
 current-plot, which is the last that was drawn to, or clicked in, like matlab"))

;; Our interface is a singleton: this isn't a web application with multiple users.
;; The user is interacting from the REPL.  *plots*
(defvar *plots* nil "Contains an instance of 'plots, or null")
(defvar *body* nil "The main html web page body, held here for basic connectivity checking")

;; Unique IDs for figures
(defvar *id* (make-array 1 :element-type '(unsigned-byte 64) :initial-element 0))
(defvar *free-ids* (list nil))
(defun new-id () (or (sb-ext:atomic-pop (car *free-ids*))
		     (sb-ext:atomic-incf (aref *id* 0))))
(defun reset-ids () (setf (car *free-ids*) nil) (setf *id* (make-array 1 :element-type '(unsigned-byte 64) :initial-element 0)))
(defun return-id (id)
  (sb-ext:atomic-push id (car *free-ids*)))

;; Not sure if this is the right thing to do, but detects if the user closes the browser pane
;; for example.
(defun connected ()
  (and *body* (clog::validp *body*)))

;; Why doesn't 3.x work?
(defun plot-gui-initialize (body)
  (reset-ids)
  (clog-gui:clog-gui-initialize body)
  (clog:load-script
   (clog:html-document (clog:connection-data-item body "clog-body"))
   "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML")
  ;; something to track our active plots
  (let ((plots (make-instance 'plots)))
    (setf *body* body) ;; for driving from the repl
    (setf *plots* plots)
    (setf (clog:connection-data-item body "plots") plots)))

(defun get-active-plot ()
  "Used internally; get the last clicked plot, or the zeroth plot, or create a new figure
 with an empty plot"
  (or (and *plots* (current-plot *plots*))
      (and *plots* (plots *plots*) (elt (plots *plots*) 0))
      (funcall 'on-file-new-plot *body*)))

(defun on-file-new-plot (obj &optional force-id)
  "Handles the callback when someone clicks NEW PLOT, also used to generate a new figure
 from the repl when calling (figure)."
  (let* ((app (clog:connection-data-item obj "plots"))
	 (id (or force-id (new-id)))
	 (name (format nil "Plot ~A" id))
	 (win (clog-gui:create-gui-window obj :title name
				     :has-pinner t :keep-on-top t
					      :top (+ 100 (* 500 (floor id 2)))
					      :left (+ 0 (* 500 (mod id 2)))
					      :width 500 :height 500))
	 (div (clog:create-div (clog-gui:window-content win)))
	 (plotly (clog-plotly:create-clog-plotly-element div)))
    (clog-plotly:attach-clog-plotly plotly)
    (let ((plot (make-instance 'plot :name name :parent win :id id :clog-plotly-element plotly)))
      (clog:set-on-click win (lambda (&rest rest) (declare (ignore rest)) (unless (closing plot) (setf (current-plot *plots*) plot))))
      (clog:set-on-click plotly (lambda (&rest rest) (declare (ignore rest)) (unless (closing plot) (setf (current-plot *plots*) plot))))
      (push plot (plots app))
      (clog-gui:set-on-window-size-done win (lambda (&rest rest)
				     (declare (ignore rest))
				     (setf (plotly:width (plotly:layout plot)) (- (clog:width win) 10))
				     (setf (plotly:height (plotly:layout plot)) (- (clog:height win) 10))
				     (let ((new-size (serialize-to-json (plotly:layout plot))))
				       (relayout-plotly plotly new-size))))
      (clog-gui:set-on-window-close win (lambda (&rest rest)
				 (declare (ignore rest) (optimize (debug 3)))
				 (return-id (plot-id plot))
				 (setf (plots app) (remove plot (plots app)))
				 (when (or (eq (current-plot app) plot) (null (plots app)))
				   (setf (current-plot app) nil))
				 (setf (closing plot) t)))
      (figure id)
      plot)))

;; Acknowledge the great work!
(defun on-help-about (obj)
  (let* ((about
	   (clog-gui:create-gui-window obj
                              :title   "About"
                              :content "<div class='w3-black'>
                                         <center><img src='/img/lisplogo_warning2_128.png'></center>
                                         <center><i> and made possible by </i></center>
                                         <center><img src='/img/clogwicon.png'></center>
                                         <center>CLOG</center>
                                         <center>The Common Lisp Omnificent GUI</center>
                                         <center>created by: David Botton</center></div>"
                              :hidden  t
                              :width   200
                              :height  215)))
    (clog-gui:window-center about)
    (setf (clog:visiblep about) t)
    (clog-gui:set-on-window-can-size about (lambda (obj)
                                    (declare (ignore obj))()))))

(defun on-new-window (body)
  (setf (clog:title (clog:html-document body)) "Plotting workbench")
  (plot-gui-initialize body)
  (clog:add-class body "w3-cyan")
  (let* ((menu  (clog-gui:create-gui-menu-bar body))
         (file  (clog-gui:create-gui-menu-drop-down menu :content "File"))
         (win   (clog-gui:create-gui-menu-drop-down menu :content "Window"))
         (help   (clog-gui:create-gui-menu-drop-down menu :content "Help")))
    (clog-gui:create-gui-menu-item file :content "New Plot" :on-click 'on-file-new-plot)
    (clog-gui:create-gui-menu-item win :content "Tile all" :on-click 'tile-all-windows)
    (clog-gui:create-gui-menu-item win :content "Maximize All" :on-click 'maximize-all-windows)
    (clog-gui:create-gui-menu-item win :content "Normalize All" :on-click 'normalize-all-windows)
    (clog-gui:create-gui-menu-item help :content "About" :on-click 'on-help-about)
    (clog-gui:create-gui-menu-full-screen menu))
  (clog:set-on-before-unload (clog:window body) (lambda(obj)
                                             (declare (ignore obj))
                                             ;; return empty string to prevent nav off page
                                        "")))

(defun maybe-start-workbench ()
  "Open up a browser window if it is not already connected."
  (unless (connected)
    (clog:initialize 'on-new-window)
    (clog:open-browser))
  (values))

(defun figure (&optional number)
  "Create a new figure, or make an extant figure active and bring it to the front"
  (maybe-start-workbench)
  (let ((found (find number (plots *plots*) :key #'plot-id)))
    (if found
	(prog1 (setf (current-plot *plots*) found)
	       (clog-gui:window-to-top-by-title *body* (format nil "Plot ~A" number)))
	(setf (current-plot *plots*) (funcall 'on-file-new-plot *body* number)))))

(defun close-figure (number)
  "Close a figure"
  (maybe-start-workbench)
  (let ((found (find number (plots *plots*) :key #'plot-id)))
    (when found
      (clog-gui:window-close (parent found)))))

;; Uncertain numbers, for representing the x and y positions of data with
;; potentially asymmetric error bars

(defstruct (uncertain-number (:constructor %make-uncertain-number (x s+ s-)))
  x s+ s-)

(defun uncertain-number-s (x)
  (assert (= (uncertain-number-s+ x) (uncertain-number-s- x)))
  (uncertain-number-s+ x))

(defun make-uncertain-number (x &key s s+ s-)
  (%make-uncertain-number x (or s+ s) (or s- s)))

(defun maybe-uncertain-to-certain (x)
  (if (uncertain-number-p x) (uncertain-number-x x) x))

;; MATLAB like interfaces
(defun hall ()
  "Set the current plot to append new traces, and automatically
 cycle colors/markers (not currently implemented, behaves like (hon))
 matlab: hold all"
  (setf (plotly:hold (get-active-plot)) :all))

(defun hoff ()
  "Set the current plot to be cleared if new traces are added.
 matlab: hold off"
  (setf (plotly:hold (get-active-plot)) nil))

(defun hon (&optional (plot (get-active-plot)))
  "Set the current plot to append new traces to the plot.  matlab: hold on"
  (setf (plotly:hold plot) t))

(defun call ()
  "Close all figures.  matlab: close all"
  (loop for plot in (plots *plots*)
	do (clog-gui:window-close (parent plot)))
  (reset-ids)
  (setf (plots *plots*) nil)
  (setf (current-plot *plots*) nil))

(defun refresh (plot)
  "Redraw.  Use if you have modified, for example, the layout of the plot."
  (let ((plotly (plotly:clog-plotly plot)))
    (relayout-plotly plotly (serialize-to-json (plotly:layout plot))))
  plot)

(defun xlim (min max &optional (plot (get-active-plot)))
  "Change the visible x-range on a plot.  Acts on the last clicked
 plot or the plot you specify (for example what is returned from
 creating/activating a figure with: (figure 1)).  matlab: xlim([min max])"
  (setf (plotly:range (plotly:x-axis (plotly:layout plot))) (list min max))
  (refresh plot))

(defun ylim (min max &optional (plot (get-active-plot)))
  "Change the visible y-range on a plot.  Acts on the last clicked
 plot or the plot you specify (for example what is returned from
 creating/activating a figure with: (figure 1).  matlab: ylim([min max])"
  (setf (plotly:range (plotly:y-axis (plotly:layout plot))) (list min max))
  (refresh plot))

;; Tools for plotting
(defun data-and-style-to-traces (data-and-style)
  (map 'list
       (lambda (x)
	 (destructuring-bind (&key data color marker line name)
	     x
	   (let ((data
		   (if (not (listp (elt data 0)))
		       (loop for count from 0 for d in data collect (list count d))
		       data))
		 (trace
		   (make-instance 'plotly:scatter-trace :name name
							:x (mapcar (compose #'maybe-uncertain-to-certain #'first) data)
							:y (mapcar (compose #'maybe-uncertain-to-certain #'second) data))))
	     (when (uncertain-number-p (elt (elt data 0) 0)) ;; x-error-bars
	       (setf (plotly:trace-error-x trace)
		     (make-instance 'plotly:error-bar :symmetric t
						      :array (mapcar (compose #'uncertain-number-s+ #'first) data))))
	     (when (uncertain-number-p (elt (elt data 0) 1))
	       (setf (plotly:trace-error-y trace)
		     (make-instance
		      'plotly:error-bar :symmetric nil
		      :array (mapcar (compose #'uncertain-number-s+ #'second) data)
		      :arrayminus (mapcar (compose #'uncertain-number-s- #'second) data))))
	     (when line
	       (setf (plotly:trace-line trace)
		     (make-instance 'plotly:line :color color :line-dash line :width 2.0)))
	     (when marker
	       (setf (plotly:trace-marker trace)
		     (make-instance 'plotly:marker :color color :size 14 :symbol marker)))
	     (setf (plotly:trace-mode trace)
		   (cond
		     ((and marker line) "lines+markers")
		     (line "lines")
		     (t "markers")))
	     trace)))
       data-and-style))

(defun plot-data (data &rest rest)
  "Plot a single 2D trace or multiple 2D traces.  Not a particularly good interface, but
 fine for playing with now.  Example:
 (plot-data
  (loop for x from -10 below 10
        collect (list (make-uncertain-number (+ x (- (random 1.0) 0.5))
					    :s+ (random 0.5d0) :s- (random 0.5d0))
		     (make-uncertain-number (+ x (- (random 1d0) 0.5))
					    :s+ (random 0.5d0) :s- (random 0.5d0))))
  :x-axis-label \"$\\text{Time }(\\mu\\text{s})$\" :y-axis-label \"$\\sqrt{signal}$\"
  :title \"A plot\" :line \"solid\" :marker \"cross\" :color \"red\" :legend '(\"my trace name\"))"
  (declare (optimize (debug 3)))
  (maybe-start-workbench)
  (destructuring-bind (&key color marker line (title "") (x-axis-label "") (y-axis-label "") legend)
      (if (stringp (first rest))
	  (print (append (parse-matlab-style (first rest)) (cdr rest)))
	  rest)
    (unless (listp legend) (setf legend (list legend)))
    (let* ((plot (get-active-plot))
	   (hold (plotly:hold plot))
	   (ensure-list-data
	     (if (= (length (first data)) 2)
		 (list (list :data data :color color :marker marker :line line :name (or (elt legend 0) "")))
		 (if legend
		     (mapcar (lambda (d l) (list :data d :color color :marker marker :line line :name l))
			     data legend)
		     (mapcar (lambda (d) (list :data d :color color :marker marker :line line))
			     data)))))
      ;; TODO make hold all cycle colors/markers if not specified
      (if hold
	  (setf (plotly:data plot) (append (plotly:data plot) ensure-list-data))
	  (setf (plotly:data plot) ensure-list-data))
      (setf (plotly:traces plot) (data-and-style-to-traces (plotly:data plot)))
      (setf (plotly:width (plotly:layout plot)) (- (clog:width (plotly:parent plot)) 10))
      (setf (plotly:height (plotly:layout plot)) (- (clog:height (plotly:parent plot)) 10))
      (setf (plotly:text (plotly:title (plotly:layout plot)))
	    (or title (plotly:text (plotly:title (plotly:layout plot)))))
      (setf (plotly:text (plotly:title (plotly:x-axis (plotly:layout plot))))
	    (or x-axis-label (plotly:text (plotly:title (plotly:x-axis (plotly:layout plot))))))
      (setf (plotly:text (plotly:title (plotly:y-axis (plotly:layout plot))))
	    (or y-axis-label (plotly:text (plotly:title (plotly:y-axis (plotly:layout plot))))))
      (plot-to-active-plot plot)))
  (values))
    
;; Converting from matlab single string styles to plotly styles
(defun search-and-delete (search-string string)
  (let ((position (search search-string string)))
    (if position
	(values t (concatenate 'string (subseq string 0 position) (subseq string (+ position (length search-string)))))
	(values nil string))))

(defun parse-matlab-style (&optional (matlab-style ""))
  (let ((color "b") (marker nil) (line nil))
    ;; I don't even know the syntax
    (loop for marker-long-name across #("square" "diamond" "pentagram" "hexagram")
	  do (multiple-value-bind (found remainder)
		 (search-and-delete marker-long-name matlab-style)
	       (when found
		 (setf marker marker-long-name)
		 (setf matlab-style remainder))))
    (loop for line-name across #("--" ":" "-." "-")
	  for converted-line-name across #("dashed" "dot" "dashdot" "solid")
	  do (multiple-value-bind (found remainder)
		 (search-and-delete line-name matlab-style)
	       (when found
		 (setf line converted-line-name)
		 (setf matlab-style remainder))))
    (unless (= (length matlab-style) 0)
      (let ((maybe-color (matlab-colour->plotly-colour (elt matlab-style 0))))
	(when maybe-color (setf color maybe-color) (setf matlab-style (subseq matlab-style 1))))) 
   (unless (= (length matlab-style) 0)
      (let ((maybe-marker (matlab-marker->plotly-marker (elt matlab-style 0))))
	(when maybe-marker (setf marker maybe-marker) (setf matlab-style (subseq matlab-style 1)))))
    (unless (= (length matlab-style) 0)
      (let ((maybe-color (matlab-colour->plotly-colour (elt matlab-style 0))))
	(when maybe-color (setf color maybe-color) (setf matlab-style (subseq matlab-style 1)))))
    (append (when color (list :color color))
	    (when marker (list :marker marker))
	    (when line (list :line line)))))

(defun matlab-marker->plotly-marker (marker-char)
  ;; Not complete
  (case marker-char
    (#\. "circle-dot")
    (#\o "circle-open")
    (#\d "diamond")
    (#\x "x")
    (#\* "asterix")
    (#\+ "cross")
    (#\^ "triangle-up")
    (#\v "triangle-down")
    (#\> "triangle-right")
    (#\< "triangle-left")
    (#\| "line-ns")
    (#\_ "line-ew")))

(defun matlab-colour->plotly-colour (colour-char)
  ;; translates matlab colour specifiers to plotly colours
  (case colour-char
    (#\r "red")
    (#\g "green")
    (#\b "blue")
    (#\c "cyan")
    (#\m "magenta")
    (#\y "yellow")
    (#\k "black")
    (#\w "white")))

(defun plot-to-active-plot (plotly-plot &optional (active-plot (get-active-plot)))
  (maybe-start-workbench)
  (let ((plotly (plotly:clog-plotly active-plot)))
    (clog-plotly:new-plot-plotly
     plotly
     (serialize-to-json (plotly:traces plotly-plot))
     (serialize-to-json (plotly:layout plotly-plot)))))

(defun scatter3d (x y z &key (color "blue") (size 4) (plot (get-active-plot)))
  "no real features yet...
    (let (data)
      (loop for x from -0.7 below 0.7 by 0.05
	    do
	       (loop for y from -0.7 below 0.7 by 0.05
		     do
			(push (list x y (* (cos (* 2 pi x x)) (cos (* 2 pi y y)))) data)))
      (scatter3d (mapcar #'first data)
		 (mapcar #'second data)
		 (mapcar #'third data)))"
  (maybe-start-workbench)
  (let* ((marker (make-instance 'plotly:marker :symbol "circle" :size size :color color))
	 (traces (list (make-instance 'plotly:3d-trace :x x :y y
						       :z z
						       :marker marker
						       ;; :type "surface"
						       )))
	 (layout (make-instance 'plotly:plot-layout
				:width (- (clog:width (plotly:parent plot)) 10)
				:height (- (clog:height (plotly:parent plot)) 10))))
    (plot-to-active-plot
     (make-instance 'plotly-plot :traces traces :layout layout)
     plot)))
    
(defun label-and-title-plot (&optional x-label y-label title)
  "Add x-label, y-label, and title.  title works on 3d plots, but x-label and
 y-label doesn't... I just haven't read the plotly docs on 3d scatter plots yet."
  (let* ((plot (get-active-plot))
	 (layout (plotly:layout plot)))
    (setf (plotly:text (plotly:title (plotly:x-axis layout))) x-label)
    (setf (plotly:text (plotly:title (plotly:y-axis layout))) y-label)
    (setf (plotly:text (plotly:title layout)) title)
    (refresh plot)))
