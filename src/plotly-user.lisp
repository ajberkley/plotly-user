(defpackage #:plotly-user
  (:use #:cl #:clog-plotly #:plotly)
  (:import-from #:clog)
  (:import-from #:clog-gui)
  (:import-from #:shasht)
  (:import-from #:alexandria #:compose)
  (:import-from #:bordeaux-threads #:make-lock #:with-lock-held)
  (:documentation
   "An interactive data exploration workspace.  The underlying plotting is handled by
 plotly in a browser which we talk to via CLOG.

 Goals: reproduce much of the functionality that matlab provides in
 terms of interactive plotting.  Provide a persistent work space for
 working with plots of data.")
  (:export
   #:maybe-start-workbench
   #:get-active-plot
   ))

(in-package :plotly-user)

;; Trying to avoid providing a full windowing system, we provide for
;; multiple browser connections with two main modes, a tabbed plot container,
;; and a free form windowed plot container, on top of the subplot feature of
;; plotly.  We provide for multiple browser tabs / browsers connected simultaneously
;; to the same repl (instead of providing a full windowing system)

(defclass browser-window-plot-container ()
  ((body :accessor body :initarg :body)
   (name :accessor name :initarg :name)
   (plots :accessor plots :initarg :plots :initform (make-instance 'plots)))
  (:documentation "An active window in the browser with an associated clog-body."))

(defclass full-screen-tabbed-plot-container (browser-window-plot-container) ())

(defclass free-form-plot-container (browser-window-plot-container) ())

(defgeneric plot-container-visible (plot-container))

(defgeneric close-plot (container plot))

(defmethod close-plot ((container free-form-plot-container) plot)
  (clog-gui:window-close (parent plot)))

(defgeneric focus (container plot))

(defmethod focus ((container free-form-plot-container) plot)
  (setf *current-browser-window* container)
  (clog-gui:window-focus (parent plot))
  (setf (current-plot (plots container)) plot))

(defgeneric new-plot (container &optional id))

(defvar *browser-windows* nil
  "A list of browser-window-plot-containers.  Remember, this is a singleton service --
 the user is interacting from a REPL to a browser.")

(defvar *current-browser-window* nil
  "The last active window -- where the user last clicked usually, or the one with the
 last drawn on figure or the last selected figure with (figure n).")

(defun set-active-browser-window (browser-window)
  (setf *current-browser-window* browser-window))

(defun get-active-browser-window ()
  (or *current-browser-window* (nth 0 *browser-windows*)))

(defun connected (browser-window)
  (and (body browser-window) (clog::validp (body browser-window))))

(defun reset-state ()
  (setf *browser-windows* nil
	*current-browser-window* nil)
  (reset-ids)
  (values))

(defun get-current-plot (&optional (create-if-does-not-exist t))
  "Return a `plot' that is active, or create a new one (including a browser connection)"
  (let* ((browser-window (get-active-browser-window))
	 (plots (and browser-window (plots browser-window))))
    (or (current-plot plots)
	(when create-if-does-not-exist
	  (unless browser-window
	    (setf browser-window (maybe-start-workbench)))
	  (new-plot browser-window)))))

(defclass plot (plotly-plot)
  ((id :reader plot-id :type fixnum :initarg :id :documentation "A unique numeric id,
 used for user short hand access through (figure id).")
   (name :accessor plot-name :type string :initarg :name :initform ""
	 :documentation "A user set id, usually the title of the plot frame")
   (plotly :accessor clog-plotly :initarg :clog-plotly-element :documentation "
 Hold a reference to a `clog-plotly:clog-plotly-element'.")
   (parent :accessor parent :initarg :parent :documentation "A `clog-gui:clog-gui-window'")
   (hold :accessor hold :initform nil :type (member nil :on :all)
	 :documentation "State of 'trace hold'.  nil means new traces should clear the plot
 first, :on means they should be appended, :all means they should be appended and a new
 color / marker should be chosen automatically for them (not implemented).")
   (closing :accessor closing :initform nil :documentation "Set to t when the figure is
 being closed, to handle multiple callbacks without confusion.")
   (data :accessor data :initform nil :documentation "Stores the data needed to recreate the plot"))
  (:documentation "Represents a plotly plot and the gui window frame around it."))

(defgeneric serialize (plot)
  (:documentation ""))

(defgeneric deserialize (plot)
  (:documentation ""))

(defclass plots ()
  ((plots-lock :reader plots-lock :initform (make-lock "plots-lock"))
   (plots :accessor plots :initform (make-hash-table :test 'eql :synchronized nil)
	  :documentation "A hashtable mapping figure-id to a `plotly::plot'")
   (current-plot :accessor current-plot :initform nil))
  (:documentation "A collection of plots on a GUI.  There is an
 current-plot, which is the last that was drawn to, or clicked in,
 like matlab.  There may be multiple plots collections."))

(defgeneric plot-by-id (plots id)
  (:documentation "Given an id return a `plotly::plot' or nil if it does not exist in
 collection PLOTS")
  (:method ((plots plots) id)
    (with-lock-held ((plots-lock plots))
      (gethash id (plots plots)))))

(defgeneric remove-plot (plots id)
  (:method ((plots plots) id)
    (with-lock-held ((plots-lock plots))
      (remhash id (plots plots)))))

(defgeneric register-plot (plots plot id)
  (:method ((plots plots) plot id)
    (with-lock-held ((plots-lock plots))
      (setf (gethash id (plots plots)) plot))))

;; Unique global IDs for figures.  Figure ids are not sequential as
;; users can ask for figure ids by calling (figure 100).  So we just
;; set an arbitrary upper-limit, and scan for free figures when
;; needed.  More complex schemes aren't worth it.

(defvar *allocated-figure-ids* (make-array 128 :element-type 'bit :initial-element 0))
(defvar *figure-id-lock* (make-lock "figure-id-lock"))
(defun reset-ids ()
  (with-lock-held (*figure-id-lock*)
    (setf *allocated-figure-ids* (make-array 128 :element-type 'bit :initial-element 0))))
(defun return-figure-id (id)
  (with-lock-held (*figure-id-lock*)
    (setf (sbit *allocated-figure-ids* id) 0)))
(defun expand-figure-id-table/no-lock
    (&optional (new-length (* 2 (length *allocated-figure-ids*))))
  "Must be called within the *figure-id-lock*"
  (let ((new-array (make-array new-length :element-type 'bit)))
    (replace new-array *allocated-figure-ids*)
    (setf *allocated-figure-ids* new-array)))
(defun figure-id-allocated (id)
  (with-lock-held (*figure-id-lock*)
    (= (sbit *allocated-figure-ids* id) 1)))
(defun allocate-figure-id (&optional requested-id)
  "Returns id"
  (with-lock-held (*figure-id-lock*)
    (cond
      ((null requested-id)
       (let ((new-id (position 0 *allocated-figure-ids*)))
	 (cond (new-id
		(setf (sbit *allocated-figure-ids* new-id) 1)
		new-id)
	       (t
		(expand-figure-id-table/no-lock)
		(let ((position (position 0 *allocated-figure-ids*)))
		  (setf (sbit *allocated-figure-ids* position) 1)
		  position)))))
      ((and requested-id (> requested-id (1+ (length *allocated-figure-ids*))))
       (expand-figure-id-table/no-lock (1+ requested-id))
       (setf (sbit *allocated-figure-ids* requested-id) 1)
       requested-id)
      (requested-id
       (when (= (sbit *allocated-figure-ids* requested-id) 0)
	 (setf (sbit *allocated-figure-ids* requested-id) 1))
       requested-id))))

(defun get-active-plot (&optional (container (get-active-browser-window)))
  "Used internally; get the last clicked plot, or the zeroth plot, or create a new figure
 with an empty plot.  Returns a `plot'"
  (unless container
    (setf container (maybe-start-workbench)))
  (or (current-plot (plots container))
      (setf (current-plot (plots container)) (new-plot container))))

(defmethod new-plot ((container free-form-plot-container) &optional (id (allocate-figure-id)))
  (funcall 'on-file-new-plot (body container) id))

(defun on-file-new-plot (obj &optional (id (allocate-figure-id)))
  "Handles the callback when someone clicks NEW PLOT in a windowed
 browser window, also used to generate a new figure from the repl when
 calling (figure)."
  (declare (optimize (debug 3)))
  (let* ((container (loop for container = (clog:connection-data-item obj "container")
			  until container
			  finally (return container)
			  do (sleep 1))) ;; sometimes container is not fully up yet
	 (app (plots container))
	 (name (format nil "Plot ~A" id))
	 (win (clog-gui:create-gui-window obj :title name
				     :has-pinner t :keep-on-top t
					      :top (+ 50 (* 500 (mod (floor id 2) 2)))
					      :left (+ 0 (* 500 (mod id 2)))
					      :width 500 :height 500))
	 (div (clog:create-div (clog-gui:window-content win)))
	 (plotly (clog-plotly:create-clog-plotly-element div)))
    (clog-plotly:attach-clog-plotly plotly)
    (let* ((plot (make-instance 'plot :name name :parent win :id id :clog-plotly-element plotly))
	   (mark-current-bring-to-front
	     (lambda (&rest rest) (declare (ignore rest))
	       (unless (closing plot)
		 (setf (current-plot (plots container)) plot)
		 (clog-gui:window-to-top-by-title (body container) name)))))
      (clog:set-on-click win mark-current-bring-to-front)
      (clog:set-on-click plotly mark-current-bring-to-front)
      (register-plot (plots container) plot id)
      (clog-gui:set-on-window-size-done win (lambda (&rest rest)
				     (declare (ignore rest))
				     (setf (plotly:width (plotly:layout plot)) (- (clog:width win) 10))
				     (setf (plotly:height (plotly:layout plot)) (- (clog:height win) 10))
				     (let ((new-size (serialize-to-json (plotly:layout plot))))
				       (relayout-plotly plotly new-size))))
      (clog-gui:set-on-window-close win (lambda (&rest rest)
					  (declare (ignore rest) (optimize (debug 3)))
					  (let ((id (plot-id plot)))
					    (return-figure-id id)
					    (remove-plot app id))
					  (when (or (eq (current-plot app) plot)
						    (zerop (hash-table-count (plots app))))
					    (setf (current-plot app) nil))
					  (setf (closing plot) t)))
      plot)))

(defun create-windowed-plot-view (body)
  (clog-gui:clog-gui-initialize body)
  (clog:add-class body "w3-cyan")
  (let* ((menu  (clog-gui:create-gui-menu-bar body))
         (file  (clog-gui:create-gui-menu-drop-down menu :content "File"))
         (win   (clog-gui:create-gui-menu-drop-down menu :content "Window")))
    (clog-gui:create-gui-menu-item file :content "New Plot" :on-click 'on-file-new-plot)
    (clog-gui:create-gui-menu-item win :content "Tile all" :on-click 'tile-all-windows)
    (clog-gui:create-gui-menu-item win :content "Maximize All" :on-click 'maximize-all-windows)
    (clog-gui:create-gui-menu-item win :content "Normalize All" :on-click 'normalize-all-windows)
    (clog-gui:create-gui-menu-full-screen menu))
  (clog:set-on-before-unload (clog:window body) (lambda(obj)
						  (declare (ignore obj))
						  ;; return empty string to prevent nav off page
						  "")))

(defun on-new-browser-window
    (body &key (browser-window-name (format nil "~A: plot workbench" (length *browser-windows*)))
	    (type (or :windowed :tabbed)))
  (setf (clog:title (clog:html-document body)) browser-window-name)
  (let ((container (make-instance 'free-form-plot-container
				  :body body :name browser-window-name)))
    (push container *browser-windows*) ;; not thread safe
    ;; (clog:debug-mode body) -- makes the console in the browser more useful
    (ecase type
      (:windowed (create-windowed-plot-view body)))
    ;; Why doesn't MathJax 3.x work?
    (clog:load-script
     (clog:html-document (clog:connection-data-item body "clog-body"))
     "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML")
    (format t "Associated with new container ~A~%" container)
    (setf (clog:connection-data-item body "container") container)))

(defun maybe-start-workbench (&optional (index 0))
  "Open up a browser window if it is not already connected.  Returns a
 `browser-window'"
  (let ((workbench (nth index *browser-windows*)))
    (cond
      ((and workbench (connected workbench))
       workbench)
      (t
       (let ((old-window (car *browser-windows*)))
	 (clog:initialize 'on-new-browser-window)
	 (clog:open-browser)
	 (loop for container = (car *browser-windows*)
	       until (not (eq container old-window))
	       finally (return container)))))))

(defun figure (&optional number (browser-window (get-active-browser-window)))
  "Create a new figure, or make an extant figure active and bring it to the front"
  (declare (optimize (debug 3)))
  (let ((browser-window (or browser-window (maybe-start-workbench))))
    (cond
      ((and number (figure-id-allocated number))
       (multiple-value-bind (browser-window figure)
	   (find-figure number)
	 (focus browser-window figure)
	 (plot-by-id (plots browser-window) number)))
      ((and number (not (figure-id-allocated number)))
       (allocate-figure-id number)
       (focus browser-window (new-plot browser-window number)))
      (t
       (focus browser-window (new-plot browser-window))))))

(defun find-figure (number)
  (loop for browser-window in *browser-windows*
	for found = (plot-by-id (plots browser-window) number)
	until found
	finally (return (values browser-window found))))
	
(defun close-figure (number)
  "Close a figure"
  (multiple-value-bind (browser-window figure)
      (find-figure number)
    (when figure (close-plot browser-window figure))))


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
	 (destructuring-bind (&key data color marker marker-size line name)
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
		     (make-instance 'plotly:marker :color color :size (or marker-size 8) :symbol marker)))
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
  (destructuring-bind (&key color marker marker-size line title x-axis-label y-axis-label legend)
      (if (stringp (first rest))
	  (print (append (parse-matlab-style (first rest)) (cdr rest)))
	  rest)
    (unless (listp legend) (setf legend (list legend)))
    (let* ((plot (get-active-plot))
	   (hold (plotly:hold plot))
	   (ensure-list-data
	     (if (= (length (first data)) 2)
		 (list (list :data data :color color :marker marker :marker-size marker-size :line line :name (or (elt legend 0) "")))
		 (if legend
		     (mapcar (lambda (d l) (list :data d :color color :marker marker :marker-size marker-size :line line :name l))
			     data legend)
		     (mapcar (lambda (d) (list :data d :color color :marker marker :marker-size marker-size :line line))
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
      ;; Annotations don't work well yet.  Trying to let plotly handle editing of them
      ;; makes a mess since we want to be in control of the plot.  We cannot poke at them
      ;; or their arrows, etc.
      (let ((last-called (get-universal-time)))
	(clog-plotly::set-on-plotly-click (clog-plotly plot)
					  (lambda (data &aux (now (get-universal-time)))
					    (unless (= now last-called)
					      (setf last-called now)
					      (let ((ht (shasht:read-json data)))
						(toggle-annotation
						 (layout plot)
						 (make-instance 'plotly:annotation
								:x (gethash "x" ht)
								:y (gethash "y" ht)
								:text
								(format nil "~,3f, ~,3f"
									(gethash "x" ht)
									(gethash "y" ht))
								:showarrow t))
						(refresh plot))))))
      (plot-to-active-plot plot ;; :config (make-instance 'plotly:plotly-config :editable t )
			   )))
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

(defun plot-to-active-plot (plotly-plot &key config (active-plot (get-active-plot)))
  "config should be a plotly:plotly-config"
  (maybe-start-workbench)
  (let ((plotly (plotly:clog-plotly active-plot)))
    (clog-plotly::new-plot-plotly*
     plotly
     (serialize-to-json (plotly:traces plotly-plot))
     (serialize-to-json (plotly:layout plotly-plot))
     (if config (serialize-to-json config) ""))))

(defun scatter3d (triplets &key (color "blue") (size 4) (plot (get-active-plot)))
  "no real features yet...
    (let (data)
      (loop for x from -0.7 below 0.7 by 0.05
	    do
	       (loop for y from -0.7 below 0.7 by 0.05
		     do
			(push (list x y (* (cos (* 2 pi x x)) (cos (* 2 pi y y)))) data)))
      (scatter3d data))"
  (maybe-start-workbench)
  (let* ((marker (make-instance 'plotly:marker :symbol "circle" :size size :color color))
	 (x (mapcar #'first triplets))
	 (y (mapcar #'second triplets))
	 (z (mapcar #'third triplets))
	 (traces (list (make-instance 'plotly:3d-trace :x x :y y
						       :z z
						       :marker marker
						       :type "scatter3d")))
	 (layout (make-instance 'plotly:plot-layout
				:width (- (clog:width (plotly:parent plot)) 10)
				:height (- (clog:height (plotly:parent plot)) 10))))
    (plot-to-active-plot
     (make-instance 'plotly-plot :traces traces :layout layout)
     :active-plot plot)))

(defun surface (x y z &key (plot (get-active-plot)))
  "Z should be a 2D, x and y 1d sequences.
  (let* ((x (loop for x from -4 below 4 by 0.1 collect x))
         (y (loop for y from -8 below 8 by 0.1 collect y)) 
	 (z (make-array (list (length y) (length x)))))
     (loop for x in x
   	   for xidx from 0
	   do (loop for y in y
		    for yidx from 0
		    do (setf (aref z yidx xidx) (* (cos (+ (* x x) (* y y)))))))
    (surface x y z))"
  (maybe-start-workbench)
  (let* ((traces (list (make-instance 'plotly:3d-trace :x x :y y :z z
						       :type "surface")))
	 (layout (make-instance 'plotly:plot-layout
				:width (- (clog:width (plotly:parent plot)) 10)
				:height (- (clog:height (plotly:parent plot)) 10))))
    (plot-to-active-plot
     (make-instance 'plotly-plot :traces traces :layout layout)
     :active-plot plot)))
    
(defun label-and-title-plot (&optional x-label y-label title)
  "Add x-label, y-label, and title.  title works on 3d plots, but x-label and
 y-label doesn't... I just haven't read the plotly docs on 3d scatter plots yet."
  (let* ((plot (get-active-plot))
	 (layout (plotly:layout plot)))
    (setf (plotly:text (plotly:title (plotly:x-axis layout))) x-label)
    (setf (plotly:text (plotly:title (plotly:y-axis layout))) y-label)
    (setf (plotly:text (plotly:title layout)) title)
    (refresh plot)))

(defun demo ()
  (maybe-start-workbench)
  (figure)
  (hoff)
  (plot-data
   (loop for x from -10 below 10
         collect (list (make-uncertain-number (+ x (- (random 1.0) 0.5))
					      :s+ (random 0.5d0) :s- (random 0.5d0))
		       (make-uncertain-number (+ x (- (random 1d0) 0.5))
					      :s+ (random 0.5d0) :s- (random 0.5d0))))
   :x-axis-label "$\\text{Time }(\\mu\\text{s})$" :y-axis-label "$\\sqrt{signal}$"
   :title "A plot" :line "solid" :marker "square" :color "red" :legend '("my first trace"))
  (hon)
  (plot-data
   (loop for x from -10 below 10
         collect (list (make-uncertain-number (+ x (- (random 1.0) 0.5))
					      :s+ (random 0.5d0) :s- (random 0.5d0))
		       (make-uncertain-number (- (+ x (- (random 1d0) 0.5)))
					      :s+ (random 0.5d0) :s- (random 0.5d0))))
   :x-axis-label "$\\text{Time }(\\mu\\text{s})$" :y-axis-label "$\\sqrt{signal}$"
   :marker-size 4
   :title "A plot" :line "solid" :marker "circle" :color "blue" :legend '("my second trace"))
  (figure)
  (let (data)
    (loop for x from -0.7 below 0.7 by 0.05
	  do
	     (loop for y from -0.7 below 0.7 by 0.05
		   do
		      (push (list x y (* (cos (* 2 pi x x)) (cos (* 2 pi y y)))) data)))
    (scatter3d data))
  (figure)
  (let* ((x (loop for x from -4 below 4 by 0.1 collect x))
         (y (loop for y from -8 below 8 by 0.1 collect y)) 
	 (z (make-array (list (length y) (length x)))))
    (loop for x in x
   	  for xidx from 0
	  do (loop for y in y
		   for yidx from 0
		   do (setf (aref z yidx xidx) (* (cos (+ (* x x) (* y y)))))))
    (surface x y z)))
