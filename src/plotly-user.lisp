(defpackage #:plotly-user
  (:use #:cl #:clog-plotly #:plotly)
  (:import-from #:clog)
  (:import-from #:clog-gui)
  (:import-from #:shasht)
  (:import-from #:alexandria)
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
   (closing :accessor closing :initform nil)))

;; Unique IDs for figures
(defvar *id* (make-array 1 :element-type '(unsigned-byte 64) :initial-element 0))
(defvar *free-ids* (list nil))
(defun new-id () (or (sb-ext:atomic-pop (car *free-ids*))
		     (sb-ext:atomic-incf (aref *id* 0))))
(defun reset-ids () (setf (car *free-ids*) nil) (setf *id* (make-array 1 :element-type '(unsigned-byte 64) :initial-element 0)))
(defun return-id (id)
  (sb-ext:atomic-push id (car *free-ids*)))

(defclass plots ()
  ((plots :accessor plots :initform nil :documentation "A list of `plotly::plot's")
   (current-plot :accessor current-plot :initform nil)))

(defvar *plots* nil)
(defvar *body* nil)

(defun connected ()
  (and *body* (clog::validp *body*)))

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
  (or (and *plots* (current-plot *plots*))
      (and *plots* (plots *plots*) (elt (plots *plots*) 0))
      (funcall 'on-file-new-plot *body*)))

(defun on-file-new-plot (obj &optional force-id)
  (let* ((app (clog:connection-data-item obj "plots"))
	 (id (or force-id (new-id)))
	 (name (format nil "Plot ~A" id))
	 (win (clog-gui:create-gui-window obj :title name
				     :has-pinner t :keep-on-top t
                                     :top 200 :left 0 :width 500 :height 500))
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
  (setf (clog:title (clog:html-document body)) "TYPHON plotting workbench")
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

(defun force-refresh-plots ()
  ;; If your browser got disconnected
  (loop for plot in (plots *plots*)
	do (figure (plot-id plot))
	do (plot-data (data plot))))

(defun maybe-start-workbench ()
  (unless (connected)
    (clog:initialize 'on-new-window)
    (clog:open-browser)))

(defun figure (&optional number)
  (maybe-start-workbench)
  (let ((found (find number (plots *plots*) :key #'plot-id)))
    (if found
	(progn (setf (current-plot *plots*) found)
	       (clog-gui:window-to-top-by-title *body* (format nil "Plot ~A" number)))
	(progn (setf (current-plot *plots*) (funcall 'on-file-new-plot *body* number))))))

(defun close-figure (&optional number)
  (maybe-start-workbench)
  (let ((found (find number (plots *plots*) :key #'plot-id)))
    (when found
      (clog-gui:window-close (parent found)))))

(defstruct (uncertain-number (:constructor %make-uncertain-number (x s+ s-)))
  x s+ s-)

(defun uncertain-number-s (x)
  (assert (= (uncertain-number-s+ x) (uncertain-number-s- x)))
  (uncertain-number-s+ x))

(defun make-uncertain-number (x &key s s+ s-)
  (%make-uncertain-number x (or s+ s) (or s- s)))

(defun maybe-uncertain-to-certain (x)
  (if (uncertain-number-p x) (uncertain-number-x x) x))

(defun compose (a b)
  (lambda (&rest rest)
    (funcall a (apply b rest))))

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

(defun hall ()
  (setf (plotly:hold (get-active-plot)) :all))

(defun hoff ()
  (setf (plotly:hold (get-active-plot)) nil))

(defun hon ()
  (setf (plotly:hold (get-active-plot)) t))

(defun call ()
  (loop for plot in (plots *plots*)
	do (clog-gui:window-close (parent plot)))
  (reset-ids)
  (setf (plots *plots*) nil)
  (setf (current-plot *plots*) nil))

(defun clf (&optional (plot (get-active-plot)))
  (setf (plotly:traces plot) nil)
  (setf (plotly:data plot) nil)
  (setf (plotly:layout plot) (make-instance 'plotly:plot-layout ))
  (clog-plotly:purge-plotly (clog-plotly plot)))

(defun refresh (plot)
  (let ((plotly (plotly:clog-plotly plot)))
    (relayout-plotly plotly (serialize-to-json (plotly:layout plot)))))

(defun xrange (min max &optional (plot (get-active-plot)))
  (setf (plotly:range (plotly:x-axis (plotly:layout plot))) (list min max))
  (refresh plot))

(defun yrange (min max &optional (plot (get-active-plot)))
  (setf (plotly:range (plotly:y-axis (plotly:layout plot))) (list min max))
  (refresh plot))
    
(defun plot-data (data &rest rest)
  "matlab style... optional color, marker line type, also :title :x-axis-label :y-axis-label"
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

;; (defun scatter3d (x y z)
;;   (maybe-start-workbench)
;;   (let ((plotly (plotly:clog-plotly plot))
;; 	(hold (plotly:hold plot)))
;;     (clog-plotly:new-plot-plotly
;;      plotly
;;      (serialize-to-json
      
;;     ))
    
;; (defun draw-hat (plotly fx fy)
;;   (let (x-data y-data z-data)
;;     (loop for x from -5f0 to 5f0 by 0.5f0
;; 	  do
;; 	     (loop for y from -5f0 to 5f0 by 0.5f0
;; 		   do
;; 		      (push x x-data)
;; 		      (push y y-data)
;; 		      (push (* (cos (* fx x)) (cos (* fy y))) z-data)))
;;     (clog-plotly:purge-plotly plotly)
;;     (clog-plotly:new-plot-plotly
;;      plotly
;;      (format nil "[{x: [~{~,3f~^,~}], y: [~{~,3f~^,~}], z: [~{~,3f~^,~}],~
;;  mode: 'markers', marker: { size:12, line: {color: 'rgba(217, 217, 217, 0.14)',~
;;  width: 0.5}, opacity: 0.8}, type: 'scatter3d'}]"
;; 	     x-data y-data z-data)
;;      "{ margin: { l: 0, r:0, b: 0, t:0} , title: 'hi', autosize: true}" "{ responsive: true }"
;;      )))


;; (defun label-and-title-plot (&optional x-label y-label title-list legend-list legend-location)
;;   (declare (ignore legend-location))
;;   (let ((plot (get-active-plot)))
;;     (data plot)
;;     (plot-data (data plot) :x-label x-label))
