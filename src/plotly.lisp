(defpackage #:plotly
  (:documentation
   "This package is responsible for generating javascript objects
 (encoded in JSON) which can be used in calls to the plotly javascript
 library.  You create a CLOS object populating the necessary slots,
 then call (serialize-to-json that-object) (or list there-of) and then
 use it in a call through CLOG-PLOTLY (for example) to plotly running
 in a browser.

 This package ends at the call (serialize-to-json scatter-trace), for
 example.  The mechanism of interaction with the javascript plotly
 library is direct serialization of CLOS objects where their slot
 names match the javascript slots in the plotly library.  I have not
 done a complete mapping of them, nor have I added any documentation.
 Generally if there is a feature you want to use, find the slot that
 controls it, and when you create your object set that slot to the
 appropriate plotly string (or keyword).  Unbound slots are not
 serialized.")
  (:use #:cl #:clog-plotly)
  (:import-from #:alexandria #:compose)
  (:export
   #:serialize-to-json
   
   #:plot-layout
   #:margin
   #:xaxis
   #:yaxis

   #:trace-type
   #:scatter-trace
   #:trace-name
   #:trace-visible
   #:trace-showlegend
   #:trace-opacity
   #:trace-mode
   #:trace-x
   #:trace-y
   #:trace-text
   #:trace-xaxis
   #:trace-yaxis
   #:trace-marker
   #:trace-line
   #:trace-textfont
   #:trace-error-x
   #:trace-error-y

   #:error-bar
   #:error-bar-array
   #:error-bar-arrayminus
   
   #:line
   #:line-dash

   ;; For both error-bar and line
   #:color
   #:thickness
   #:visible
   #:width

   #:marker
   #:marker-color
   #:marker-colorbar
   #:size
   #:marker-symbol
   #:font-family
   #:plotly-font
   #:title-font
   #:text
   #:title
   #:axis-title
   #:plotly-plot
   #:plot
   #:plot-id
   #:plot-name
   #:clog-plotly
   #:parent
   #:hold
   #:traces
   #:plot-title
   #:make-plot-title
   #:last-touched
   #:data
   #:height
   #:layout
   #:range
   #:x-axis
   #:y-axis
   #:3d-trace
   #:plotly-config
   #:annotation
   #:add-annotation
   #:toggle-annotation))

(in-package #:plotly)

;; We directly map the javascript objects to CLOS objects and provide
;; for serializing them to json.  This gives us at least a modicum of
;; programming niceness.

(defgeneric serialize-to-json (object &optional stream))

(defmethod serialize-to-json (object &optional stream)
  (let ((shasht:*symbol-name-function* (alexandria:compose #'string-downcase #'symbol-name)))
    (shasht:write-json object stream)))

(defmethod serialize-to-json ((object sequence) &optional stream)
  (let ((shasht:*symbol-name-function* (alexandria:compose #'string-downcase #'symbol-name)))
    (shasht:write-json object stream)))

(defclass plot-layout ()
  ((margin :accessor margin :initform (make-instance 'margin :t 20) :initarg :margin)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)
   (title :accessor title :initform (make-instance 'title) :type title :initarg :title)
   (xaxis :accessor x-axis :initform (make-instance 'axis) :type axis :initarg :x-axis)
   (yaxis :accessor y-axis :initform (make-instance 'axis) :type axis :initarg :y-axis)
   (annotations :initarg :annotations :accessor annotations :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defun ensure-annotations (plot-layout)
  (check-type plot-layout plot-layout)
  (unless (slot-boundp plot-layout 'annotations)
    (setf (annotations plot-layout) (make-array 0 :adjustable t :fill-pointer 0))))
  
(defun add-annotation (plot-layout annotation)
  (ensure-annotations plot-layout)
  (check-type annotation annotation)
  (vector-push-extend annotation (annotations plot-layout)))

(defun annotation-equal (a b)
  (check-type a annotation)
  (check-type b annotation)
  (and (= (slot-value a 'x) (slot-value b 'x))
       (= (slot-value a 'y) (slot-value b 'y))))

(defun toggle-annotation (plot-layout annotation)
  (ensure-annotations plot-layout)
  (let ((previous (find annotation (annotations plot-layout) :test 'annotation-equal)))
    (if previous
	(let ((new (remove previous (annotations plot-layout))))
	  (setf (annotations plot-layout) (make-array (length new) :adjustable t :fill-pointer t )))
      (add-annotation plot-layout annotation))))

(defclass annotation ()
  ((xref :initarg :xref :initform "x") ;; or "paper"
   (yref :initarg :yref :initform "y")
   (x :initarg :x)
   (y :initarg :y)
   (xanchor :initarg :xanchor ) ;; or "left" "center" "right"
   (yanchor :initarg :yanchor) ;; "top" "center" ?
   (text :initarg :text :initform "annotation")
   (showarrow :initarg :showarrow :initform nil)
   (arrowhead :initarg :arrowhead)
   (ax :initarg :ax)
   (ay :initarg :ay)))

;; Because we use :common-lisp, t is defined as a constant, so we
;; cannot use it as a symbol name which breaks our auto-json
;; serialization for margin which has a "t" slot.  So, we provide a t
;; hidden in the hidden-t package which doesn't use the :common-lisp
;; package.

(defpackage #:hidden-t
  (:export #:t))

(in-package #:plotly)
(defclass margin ()
  ((hidden-t:t :initarg :t)
   (b :initarg :b) (l :initarg :l) (r :initarg :r)))

;; See https://plotly.com/javascript/reference/scatter/
(defclass scatter-trace ()
  ((type :initform "scatter")
   (name :initform "" :accessor trace-name :type string :initarg :name)
   (visible :initform t :accessor trace-visible :type (member t nil "legendonly"))
   (showlegend :initform t :accessor trace-showlegend :type boolean)
   ;;(legendrank :initform 1000 :accessor trace-legendrank :type fixnum)
   ;;(legendgroup :initform "" :accessor trace-legendgroup :type string)
   ;;(legendgrouptitle :initform nil :accessor legendgrouptitle :type legendgrouptitle)
   ;;(legendwidth :initform 0.1 :accessor trace-legendwidth :type (real 0.0)) ;; width fraction or px
   (opacity :initform 1 :accessor trace-opacity :type (real 0.0 1.0))
   (mode :type string :initform "none" :accessor trace-mode) ;; lines, markers, test joined with + or none
   ;; ids
   (x :accessor trace-x :type sequence :initarg :x)
   (y :accessor trace-y :type sequence :initarg :y)
   ;; x0, dx, y0, dy
   (text :accessor trace-text :type sequence :initform nil) ;; strings for each x,y pair
   ;; controlled by hoverinfo and if hovertext is not set
   ;; (textposition :accessor trace-textposition :initform nil)
   ;; either "top left" | "top center" ... or an array of these
   ;; texttemplate
   ;; hovertext
   ;; hoverinfo
   ;; hovertemplate
   ;; xhoverformat
   ;; yhoverformat
   ;; meta
   ;; customdata
   (xaxis :accessor trace-xaxis :type string :initform "x") ;; which axis this is drawn against
   (yaxis :accessor trace-yaxis :type string :initform "y")
   ;;(orientation :accessor trace-orientation :type string...
   ;; (groupnorm
   ;; alignmentgroup
   ;; offsetgroup
   ;; stackgroup
   ;; xperiod, xperiodalignmnet, xperiod0
   ;; yperiod, yperiodalignment, yperiod0
   (marker :accessor trace-marker :type marker)
   (line :accessor trace-line :type line)
   (textfont :accessor trace-textfont :type (or null plotly-font)) ;; for any text
   (error_x :accessor trace-error-x :type (or null error-bar))
   (error_y :accessor trace-error-y :type (or null error-bar))
   ;;selectedpoints selected unselected
   ;; cliponaxis connectgaps
   ;; fill fillcolor fillpattern
   ;; hoverlabel
   ;; hoveron
   ;; stackgaps xcalendar ycalendar uirevision
   ))

(defclass 3d-trace ()
  ((type :initform "scatter3d" :initarg :type)
   (name :initform "" :accessor trace-name :type string :initarg :name)
   (visible :initform t :accessor trace-visible :type (member t nil "legendonly"))
   (showlegend :initform t :accessor trace-showlegend :type boolean)
   (opacity :initform 1 :accessor trace-opacity :type (real 0.0 1.0))
   (mode :type string :initform "markers" :accessor trace-mode :initarg :mode)
   (x :accessor trace-x :type sequence :initarg :x)
   (y :accessor trace-y :type sequence :initarg :y)
   (z :accessor trace-y :type sequence :initarg :z)
   (xaxis :accessor trace-xaxis :type string ;; :initform "x"
	  ) ;; which axis this is drawn against
   (yaxis :accessor trace-yaxis :type string ;; :initform "y"
	  )
   ;;(orientation :accessor trace-orientation :type string...
   ;; (groupnorm
   ;; alignmentgroup
   ;; offsetgroup
   ;; stackgroup
   ;; xperiod, xperiodalignmnet, xperiod0
   ;; yperiod, yperiodalignment, yperiod0
   (marker :accessor trace-marker :type marker :initarg :marker)
   (line :accessor trace-line :type line)
   (textfont :accessor trace-textfont :type (or null plotly-font)) ;; for any text
   (error_x :accessor trace-error-x :type (or null error-bar))
   (error_y :accessor trace-error-y :type (or null error-bar))
   ;;selectedpoints selected unselected
   ;; cliponaxis connectgaps
   ;; fill fillcolor fillpattern
   ;; hoverlabel
   ;; hoveron
   ;; stackgaps xcalendar ycalendar uirevision
   ))  

(defclass error-bar ()
  ((array :accessor error-bar-array :initarg :array)
   (arrayminus :accessor error-bar-arrayminus :initarg :arrayminus)
   (color :accessor color)
   (thickness :accessor thickness)
   (type :initform "data")
   (symmetric :initarg :symmetric)
   (visible :accessor visible)
   (width :type (real 0.0) :accessor width)))

(defclass line ()
  ((color :accessor color :initarg :color)
   (dash :type (or null string) :accessor line-dash :initarg :line-dash)
   ;; shape for splines or steps
   ;; simplify
   ;; smoothing
   (width :type (integer 0) :accessor width :initarg :width)))

(defclass marker ()
  (;;angle
   ;;angleref
   autocolorscale
   cauto
   cmax
   cmid
   cmin
   (color :accessor marker-color :initarg :color) ;; will be a numerical array assigning colors to each point
   coloraxis
   (colorbar :type (or null colorbar) :accessor marker-colorbar)
   colorscale
   ;;gradient
   ;;line
   ;;maxdisplayed
   ;;opacity
   ;;reversescale
   showscale ;; colorbar displayed or not, only used if color is set
   (size :initform 12 :type (or number sequence) :accessor size :initarg :size)  ;; number or array of nums marker size in px
   ;;sizemin sizemode sizeref standoff
   (symbol :type string :accessor marker-symbol :initarg :symbol)
   ))

(defclass colorbar ()
  (bgcolor
   bordercolor
   borderwidth
   dtick
   expoentformat
   labelalias
   len
   lenmode
   minexponent
   nticks
   orientation
   outlinecolor
   outlinewidth
   separatethousands
   showexponent
   showticklabels
   showtickprefix
   showticksuffix
   thickness
   thicknessmode
   tick0
   tickangle
   tickcolor
   (tickfont :initform nil :type (or plotly-font null))
   tickformat
   ;;tickformatstops, ticklabeloverflow ticklabelposition ticklabelstep
   ;; ticklen tickmode tickprefix ticks ticksuffix ticktext tickvals
   (title :type colorbar-title)
   ;; x, y
   ;; xanchor xpad yanchor ypad
   ))

(defclass plotly-font ()
  ((color :initarg :color :type string :initform "black" :accessor color)
   (family :initarg :family :type string :initform "Courier New, monospace" :accessor font-family)
   (size :initarg :size :type integer :initform 14 :accessor size)))

(defclass title ()
  ((font :type plotly-font :accessor title-font :initform (make-instance 'plotly-font :size 14)
	 :initarg :font)
   (text :type string :accessor text :initarg :text)
   (automargin :type boolean :initform t :initarg :automargin)))

(defclass plot-title (title)
  ((xref :initform "paper")
   (x :initform 0.5)
   (yref :initform "paper")
   (y :initform 1.0)
   (yanchor :initform "bottom")
   (font :initform (make-instance 'plotly-font :size 14))))

(defun make-plot-title (text &optional (size 14))
  (make-instance 'plot-title :font (make-instance 'plotly-font :size size) :text text))

(defclass colorbar-title (title)
  (side))

(defclass axis-title (title)
  ((standoff :accessor axis-title-standoff)))

(defclass plot-trace () ())

(defclass plotly-plot ()
  ((traces
    :initarg :traces
    :accessor traces
    :type traces
    :documentation "List of traces on the plot")
   (layout
    :initarg :layout
    :accessor layout
    :type plot-layout
    :initform (make-instance 'plot-layout)
    :documentation "Plot layout")))

(defclass axis ()
  (anchor
   automargin
   autorange
   autotypenumbers
   calendar
   categoryarray
   categoryorder
   color
   constrain
   constraintoward
   dividercolor
   dividerwidth
   domain
   dtick
   exponentformat
   fixedrange
   gridcolor
   griddash
   gridwidth
   hoverformat
   labelalias
   layer ;; "above traces" or "below traces"
   linecolor
   linewidth
   matches
   minexponent
   (minor :initarg :minor)
   mirror
   nticks ;; number of ticks in tickmode auto
   overlaying
   position
   (range :accessor range :initarg :range)
   ;; rangebreaks
   ;; rangemode
   ;; rangeselector
   ;; rangeslider
   ;; scaleanchor
   ;; scaleratio
   ;; separatethousands
   ;; showdividers
   ;; showexponent
   showgrid
   showline
   showspikes
   showticklabels
   showtickprefix
   showticksuffic
   side
   ;; spikecolor
   ;; spikedash
   ;; spikemode
   ;; spikesnap
   ;; spikethickness
   tick0
   tickangle
   tickcolor
   (tickfont :initarg :tickfont)
   tickformat
   tickformatstops
   ticklabelmode
   ticklabeloverflow
   ticklabelposition
   ticklabelstep
   ticklen
   (tickmode :initarg :tickmode) ;; "auto"
   tickprefix
   ticks
   tickson
   ticksuffix
   ticktext
   tickwidth
   (title :type axis-title :initform (make-instance 'axis-title) :accessor title)
   type ;; -, linear, log, date, category, multicategory
   uirevision
   (visible :initarg :visible)
   (zeroline :initarg :zeroline)
   (zerolinecolor :initarg :zerolinecolor)
   (zerolinewidth :initarg :zerolinewidth)))

(defclass minor ()
  (dtick
   gridcolor
   griddash
   gridwidth
   nticks
   showgrid
   tick0
   tickcolor
   ticklen
   tickmode
   ticks
   tickvals
   tickwidth))


(defclass plotly-config ()
  ((editable :initarg :editable)))
