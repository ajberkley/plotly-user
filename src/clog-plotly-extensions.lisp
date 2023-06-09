(in-package :clog-plotly)
;; code that clog-plotly should have but doesn't yet

;; we want which button, where the nearest point is
;; we can't get clicks on title --- for that we'd need to grab the underlying d3
;; title thing and bind a callback on it.  We also can't get clicks on the x or y
;; axis.  I think just having an editing mode come up when you double click the plot...

;; So, if you single click on a point, we get it and can just turn on an annotation.

(defmethod set-on-plotly-click ((obj clog-plotly-element) handler &key one-time cancel-event)
    (clog::set-event obj "plotly_click"
	       handler
               :call-back-script "+ '{ \"x\":' + data.points[0].x + ', \"y\":' + data.points[0].y + ', \"z\":' + (data.points[0].z || false) + '}'"
               :cancel-event cancel-event
               :one-time     one-time))

(defgeneric new-plot-plotly* (clog-plotly-element json-data json-layout &optional json-config)
  (:documentation "Plot with plotly json-data and json-layout"))

(defmethod new-plot-plotly* ((obj clog-plotly-element) json-data json-layout &optional (json-config ""))
  (js-execute obj (format nil "Plotly.newPlot(~A, ~A, ~A, ~A)"
                          (script-id obj) json-data json-layout json-config)))
