# plotly-user

This package is just in its infancy.

A Common Lisp package for interactive investigation of data sets and
fits using 2D and 3D plots.  The visual interface is browser based using plotly,
but the intent is the user spends most of their time at the Common
Lisp REPL working with data and generating plots in the browser as
they investigate data.

## Installing
```
cd ~/quicklisp/local-projects
git clone https://github.com/ajberkley/plotly-user.git
git clone https://github.com/rabbibotton/clog-plotly.git
git clone https://github.com/rabbibotton/clog.git
```

```
CL-USER> (quicklisp:quickload "PLOTLY-USER")
CL-USER> (in-package "PLOTLY-USER")
PLOTLY-USER> (maybe-start-workbench)
PLOTLY-USER>
 (plot-data
  (loop for x from -10 below 10
        collect (list (make-uncertain-number (+ x (- (random 1.0) 0.5))
					    :s+ (random 0.5d0) :s- (random 0.5d0))
		     (make-uncertain-number (+ x (- (random 1d0) 0.5))
					    :s+ (random 0.5d0) :s- (random 0.5d0))))
  :x-axis-label "$\\text{Time }(\\mu\\text{s})$" :y-axis-label "$\\sqrt{signal}$"
  :title "A plot" :line "solid" :marker "cross" :color "red" :legend '("my trace name"))
PLOTLY-USER> (figure 1)
PLOTLY-USER> (let (data)
	       (loop for x from -0.3 below 0.3 by 0.05
		   do
		      (loop for y from -0.3 below 0.3 by 0.05
			    do
			       (push (list x y (* (cos (* 2 pi x)) (cos (* 2 pi y)))) data)))
	       (scatter3d (mapcar #'first data)
			  (mapcar #'second data)
			  (mapcar #'third data)))
```



## Main dependencies
[CLOG](https://github.com/rabbibotton/clog): This provides the browser based user interface and is a great framework for quick development of such things.

[SHASHT](https://github.com/yitzchak/shasht): We talk back and forth to the browser through JSON, so need a JSON encoder.  This one worked easily.

[PLOTLY](https://plotly.com/javascript/): The javascript plotly library is the visualization library.

Alexandria: the main collection of lisp niceties.

## Intended interactions

It would be good to support the data-frame used by
[Lisp-Stat](https://github.com/Lisp-Stat).  It would be nice to make
this package work nicely with Lisp-Stat, which uses Vega for the
current plotting which does not handle 3D data sets in a useful way.
