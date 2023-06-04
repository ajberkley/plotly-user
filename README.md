# plotly-user

This package is just in its infancy.

A Common Lisp package for interactive investigation of data sets and
fits using 2D and 3D plots.  The visual interface is browser based using plotly,
but the intent is the user spends most of their time at the Common
Lisp REPL working with data and generating plots in the browser as
they investigate data.

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