(asdf:defsystem #:plotly-user
  :description "Generate pretty plots in your browser from the REPL"
  :author "Andrew J. Berkley <ajberkley@gmail.com>"
  :license "BSD 3-clause"
  :version "0.0.1"
  :serial t
  :entry-point "plotly-user:maybe-start-workbench"
  :depends-on (#:clog #:clog-plotly #:alexandria #:shasht)
  :components ((:module "src"
		:components ((:file "plotly")
			     (:file "plotly-user")))
	       (:module "www")))
			
