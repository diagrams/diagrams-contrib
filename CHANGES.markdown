0.6.0.3: 27 January 2013
------------------------

* Switch to explicit import list for Control.Lens to avoid name conflicts

0.6.0.2: 24 January 2013
------------------------

* Require lens-3.8

0.6.0.1: 17 December 2012
-------------------------

* Update dependencies of tests

0.6: 11 December 2012
---------------------

* **New features**

    - New pure implementation of Turtle library, in `Turtle.Internals`

    - `Diagrams.TwoD.Layout.Tree`: 

	- New `renderTree'` function which gives
	  the edge-drawing function access to the values stored at the
	  nodes instead of just the node positions.

	- The type of `renderTree` is generalized to work with any
	  `QDiagram b R2 m`.

* **Bug fixes**

    - Tiling generation code in `Diagrams.TwoD.Tilings` wasn't actually
      checking whether vertexes had been already visited.
      
* **Dependency/version changes**

    - Switch from `fclabels` to `lens`

0.1.1.1: 13 May 2012
--------------------

* bump `mtl` (< 2.2) and `data-default` (< 0.5) upper bounds

0.1.1.0: 16 March 2012
----------------------

* Add Andrew Kennedy's symmetric rose tree layout algorithm to
    `Diagrams.TwoD.Layout.Tree`

0.1.0.0: 9 March 2012
---------------------

Initial release, containing:

* `Diagrams.Layout.Wrap`, for laying out diagrams "wrapped" inside an
    arbitrary region (Michael Sloan)

* `Diagrams.TwoD.Tilings`, for generating various 2D regular tilings
    (Brent Yorgey)

* `Diagrams.TwoD.Apollonian`, for generating Apollonian gaskets (Brent
    Yorgey)

* `Diagrams.TwoD.Layout.Tree`, tree layout and drawing algorithms
    (Brent Yorgey)

* `Diagrams.TwoD.Path.Turtle`, creation of 2D paths using a stateful
    "turtle" interface (Michael Sloan)
