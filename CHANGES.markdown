[v1.3.0.3](https://github.com/diagrams/diagrams-contrib/tree/v1.3.0.3) (26 May 2015)
-----------------------------------------------------------------------------------

- allow `lens-4.11`

1.3.0.2 (12 May 2015)
---------------------

- allow `MonadRandom-0.4`

1.3.0.1 (29 April 2015)
-----------------------

- allow `QuickCheck-2.8` in test suite

1.3.0.0 (19 April 2015)
-----------------------

* **New features**

    - Generalized `Diagrams.TwoD.IteratedSubset` and added more examples.
    - New module `Diagrams.TwoD.Layout.Grid`.

* **API changes**

    - `Diagrams.Lens` has had a lot of its lenses removed because
      they're either invalid lenses or are now in diagrams-lib.

* **Dependency/version changes**

    - Allow `diagrams-core-1.3`
    - Allow `diarams-lib-1.3`
    - use `linear` instead of `vector-space`

1.1.2.6 (2 April 2015)
----------------------

 - allow `lens-4.9`
 - allow `vector-space-0.10`

1.1.2.5 (13 Jan 2015)
---------------------

- Allow `vector-space-0.9`
- Allow `lens-4.7`

1.1.2.4 (04 Dec 2014)
---------------------

- Allow `semigroups-0.16`

1.1.2.3 (17 November 2014)
--------------------------

- Allow `lens-4.6`

1.1.2.2 (12 Sept 2014)
----------------------

  - Allow `text-1.2`
  - Allow `MonadRandom 0.3`

1.1.2.1 (22 August 2014)
------------------------

- Allow `lens-4.4`

1.1.2 (28 May 2014)
-------------------

* **New features**

    - New module `Diagrams.TwoD.Grid`, for making grids.
      
* **Dependency/version changes**

    - Allow `diagrams-core-1.2`
    - Allow `diarams-lib-1.2`
    - Allow `semigroups-0.15`
    - Allow `lens-4.2`
    - Allow `mtl-2.2`
    
1.1.1.5 (15 May 2014)
------------------------

    - Allow `semigroups-0.14`

1.1.1.4 (10 April 2014)
----------------------

    - Allow `semigroups-0.13`

1.1.1.2 (19 March 2014)
----------------------

  - Allow `lens-4.1`

1.1.1.1 (9 March 2014)
----------------------

    - Allow `vector-space-points-0.2`

1.1.1 (8 March 2014)
--------------------

    - require `diagrams-core-1.1` and `diagrams-lib-1.1`

1.1.0.1 (15 January 2014)
-------------------------

    - allow text-1.1

1.1 (1 January 2014)
--------------------

    - Switch to using lens for `SymmLayoutOpts` and `ForceLayoutTreeOpts`
    - Require `force-layout-0.3`

1.0.0.1 (28 November 2013)
--------------------------

    - Allow semigroups-0.12

1.0: 25 November 2013
---------------------

* **New features**

    - New module `Diagrams.TwoD.Sunburst`, for drawing sunburst
      charts.
    - New module `Diagrams.TwoD.Path.Metafont`, for specifying
      trails/paths using an API inspired by Metafont.  Like
      `cubicSpline` but gives you control over things like the
      tension/curvature or the tangent at a given point.
    - New module `Diagrams.TwoD.Path.Calligraphic` for making simple
      "calligraphic" strokes.
    - New module `Diagrams.Lens` with lenses for working with
      diagrams.

* **Bug fixes**

    - `Diagrams.TwoD.Factorization.primeLayout` has been rewritten to
      avoid iterated transformations, giving a significant performance
      boost.

0.7: 9 August 2013
------------------

* **New features**

    - New module `Diagrams.Color.HSV` with an `hsvBlend` function for
      blending colors in HSV space.

    - Diagrams logo code is now in `Diagrams.Example.Logo`.

    - New symmetric layout algorithm for binary trees in
      `Diagrams.TwoD.Layout.Tree`.

* **Bug fixes**

    - Fix a bug in `Diagrams.TwoD.Path.Turtle` which sometimes caused
      it to output a doubled path (#13).

0.6.1 (23 March 2013)
---------------------

* **New features**

    - New `Diagrams.TwoD.Path.IteratedSubset` module, for constructing
      paths using an "iterated subset" algorithm (repeatedly replacing
      segments with a given path).

    - New `Diagrams.TwoD.Layout.CirclePacking` module for
      circle-packing layout

    - New `Diagrams.TwoD.Factorization` module, for creating
      "factorization diagrams" as seen at
      http://mathlesstraveled.com/2012/11/05/more-factorization-diagrams/
      and on the cover of Hacker Monthly
      (http://mathlesstraveled.com/2012/10/05/factorization-diagrams/).

    - `Diagrams.TwoD.Path.Turtle`: generalize `runTurtle` function,
      and add new functions `drawTurtle` and `sketchTurtle`.
      `drawTurtle` results in a diagram (like the old `runTurtle`),
      and `sketchTurtle` yields a path (ignoring pen style commands).

* **Documentation**

    - Added lots of example images using `diagrams-haddock`

0.6.0.4 (19 March 2013)
-----------------------

* bump upper bound to allow QuickCheck-2.6

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
