1.4.5 (12 Nov 2022)
-------------------

- `TwoD.Layout.Grid`: Return `mempty` in `gridCat` and `gridSnake` on
  empty input ([#89](https://github.com/diagrams/diagrams-contrib/pull/89))

- Hackage revisions
    - r1 (3 Dec 2022): allow `linear-1.22`

1.4.4 (12 Nov 2019)
-------------------

- Updated to build on GHC 8.8
- New module `Diagrams.TwoD.Path.IntersectionExtras`

- Hackage revisions (latest revision 7 Nov 2022)
    - r1: allow lens-4.19 and linear-1.21
    - r2: allow
        - base-4.15
        - monoid-extras-0.6
        - diagrams-core-1.5
        - lens-5.0
        - random-1.2
        - QuickCheck-2.14
    - r3: update homepage URL
    - r4: allow
        - `base-4.16`
        - `hashable-1.4`
        - `lens-5.1`
        - `semigroups-0.20`
    - r5: allow `text-2.0`
    - r6: allow `base-4.17`, `lens-5.2`, and test with GHC 9.4
    - r7: allow `MonadRandom-0.6`

1.4.3 (14 May 2018)
-------------------

- New `Semigroup` instance for `Following`
- Allow `base-4.11` and `lens-4.16`
- Drop GHC-7.8 support

1.4.2.1 (9 February 2018)
-------------------------

- Bug fix: remove `Semigroup` instance on `Following` for now, until
  a corresponding instance is added for `Trail'` in `diagrams-lib`.

1.4.2 (4 Feb 2018)
------------------

- New function `Diagrams.TwoD.Layout.Constrained.runLayout` (thanks to
  Sandy Maguire)

1.4.1 (3 July 2017)
---------------------

- allow QuickCheck-2.10
- allow base-4.10 (for GHC-8.2)
- allow MonadRandom-0.5
- export some things inadvertently unexported from Diagrams.TwoD.Grid (#68)

Hackage revisions:
- r1:
    - allow `QuickCheck-2.11`

1.4.0.1 (13 November 2016)
--------------------------

- allow `cubicbezier-0.5`
- allow `HUnit-1.6` (Hackage revision 2, 21 March 2017)

- Revision 2 on Hackage requires `cubicbezier-0.6` (which has some
  relevant bug fixes).

1.4 (2016-10-26)
----------------

* **New features**

    - New modules:

        - `Diagrams.TwoD.Path.LSystem`: module for generating L-system
          fractals

        - `Diagrams.TwoD.Path.Boolean`: boolean operations on paths

        - `Diagrams.Color.XKCD`: Common names for the 949 most common
          RGB monitor colors, as determined by the xkcd color name
          survey.

        - `Diagrams.TwoD.Layout.Constrained`: 2D layout via relative
          constraint specifications

        - `Diagrams.Anchors`: layout via specified anchor points

        - `Diagrams.TwoD.Path.Follow`: monoid for trails which matches
          tangent vectors at endpoints

    - `Diagrams.TwoD.Path.IteratedSubset` is greatly expanded, with
      more tools, examples, and documentation

    - New radial layout algorithm for rose trees, `radialLayout`
      function in `Diagrams.TwoD.Layout.Tree`

    - Code in `Diagrams.TwoD.Apollonian` cleaned up and generalized

* **API changes**

    - `Diagrams.TwoD.Path.Turtle`: pen width is now a `Measure n`
      instead of `n`

1.3.0.12 (31 August 2016)
-------------------------

- allow QuickCheck-2.9

1.3.0.11 (6 June 2016)
----------------------

- allow `base-4.9`
- allow `data-default-0.7`
- allow `data-default-class-0.1`
- test with GHC 8.0.1

1.3.0.10 (1 May 2016)
-----------------------

- allow `lens-4.14`

1.3.0.9 (20 April 2016)
-----------------------

- allow `data-default-0.6`

1.3.0.8 (10 November 2015)
--------------------------

- allow `semigroups-0.18`


1.3.0.7 (28 September 2015)
---------------------------

Fix compilation error with `lens-4.13` under `ghc-7.10`.

1.3.0.6 (17 September 2015)
---------------------------

- allow `lens-4.13`
- allow `linear-1.20`
- allow `semigroups-0.17`

1.3.0.5 (4 September 2015)
--------------------------

- allow `HUnit-1.3`

## [v1.3.0.4](https://github.com/diagrams/diagrams-contrib/tree/v1.3.0.4) (2015-07-19)

[Full Changelog](https://github.com/diagrams/diagrams-contrib/compare/v1.3.0.3...v1.3.0.4)

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
