This package is a repository for user contributions to the diagrams
project (http://projects.haskell.org/diagrams): generation of
specialized diagrams, fun or instructive examples, half-baked ideas,
stuff which is not sufficiently polished or general to go in the
diagrams-lib package but is nonetheless worth sharing.  Any code is
welcome, as long as it conforms to a few simple standards:

  - Code must be released under a BSD3 license (see the LICENSE).

  - You must list yourself as the maintainer.

  - Try to keep external dependencies to a minimum; the goal is for
    diagrams-contrib to be easily installable by as many people as
    possible.  New dependencies will be considered on a case-by-case
    basis.  Dependencies involving the FFI will most likely be
    rejected.  If you have some cool code using diagrams which
    requires big external dependencies, you should release it as a
    separate package rather than including it in diagrams-contrib.

  - There should at minimum be a Haddock comment on the module itself,
    explaining the purpose, giving some examples of use, etc.

  - It must compile with no warnings under -Wall. This may seem a bit
    draconian, but you'll get over it.  If it makes you feel any
    better, you are welcome to turn off specific warnings for your
    module with an {-# OPTIONS_GHC -fno-warn-blah #-} pragma.
