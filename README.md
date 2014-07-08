# imp


Imp (named after the small demons of Discworld widely utilized as
painters in iconographs) is yet another graphics package for R.

The tessella package provides a bare-bones low-level graphics API.
This package (imp) implements a high-level front-end using tessella,
with `yplot()` as the primary user-level function.  For those familiar
with `lattice`, the `imp.lattice` package provides wrappers to
`yplot()` with the same names, and at least superficially similar
interfaces, as lattice high-level functions.


## Installation


```
install.packages("devtools")
library("devtools")

install_github("deepayan/tessella/tessella")
install_github("deepayan/yagpack/imp")
install_github("deepayan/yagpack/imp.lattice")
```

For a backend that does not utilise standard R graphics, you could
also install


```
install_github("ggobi/qtbase")
install_github("ggobi/qtpaint")
install_github("ggobi/qtutils")
install_github("deepayan/tessella/quilt")
```

But this will require a little work (installing cmake and Qt >= 5.2.0)
on Mac/Linux, and is rather more difficult on Windows.


## Long term goals

As an API with multiple backend implementations, the point of
`tessella` is to cleanly separate the graphics API and rendering
backends, and thus to serve as a "code once, run anywhere" target for
graphics programmers.  The goal of the `imp` family of packages is to
provide a high-level framework for both static and dynamic graphics by
building on `tessella`.  R graphics devices provide a tested and
powerful cross-platform solution for static graphics, that can be
leveraged for publication quality static graphics.  Dynamic and
interactive graphs usually need faster, if less polished, backends, as
well as a support framework to enable useful interactions (e.g., a
model-view-control type scheme to enable linked displays) and a
transparent way to handle mouse/keyboard events in a
backend-independent way.  We hope to provide such tools, preferably by
leveraging existing packages like
(plumbr)[https://github.com/ggobi/plumbr] and
(objectSignals)[http://cran.r-project.org/web/packages/objectSignals/].


## Current status

The static graphics API is in useable shape, although much work still
needs to be done.

The dynamic graphics API is still in the planning stages. 


