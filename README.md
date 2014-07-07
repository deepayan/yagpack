imp
===

Imp (named after the small demons of Discworld widely utilized as
painters in iconographs) is yet another graphics package for R.

The tessella package provides a bare-bones low-level graphics API.
This package (imp) implements a high-level front-end using tessella,
with `yplot()` as the primary user-level function.  For those familiar
with `lattice`, the `imp.lattice` package provides wrappers to
`yplot()` with the same names, and at least superficially similar
interfaces, as lattice high-level functions.


Installation
------------

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

