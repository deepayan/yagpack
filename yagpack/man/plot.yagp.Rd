\name{plot.yagp}
\alias{plot.yagp}
\alias{print.yagp}
\alias{summary.yagp}
\alias{dim.yagp}
\alias{dimnames.yagp}
\title{Plot yagp objects}
\usage{
\method{plot}{yagp}(x, page = 1, primitives = NULL, ..., new = TRUE,
  draw_labels = TRUE, draw_panels = TRUE, draw_strips = TRUE,
  draw_axes = TRUE, position = list(x = 0, y = 0, h = 1, w = 1, split =
  NULL))
\method{print}{yagp}(x, ...)
\method{summary}{yagp}(object, ...)
\method{dim}{yagp}(x)
\method{dimnames}{yagp}(x)
}
\arguments{
  \item{x, object}{An object of class \code{"yagp"}.}

  \item{page}{A vector specifying which page(s) to draw,
  for multi-page plots.}

  \item{primitives}{An environment containing the
  primitives to use, or a function that returns such an
  environment.  By default, \code{.yagpenv$backend} is
  used.}

  \item{...}{Further arguments, passed on to
  \code{\link{yagp_page}}.}

  \item{new}{Logical flag to control whether the plot
  should start a new page.}

  \item{draw_labels}{Logical flag to control whether labels
  are to be drawn.}

  \item{draw_panels}{Logical flag to control whether panels
  are to be drawn.}

  \item{draw_strips}{Logical flag to control whether strips
  are to be drawn.}

  \item{draw_axes}{Logical flag to control whether axes are
  to be drawn.}

  \item{position}{A list specifying the position of a
  viewport inside the main figure region (root viewport)
  where the object is to be drawn.  The viewport is most
  generally specified in terms of the location of the
  top-left corner (\code{x}, \code{y}) and width (\code{w})
  and height (\code{h}) assuming a coordinate system with
  range [0, 1] on both axes.  Alternatively, \code{split}
  specified in the form \code{c(row, column, nrow, ncol)}
  can be used to specify the position as a cell in a
  matrix.}
}
\description{
  Plot method for yagp objects.
}
\details{
  TBW.
}
\author{
Deepayan Sarkar
}
