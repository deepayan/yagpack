\name{yplot}
\alias{yplot}
\title{Create a plot object}
\usage{
yplot(data = environment(), enclos = .GlobalEnv, margin.vars = list(),
  layout = NULL, skip = FALSE, packets = compute.packets(margin.vars, data
  = data, enclos = enclos), panel.vars = NULL, panel = list(),
  relation = list(), alternating = list(), scales = list(x =
  list(tick.number = 5, font = 1, tck = 1, rot = 0), y = list(tick.number = 5,
  font = 1, tck = 1, rot = 0)), switch.axes = FALSE, aspect = NULL,
  xlim = NULL, ylim = NULL, xlab = shared.env$setup$xlab,
  ylab = shared.env$setup$ylab, main = NULL, sub = NULL,
  theme = yagp.theme(), legend = NULL, ...)
}
\arguments{
  \item{data}{The data source, usually a
  \code{"data.frame"} object.}

  \item{enclos}{The enclosing environment.  See
  \code{\link{eval}}.}

  \item{margin.vars}{List of expressions, possibly named,
  resolving to variables that are to be used as
  conditioning or faceting variables.  Usually produced by
  \code{\link{elist}} or \code{\link{margin.terms}}.}

  \item{layout}{Integer vector of length 2 or 3, giving the
  number of (rows, columns) or (rows, column, pages) into
  which the panels are to be arranged.}

  \item{skip}{Logical vector, replicated as necessary,
  giving the positions of panels that are to be skipped.}

  \item{packets}{A list array with margins compatible with
  \code{margin.vars}, with each component giving the
  indices to the full dataset (more generally, a valid
  \code{subset} argument to \code{\link{evaluate}}).}

  \item{panel.vars}{List of named expressions resolving to
  variables that are to be used by panel layers (through
  the names, so the names must match those expected by the
  corresponding layers). Usually produced by
  \code{\link{elist}}.}

  \item{panel}{A \code{"ylayer"} object, which is
  essentially a list of layers.  See \code{\link{ylayer}}
  and \code{\link{"+.ylayer"}}.}

  \item{relation}{List with possible components \code{x}
  and \code{y}, each a character string specifying how
  limits should be combined across panels: \code{"same"}
  for common limits, \code{"free"} for separate per-panel
  limits).}

  \item{alternating}{List with possible components \code{x}
  and \code{y}, each an integer vector, recycled if
  necessary to match rows and columns, specifying how axes
  tick marks should be labeled on a per-row or per-column
  basis: \code{1} means left/bottom, \code{2} means
  right/top, and \code{3} means both.  For example,
  \code{c(1, 2)} will lead to alternating on alternating
  sides.}

  \item{scales}{List with possible components \code{x} and
  \code{y}, with components controlling the details of axis
  annotation such as number of ticks, etc.  Currently
  ignored.}

  \item{switch.axes}{Logical indicating whether the axes
  are to be switched.  This feature works only if supported
  by panel layers.}

  \item{aspect}{The aspect ratio.}

  \item{xlim}{Range for the x-axis.}

  \item{ylim}{Range for the y-axis.}

  \item{xlab}{X-axis label.}

  \item{ylab}{Y-axis label.}

  \item{main}{Main title.}

  \item{sub}{Subtitle.}

  \item{theme}{A theme, typically produced by
  \code{\link{yagp.theme}}.}

  \item{legend}{A legend.}

  \item{...}{Further arguments, stored in the \code{xargs}
  component of the result.}
}
\value{
A list with class \code{c("yagp", "ylayer")} containing the
information passed to the function after some minimal
processing.
}
\description{
High-level function to produce a plot object.
}
\details{
  TBW.
}
\author{
Deepayan Sarkar
}
