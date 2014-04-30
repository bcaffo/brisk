\name{graphVis}
\alias{graphVis}
\title{Graph Vis}
\usage{
graphVis(graph, roiNames, cutoff = NULL, expansion = 1.2, bend = 0.5,
  lwd = 2, col = "black")
}
\arguments{
  \item{graph}{the graph either sent as a matrix or a
  vector}

  \item{roiNames}{a character vector of roiNames}

  \item{expansion}{how far the label names are from the
  nodes}

  \item{bend}{how bendy the lines connecting nodes are}

  \item{lwd}{line width}

  \item{col}{node and line color}
}
\description{
This is a sort of low brow graph visulation function that
plots on a circle
}
\details{
function creates a graph based on roinames for graph <
cutoff assumes that the values are put is as
matrix[upper.tri[matrix]] of a data matrix where the column
and row orders correspond to roiNames bend is how much the
lines bend toward the origin (between 0 and 1) if this
isn't the case
}

