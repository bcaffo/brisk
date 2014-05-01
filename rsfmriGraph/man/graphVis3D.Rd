\name{graphVis3D}
\alias{graphVis3D}
\title{Graph Vis 3D}
\usage{
graphVis3D(graph, roiNames, cutoff = NULL, expansion = 1.2, bend = 0.5,
  lwd = 2, col = "black")
}
\arguments{
  \item{graph}{a graph as a matrix or vector}

  \item{roiNames}{the character list of roiNames}
}
\description{
function creates a graph based on roinames for graph <
cutoff if the cutoff isn't specified it simply assumes a
binary graph assumes that the values are put is as
matrix[upper.tri[matrix]] of a data matrix where the column
and row orders correspond to roiNames bend is how much the
lines bend toward the origin (between 0 and 1) if it's
input as a matrix, then it does this automatically
}
\author{
Brian Caffo
}

