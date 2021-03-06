% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeobj.R
\name{writeobj}
\alias{writeobj}
\title{Write a tri-mesh object to a Wavefront OBJ 3D scene file}
\usage{
writeobj(obj.file, m)
}
\arguments{
\item{obj.file}{Path to an OBJ file}

\item{m}{a tri-mesh object}
}
\value{
invisibly returns the name of the OBJ file to which the data was written.
}
\description{
This function writes a tri-mesh object to a OBJ file, which a file format that is commonly used in 3D graphics
applications. It does not represent text, but does represent points, lines, polygons.
}
\examples{
data(refface)
ref.m <- refface$ref.m
obj.file <- tempfile(fileext = '.obj')
writeobj(obj.file, ref.m)
ref.m <- readobj(obj.file)

}
