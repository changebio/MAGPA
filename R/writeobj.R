#' @title  Write a tri-mesh object to a Wavefront OBJ 3D scene file
#'
#' @aliases writeobj
#'
#' @description
#' This function writes a tri-mesh object to a OBJ file, which a file format that is commonly used in 3D graphics
#' applications. It does not represent text, but does represent points, lines, polygons.
#'
#'
#' @param obj.file Path to an OBJ file
#' @param m a tri-mesh object
#'
#' @return
#' invisibly returns the name of the OBJ file to which the data was written.
#'
#' @examples
#' data(refface)
#' ref.m <- refface$ref.m
#' obj.file <- tempfile(fileext = '.obj')
#' writeobj(obj.file, ref.m)
#' ref.m <- readobj(obj.file)
#'
#' @importFrom rgl tmesh3d
#'
#' @export
#'
writeobj <- function(obj.file, m) {
    if (is.character(obj.file)) {
        obj.file <- file(obj.file, "w")
        on.exit(close(obj.file))
    }
    vertices <- t(m$vb)
    normals <- t(m$normals)
    texcoords <- t(m$texcoords)
    triangles <- t(m$it)
    tri1 <- paste(triangles[, 1], triangles[, 1], triangles[, 1], sep = "/")
    tri2 <- paste(triangles[, 2], triangles[, 2], triangles[, 2], sep = "/")
    tri3 <- paste(triangles[, 3], triangles[, 3], triangles[, 3], sep = "/")
    cat(paste("v", vertices[, 1], vertices[, 2], vertices[, 3]), sep = "\n", file = obj.file)
    cat(paste("vn", normals[, 1], normals[, 2], normals[, 3]), sep = "\n", file = obj.file)
    cat(paste("vt", texcoords[, 1], texcoords[, 2]), sep = "\n", file = obj.file)
    cat(paste("f", tri1, tri2, tri3), sep = "\n", file = obj.file)
    return(obj.file)
}
