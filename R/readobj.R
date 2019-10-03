#' @title  Read a Wavefront OBJ 3D scene file into a tri-mesh
#'
#' @aliases readobj
#'
#' @description
#' This function reads a OBJ file, which a file format that is commonly used in 3D graphics
#' applications. It does not represent text, but does represent points, lines, polygons.
#'
#'
#' @param obj.file Path to an OBJ file
#' @param mesh boolean, construct tri-mesh? the default is TRUE
#'
#' @return  a named list with items \code{shapes} and \code{materials},
#'   each containing sublists with one entry
#'   per object (shapes) or material (materials).
#'   Objects in the \code{shapes} list have the following structure \itemize{
#'
#'   \item positions 3xN set of 3D vertices
#'
#'   \item normals 3xN set of normal directions for each vertex (has 3 rows and
#'   0 cols when normals are not available)
#'
#'   \item texcoords vector containing unprocessed texture coordinates
#'
#'   \item indices 3/4xM set of indices into vertex array (trimesh/quadmesh)
#'   0-indexed
#'
#'   \item material_ids (0-indexed, -1 when not set) }
#'
#' @examples
#' data(refface)
#' ref.m <- refface$ref.m
#' obj.file <- tempfile(fileext = '.obj')
#' writeobj(obj.file, ref.m)
#' ref.m <- readobj(obj.file)
#'
#' @importFrom rgl tmesh3d
#' @importFrom utils read.table
#'
#' @export
#'
readobj <- function(obj.file, mesh = TRUE) {
    obj <- read.obj(obj.file)
    if (mesh) {
        m <- tmesh3d(obj$v, obj$f, homogeneous = FALSE, normals = if (is.null(obj$vn))
            NULL else obj$vn, texcoords = if (is.null(obj$vt))
            NULL else obj$vt)
        m$index <- 1:ncol(m$vb)
        return(m)
    }
    return(obj)
}


# convert individual tiny shape into an rgl mesh3d object
read.mesh <- function(obj.file, idx = NULL, homogeneous = FALSE, material = NULL, ref = ref.m) {
    obj <- read.obj(obj.file)
    if (!identical(obj$f, ref$it))
        print("Warning: the obj doesn't match with ref obj ")
    # nb normals are expected to be 4 component in some places
    m <- tmesh3d(obj$v, obj$f, homogeneous = homogeneous, normals = if (is.null(obj$vn))
        NULL else obj$vn, texcoords = if (is.null(obj$vt))
        NULL else obj$vt)
    m$index <- 1:ncol(m$vb)
    return(m)
}

read.mesh2 <- function(vtx.file, ref = ref.m, homogeneous = FALSE, material = NULL, normals = NULL, texcoords = NULL) {
    vertices <- read.vtx(vtx.file)
    if (homogeneous == TRUE)
        vrows <- 4 else vrows <- 3
    object <- list(vb = matrix(vertices, nrow = vrows), it = ref$it, primitivetype = "triangle", material = material,
        normals = normals, texcoords = texcoords)
    if (!homogeneous)
        object$vb <- rbind(object$vb, 1)
    object$index <- 1:ncol(object$vb)
    class(object) <- c("mesh3d", "shape3d")
    return(object)
}



get.coord <- function(string.coord, if3 = FALSE) {
    z <- unlist(strsplit(string.coord, " "))[-1]
    if (if3) {
        z <- unlist(strsplit(z, "/"))
    }
    return(as.numeric(z))
}

read.obj <- function(obj.file) {
    obj.lines <- readLines(obj.file)
    obj.cv <- obj.lines[grep("^v .*", obj.lines)]
    obj.cvn <- obj.lines[grep("^vn .*", obj.lines)]
    obj.cvt <- obj.lines[grep("^vt .*", obj.lines)]
    obj.cf <- obj.lines[grep("^f .*", obj.lines)]
    obj.v <- do.call(cbind, lapply(obj.cv, get.coord))
    obj.vn <- do.call(rbind, lapply(obj.cvn, get.coord))
    obj.vt <- do.call(rbind, lapply(obj.cvt, get.coord))
    obj.fm <- do.call(cbind, lapply(obj.cf, get.coord, if3 = TRUE))
    obj.f <- obj.fm[c(1, 4, 7), ]
    obj.ft <- obj.fm[c(2, 5, 8), ]
    obj.fn <- obj.fm[c(3, 6, 9), ]
    return(list(v = obj.v, vn = obj.vn, vt = obj.vt, f = obj.f, fn = obj.fn, ft = obj.ft))
}

read.inf <- function(inf.file) {
    return(t(suppressWarnings(read.table(inf.file, row.names = 1, col.names = c("x", "y", "z")))))
}

read.vtx <- function(vtx.file) {
    return(t(read.table(vtx.file, col.names = c("x", "y", "z"))))
}
