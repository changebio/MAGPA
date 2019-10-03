#' @title  Extract sub mesh
#'
#' @aliases Meshextract
#'
#' @description
#' The function extracts the vertices and edges from the reference tri-mesh based on the index vector
#' and reconstructs the tri-mesh for the vertices and edges which were extracted.
#'
#' @param ref.m
#' a reference tri-mesh of 3D face
#'
#' @param idx
#' the index vector for extraction region
#'
#' @return a tri-mesh of facial region
#'
#'
#' @examples
#' data(refface)
#' ref.m <- refface$ref.m
#' seg.idx<- refface$seg.idx
#' nose.idx<- which(seg.idx==10)
#' nose.m <- Meshextract(ref.m,nose.idx)
#'
#'
#' @export
#'
Meshextract <- function(ref.m, idx) {
    fr.m <- ref.m
    fr.m$vb <- fr.m$vb[, idx]
    it <- apply(fr.m$it, 2, function(x) all(x %in% idx))
    fr.m$it <- fr.m$it[, it]
    fr.m$normals <- fr.m$normals[, idx]
    fr.m$texcoords <- fr.m$texcoords[, idx]
    fr.m$index <- idx
    new.it <- data.frame(index = 1:length(idx), row.names = idx)
    fr.m$it <- matrix(new.it[as.character(fr.m$it), ], nrow = 3)
    fr.m$it.index <- it
    return(fr.m)
}

#' @title  Calculate the volume of a mesh
#'
#' @aliases Meshvolume
#'
#' @description
#' the function calculate the volume of each tetrahedron
#' which was made by tri-mesh and origin of coordinates of the tri-mesh
#'
#' @param m
#' a tri-mesh object
#'
#'
#' @return a vector of the volume of the mesh
#'
#'
#' @examples
#' data(refface)
#' ref.m <- refface$ref.m
#' face.vol <- Meshvolume(ref.m)
#'
#'
#' @export
#'
Meshvolume <- function(m) {
    vtx <- m$vb[1:3, ]
    fv <- m$it
    vol <- Vtxvolume(vtx, fv)
    return(vol)
}


# common facia vtx list operation extract facial region

### PART2 common functions for vtx

# VTX2M input: reference tri-mesh of 3D face, and vtx
Vtx2m <- function(ref.m, vtx) {
    vtx.m <- ref.m
    vtx.m$vb[1:3, ] <- vtx
    return(vtx.m)
}

Vtx2idx <- function(ref.m, vtx) {
    ref.p.idx <- apply(vtx, 2, function(p) which.min(apply(ref.m$vb[1:3, ], 2, function(x) sum(abs(ref.p[i, 
        ] - x)))))
    return(ref.p.idx)
}

euc.dist <- function(v) sqrt(sum(v^2))

Vtxvolume <- function(vtx, fv, norm = NULL) {
    if (is.null(norm)) {
        vol <- sapply(seq(ncol(fv)), function(i) det(vtx[, fv[, i]])/6)
    } else if (norm == "dist") {
        vol <- sapply(seq(ncol(fv)), function(i) {
            p <- vtx[, fv[, i]]
            vol <- det(p) * 10^5/mean(apply(p, 2, euc.dist))^3
            return(vol)
        })
    } else if (norm == "mean") 
        vol <- sapply(seq(ncol(fv)), function(i) {
            p <- vtx[, fv[, i]]
            vol <- det(p) * 10^5/euc.dist(rowMeans(p))^3
            return(vol)
        })
    return(vol)
}


### PART3 common functions for vtx.lst

# Average VTX input list of vtx output the average of the list of vtx e.g ave.vtx<- AverageVTX(vtx.lst)
AverageVtx <- function(vtx.lst) {
    return(Reduce(`+`, vtx.lst)/length(vtx.lst))
}

# Average VTX2M input: reference tri-mesh of 3D face, and a list of vtx output the tri-mesh of the
# average of the list of vtx e.g ave.m<- AverageVtx2m(ref.m,vtx.lst)
AverageVtx2m <- function(ref.m, vtx.lst) {
    return(Vtx2m(ref.m, AverageVtx(vtx.lst)))
}

# extract face region

extractFR2mx <- function(vtx.lst, idx) {
    return(t(sapply(vtx.lst, function(x) as.vector(x[, idx]))))
}

extractFR <- function(vtx.lst, idx) {
    return(lapply(vtx.lst, function(x) x[, idx]))
}

