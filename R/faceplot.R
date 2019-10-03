#' @title  Annotate and Plot mesh Face with different colorpalette
#'
#' @aliases visual3d
#'
#' @description
#' an interactive graphing function based on the `rgl` package.
#' it can draw a 3D object with different style and gradient colors.
#'
#' @param mesh
#' a mesh3d object (class qmesh3d or tmesh3d), or for as.mesh3d an object with a method defined.
#'
#' @param value
#' a vector or list.
#' If it is a vector, each of whose elements can be an index or a quantitative value,
#' such as the phenotypic changes under different genotypes.
#' If it is a list, it should be a list of index vector.
#'
#' @param col
#' a vector of colors or a color palette, if it is not given, the default color palette will be used.
#'
#' @param output
#' the output path. the default is NULL.
#' if output is NULL, the plot will not be printed to a file.
#'
#' @return an interactive plot
#'
#'
#' @examples
#' data(refface)
#' ref.m <- refface$ref.m
#' seg.idx<- refface$seg.idx
#' visual3d(ref.m,output = '../vignettes/ref_face')
#'
#' nose.idx<- which(seg.idx==10)
#' visual3d(ref.m,nose.idx,output = '../vignettes/highlight_nose')
#'
#' visual3d(ref.m,seg.idx,col = colorbar(1:10,c('#377EB8', '#4DAF4A','white', '#984EA3', '#FF7F00')))
#'
#' seg.lst<- sapply(unique(seg.idx),function(i)which(i==seg.idx))
#' visual3d(ref.m,seg.lst,col = colorbar(1:10),'../vignettes/seg_lst')
#'
#' nose.m <- Meshextract(ref.m,nose.idx)
#' nose.vol<- Meshvolume(nose.m)
#' visual3d(nose.m,nose.vol)
#' visual3d(nose.m,nose.vol,col = colorRampPalette(c('white','black')))
#'
#' @import rgl
#' @importFrom grDevices colorRampPalette
#'
#' @export
#'
visual3d <- function(mesh, value = NULL, col = NULL, output = NULL) {

    ## Featureplot input: ref.m, index of feature, and color output: plot feature with color
    Featureplot <- function(ref.m, idx, col = "red", alpha = 1, output = NULL) {
        idxtype <- if (length(idx) < ncol(ref.m$vb))
            annofaceidx(ref.m, idx) else idx
        dota <- dotanno(idxtype, cf = colorRampPalette(c("white", col)))
        col_load <- shadeanno(dota, ref.m$it)
        rgl.viewpoint(0, 0, fov = 0)
        shade3d(ref.m, col = col_load, alpha = (col_load == col) * (1 - alpha) + alpha)
        if (!is.null(output)) {
            rgl.postscript(paste0(output, ".pdf"), fmt = "pdf")
            rgl.clear()
        }
    }

    ## Featuresplot for multiple features in one plot input: ref.m, list of index of features, and color
    ## vector for each feature output: plot feature with color
    Featuresplot <- function(ref.m, idx.lst, col, output = NULL) {

        face.seg <- rep("white", ncol(ref.m$vb))
        for (i in 1:length(idx.lst)) {
            face.seg[idx.lst[[i]]] <- col[i]
        }

        col_load <- shadeanno(face.seg, ref.m$it)
        rgl.viewpoint(0, 0, fov = 0)
        shade3d(ref.m, col = col_load)
        if (!is.null(output)) {
            rgl.postscript(paste0(output, ".pdf"), fmt = "pdf")
            rgl.clear()
        }
    }

    ## Heatmap for shade of mesh input: ref.m, value of each shade, and color palette output: heatmap face
    ## and color bar
    shadehm <- function(ref.m, val, col.pal = colorRampPalette(c("blue", "red")), output = NULL) {
        col_load <- rep(colorbar(val, col.pal), each = 3)
        rgl.viewpoint(0, 0, fov = 0)
        shade3d(ref.m, col = col_load)
        if (!is.null(output)) {
            rgl.postscript(paste0(output, ".pdf"), fmt = "pdf")
            rgl.clear()
        }
    }
    if (is.null(value) & is.null(col)) {
        rgl.viewpoint(0, 0, fov = 0)
        shade3d(ref.m, col = "white")
        if (!is.null(output)) {
          rgl.postscript(paste0(output, ".pdf"), fmt = "pdf")
          rgl.clear()
        }
    }
    if (is.list(value)) {
        if (is.null(col))
            col <- colorbar(1:length(value))
        Featuresplot(mesh, value, col, output)
    } else if (length(value) < ncol(mesh$vb)) {
        if (is.null(col))
            col <- "red"
        Featureplot(mesh, value, col, output = output)
    } else if (length(table(value)) <= 10) {
        if (is.null(col))
            col <- colorbar(1:length(table(value)))
        value <- sapply(unique(value), function(i) which(i == value))
        Featuresplot(mesh, value, col, output)
    } else {
        if (is.null(col)) {
            shadehm(mesh, value, output = output)
        } else {
            shadehm(mesh, value, col, output)
        }
    }

}

#' @title  colorpalette generation
#'
#' @aliases colorbar
#'
#' @description
#' The function is used to genrate color palette, which will used for \code{visualed}
#' to render mesh object.
#'
#' @param vec
#' A vector. Each of its elements will be assigned a RGB value from the color palette
#'
#' @param col.pal
#' A list of colors or a colorRampPalette object.
#' The default is \code{colorRampPalette(c('blue','red'))}
#'
#' @param output
#' the output path. the default is NULL.
#' if output is NULL, the color bar will not be printed to a file.
#'
#' @return a vector of colors
#'
#'
#' @examples
#' colorbar(1:10,c('#377EB8', '#4DAF4A','white', '#984EA3', '#FF7F00'))
#'
#' @importFrom grDevices colorRampPalette dev.off pdf
#' @importFrom graphics rect
#'
#' @export
#'
colorbar <- function(vec, col.pal = colorRampPalette(c("blue", "red")), output = NULL) {
    vec.f <- as.factor(vec)
    if (is.function(col.pal)) {
        vec.col <- col.pal(nlevels(vec.f))
    } else {
        vec.col <- colorRampPalette(col.pal)(nlevels(vec.f))
    }

    min.vec <- min(vec)
    max.vec <- max(vec)
    y <- seq(min.vec, max.vec, length.out = nlevels(vec.f))
    if (!is.null(output)) {
        pdf(file = paste0(output, "cbp.pdf"), width = 2, height = 6)
        plot(c(0, 20), c(min.vec, max.vec), type = "n", bty = "n", xaxt = "n", xlab = "", ylab = "")
        rect(0, y[1:(length(y) - 1)], 10, y[2:length(y)], col = vec.col, border = NA)
        dev.off()
    }
    return(vec.col[vec.f])
}


# annotate face by index
annofaceidx <- function(obj, idx) {
    idxtype <- rep(0, ncol(obj$vb))
    idxtype[idx] <- 1
    return(idxtype)
}


# functions that annotate face to different region(levels) by value range or category annotation face by
# value range(bin)
bincut <- function(v, n = 2) {
    brks <- seq(min(v), max(v), length.out = n)
    grps <- cut(v, breaks = brks, include.lowest = TRUE)
    return(grps)
}
binfaceanno <- function(vanno) bincut(vanno, n = 100)

fivepal <- function(value, color = c("#377EB8", "#4DAF4A", "white", "#984EA3", "#FF7F00"), region = NULL,
    cut = 0, parts = 10, output = NULL) {
    new.value <- value - cut
    value.cut <- cut(abs(new.value), parts)
    if (!is.null(region)) {
        new.value[new.value >= region] <- region
        new.value[new.value <= -region] <- -region
        value.cut <- cut(c(region, abs(new.value)), parts)[-1]
    }

    color.up <- colorRampPalette(color[3:5])(parts)
    color.down <- colorRampPalette(color[1:3])(parts)
    color.pal <- rep(color[2], length(value))
    color.pal[new.value >= 0] <- color.up[value.cut[new.value >= 0]]
    # remember to reverse color.down
    color.pal[new.value < 0] <- rev(color.down)[value.cut[new.value < 0]]
    new.value.max <- max(abs(range(new.value)))
    y <- seq(cut - new.value.max, cut + new.value.max, length.out = 2 * parts + 1)
    if (!is.null(output)) {
        pdf(file = paste0(output, "trip.pdf"), width = 2, height = 6)
        plot(c(0, 20), c(cut - new.value.max, cut + new.value.max), type = "n", bty = "n", xaxt = "n", xlab = "",
            ylab = "")
        rect(0, y[1:(length(y) - 1)], 10, y[2:length(y)], col = c(color.down, color.up), border = NA)
        dev.off()
    }
    return(color.pal)
}


## colorbar
tripalette <- function(value, color = c("blue", "white", "red"), region = NULL, cut = 0, parts = 10, output = NULL) {
    new.value <- value - cut
    value.cut <- cut(abs(new.value), parts)
    if (!is.null(region)) {
        new.value[new.value >= region] <- region
        new.value[new.value <= -region] <- -region
        value.cut <- cut(c(region, abs(new.value)), parts)[-1]
    }

    color.up <- colorRampPalette(color[2:3])(parts)
    color.down <- colorRampPalette(color[1:2])(parts)
    color.pal <- rep(color[2], length(value))
    color.pal[new.value >= 0] <- color.up[value.cut[new.value >= 0]]
    # remember to reverse color.down
    color.pal[new.value < 0] <- rev(color.down)[value.cut[new.value < 0]]
    new.value.max <- max(abs(range(new.value)))
    y <- seq(cut - new.value.max, cut + new.value.max, length.out = 2 * parts + 1)
    if (!is.null(output)) {
        pdf(file = paste0(output, "trip.pdf"), width = 2, height = 6)
        plot(c(0, 20), c(cut - new.value.max, cut + new.value.max), type = "n", bty = "n", xaxt = "n", xlab = "",
            ylab = "")
        rect(0, y[1:(length(y) - 1)], 10, y[2:length(y)], col = c(color.down, color.up), border = NA)
        dev.off()
    }
    return(color.pal)
}

colorbin <- function(vec, col.pal = colorRampPalette(c("blue", "red")), region = NULL, parts = 20, output = NULL) {

    vec.cut <- cut(vec, parts)
    min.vec <- min(vec)
    max.vec <- max(vec)
    if (!is.null(region)) {
        vec[vec >= region] <- region
        vec[vec <= -region] <- -region
        vec.cut <- cut(c(-region, region, vec), parts)
        min.vec <- -region
        max.vec <- region
    }
    color.base <- col.pal(parts)
    vec.col <- color.base[vec.cut]

    y <- seq(min.vec, max.vec, length.out = parts + 1)
    if (!is.null(output)) {
        pdf(file = paste0(output, "cbin.pdf"), width = 2, height = 6)
        plot(c(0, 20), c(min.vec, max.vec), type = "n", bty = "n", xaxt = "n", xlab = "", ylab = "")
        rect(0, y[1:(length(y) - 1)], 10, y[2:length(y)], col = color.base, border = NA)
        dev.off()
    }
    return(vec.col)
}

# annotate each dot(point) a color
dotanno <- function(vanno, cf = faceterrain, f = as.factor) {
    catv <- f(vanno)
    colcat <- cf(nlevels(catv))
    return(colcat[catv])
}
# annotate each wire(edge) a color
wireanno <- function(dota, it) {
    return(dota[it])
}
# annotate each shade(surface) a color
shadeanno <- function(dota, it) {
    return(dota[it])
}




