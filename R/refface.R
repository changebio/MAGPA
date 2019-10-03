#' Reference face
#'
#' refface includes two object, a 3D reference face `ref.m` and
#' a label vecter of facial segmentation `seg.idx`.
#'
#' \itemize{
#'   \item ref.m a large mesh3d object
#'   \item seg.idx a vector with 32251 length
#'}
#'
#' @docType data
#' @keywords datasets
#' @name refface
#' @usage data(refface)
#' @examples
#' data(refface)
#' ref.m <- refface$ref.m
#' seg.idx<- refface$seg.idx
#' table(seg.idx)
#' visual3d(ref.m)
NULL
