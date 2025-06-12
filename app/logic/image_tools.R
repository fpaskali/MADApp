box::use(
  EBImage[flip, flop, rotate],
  methods[slot],
)

#' @export
crop_image <- function(image, roi) {
  if (length(dim(image)) == 2)
    image <- image[roi$xmin:roi$xmax, roi$ymin:roi$ymax, drop = FALSE]
  else if (length(dim(image)) == 3)
    image <- image[roi$xmin:roi$xmax, roi$ymin:roi$ymax, , drop = FALSE]
  image
}

#' @export
rotate_fine <- function(image, angle) {
  image <- rotate(image,
                  angle,
                  output.dim = dim(image)[1:2],
                  bg.col = "white")
  image
}

#' @export
rotate_ccw <- function(image) {
  image <- rotate(image,
                  -90,
                  output.dim = dim(image)[1:2],
                  bg.col = "white")
  image
}

#' @export
rotate_cw <- function(image) {
  image <- rotate(image,
                  +90,
                  output.dim = dim(image)[1:2],
                  bg.col = "white")
  image
}

#' @export
flip_hor <- function(image) {
  image <- flop(image)
  image
}

#' @export
flip_ver <- function(image) {
  image <- flip(image)
  image
}
