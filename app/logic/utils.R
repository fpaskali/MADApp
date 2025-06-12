box::use(
  methods[slot],
  stats[quantile],
)

# Calculate linear function by given points
#' @export
get_lin_fun <- function(x1, y1, x2, y2) {
  m <- (y2 - y1) / (x2 - x1)
  b <- y1 - m * x1

  lin_fun <- function(x) {
    y <- NULL
    if (length(x) != 1) {
      for (i in x) {
        y <- c(y, m * i + b)
      }
    } else {
      y <- m * x + b
    }
    y
  }
}

# Fix the image size. It is required sometimes due to rounding errors when binding
# subimages into an whole image.
#' @export
fix_segments <- function(segmentation_list) {
  min_h <- Inf
  min_w <- Inf

  for (row in segmentation_list) {
    for (i in row) {
      if (min_h > dim(i)[1]) min_h <- dim(i)[1]
      if (min_w > dim(i)[2]) min_w <- dim(i)[2]
    }
  }

  for (row in seq_along(segmentation_list)) {
    for (i in seq_along(segmentation_list[[row]])) {
      if (length(dim(segmentation_list[[row]][[i]])) == 3) {
        segmentation_list[[row]][[i]] <- segmentation_list[[row]][[i]][1:min_h, 1:min_w, ]
      } else {
        segmentation_list[[row]][[i]] <- segmentation_list[[row]][[i]][1:min_h, 1:min_w]
      }
    }
  }
  segmentation_list
}

#' @export
quant_minmax_scaling <- function(intensities, grid, analyte_names, empty_analytes, control_analytes,
                                 high_quant = 0.9, low_quant = 0.1) {
  inten<- intensities
  MAX <- quantile(inten[grid %in% match(control_analytes, analyte_names)], low_quant, na.rm = TRUE)
  MIN <- quantile(inten[grid %in% match(empty_analytes, analyte_names)], high_quant, na.rm = TRUE)

  if (!is.na(MAX) && !is.na(MIN)) {
    inten <- (inten - MIN) / MAX
    inten[inten < 0] <- NA
    inten[inten > 1] <- 1
    inten
  } else {
    NULL
  }
}
