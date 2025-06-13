box::use(
  EBImage[abind, channel, colorMode, imageData, otsu],
  stats[median, quantile],
)

#' @export
otsu_threshold <- function(image, segmentation_list, conv_mode, spot_type, otsu_mode) {
  back_thresh <- matrix(nrow = length(segmentation_list), ncol = length(segmentation_list[[1]]))

  if (otsu_mode == "global") {
    if (colorMode(image) > 0) {
      image <- channel(image, conv_mode)
    }
    if (spot_type == "dark") {
      image <- 1 - image
    }
    thr <- otsu(image)
  }

  img_above_bg <- NULL
  for (y in seq_len(nrow(back_thresh))) {
    row_segment <- NULL
    for (x in seq_len(ncol(back_thresh))) {
      segment <- segmentation_list[[y]][[x]]
      if (colorMode(segment) > 0) {
        segment <- channel(segment, conv_mode)
      }
      if (spot_type == "dark") {
        segment <- 1 - segment
      }
      if (otsu_mode == "local") thr <- otsu(segment)
      back_thresh[y, x] <- thr
      signal <- imageData(segment) > back_thresh[y, x]
      row_segment <- abind(row_segment, signal, along = 1)
    }
    img_above_bg <- abind(img_above_bg, row_segment, along = 2)
  }

  # Initialization of matrices
  mean_intensities <- matrix(NA, nrow = nrow(back_thresh), ncol = ncol(back_thresh))
  median_intensities <- matrix(NA, nrow = nrow(back_thresh), ncol = ncol(back_thresh))
  valid_pixels <- matrix(NA, nrow = nrow(back_thresh), ncol = ncol(back_thresh))

  img_after_bc <- NULL
  for (y in seq_len(nrow(back_thresh))) {
    row_segment <- NULL
    for (x in seq_len(ncol(back_thresh))) {
      segment <- segmentation_list[[y]][[x]]
      if (colorMode(segment) > 0) {
        segment <- channel(segment, conv_mode)
      }
      if (spot_type == "dark") {
        segment <- 1 - segment
      }
      signal <- imageData(segment) > back_thresh[y, x]
      corrected_segment <- (segment - back_thresh[y, x]) * signal

      mean_intensities[y, x] <- mean(corrected_segment[signal], na.rm = TRUE)
      median_intensities[y, x] <- median(corrected_segment[signal], na.rm = TRUE)
      valid_pixels[y, x] <- sum(signal) / (dim(signal)[1] * dim(signal)[2])

      row_segment <- abind(row_segment, corrected_segment, along = 1)
    }
    img_after_bc <- abind(img_after_bc, row_segment, along = 2)
  }

  list(
    image_above_bg = img_above_bg,
    image_after_bc = img_after_bc,
    method = "Otsu",
    prob = NULL,
    threshold = back_thresh,
    mean_intensities = mean_intensities,
    median_intensities = median_intensities,
    valid_pixels = valid_pixels
  )
}

#' @export
quan_threshold <- function(segmentation_list, conv_mode, spot_type, quant) {
  nrows <- length(segmentation_list)
  ncols <- length(segmentation_list[[1]])
  back_thresh <- NULL

  for (y in seq_len(nrows)) {
    for (x in seq_len(ncols)) {
      segment <- segmentation_list[[y]][[x]]
      if (colorMode(segment) > 0) {
        segment <- channel(segment, conv_mode)
      }
      if (spot_type == "dark") {
        segment <- 1 - segment
      }
      back_thresh <- c(back_thresh, as.numeric(segment))
    }
  }
  back_thresh <- quantile(back_thresh, probs = quant / 100)

  # Initialization of matrices
  mean_intensities <- matrix(NA, nrow = nrows, ncol = ncols)
  median_intensities <- matrix(NA, nrow = nrows, ncol = ncols)
  valid_pixels <- matrix(NA, nrow = nrows, ncol = ncols)

  img_above_bg <- NULL
  img_after_bc <- NULL
  for (y in seq_len(nrows)) {
    row_segment_1 <- NULL
    row_segment_2 <- NULL
    for (x in seq_len(ncols)) {
      segment <- segmentation_list[[y]][[x]]
      if (colorMode(segment) > 0) {
        segment <- channel(segment, conv_mode)
      }
      if (spot_type == "dark") {
        segment <- 1 - segment
      }
      signal <- imageData(segment) > back_thresh
      corrected_segment <- (segment - back_thresh) * signal

      mean_intensities[y, x] <- mean(corrected_segment[signal], na.rm = TRUE)
      median_intensities[y, x] <- median(corrected_segment[signal], na.rm = TRUE)
      valid_pixels[y, x] <- sum(signal) / (dim(signal)[1] * dim(signal)[2])

      row_segment_1 <- abind(row_segment_1, signal, along = 1)
      row_segment_2 <- abind(row_segment_2, corrected_segment, along = 1)
    }
    img_above_bg <- abind(img_above_bg, row_segment_1, along = 2)
    img_after_bc <- abind(img_after_bc, row_segment_2, along = 2)
  }

  list(
    image_above_bg = img_above_bg,
    image_after_bc = img_after_bc,
    method = "Quantile",
    prob = quant,
    threshold = back_thresh,
    mean_intensities = mean_intensities,
    median_intensities = median_intensities,
    valid_pixels = valid_pixels
  )
}
