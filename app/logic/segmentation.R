box::use(
  methods[slot],
)

box::use(
  app/logic/utils[fix_segments, get_lin_fun],
)

#' @export
segment <- function(image, nrows, ncols, grid_mode = "rect") {
  if (grid_mode == "rect") { # Rectangular mode
    colcuts <- seq(1, dim(image)[1], length.out = ncols + 1)
    rowcuts <- seq(1, dim(image)[2], length.out = nrows + 1)

    segmentation_list <- list()

    for (y in 1:nrows) {
      tmp_row <- vector("list", length = ncols)
      for (x in 1:ncols) {
        if (length(dim(image)) == 2) {
          segment <- image[colcuts[x]:colcuts[x + 1], rowcuts[y]:rowcuts[y + 1]]
        }
        if (length(dim(image)) == 3) {
          segment <- image[colcuts[x]:colcuts[x + 1], rowcuts[y]:rowcuts[y + 1], ]
        }
        tmp_row[[x]] <- segment
      }
      segmentation_list[[y]] <- tmp_row
    }
    fix_segments(segmentation_list)
  } else if (grid_mode == "par") { # Parallelogram mode
    colcuts <- seq(1, dim(image)[1], length.out = ncols + 1)
    rowcuts <- seq(1, dim(image)[2], length.out = nrows + 2)

    segmentation_list <- vector("list", length = nrows)

    for (y in 1:nrows){
      tmp_row <- vector("list", length = ncols)

      if (length(dim(image)) == 2)
        segment <- image[seq_len(dim(image)[1]), rowcuts[y]:rowcuts[y + 2]]
      if (length(dim(image)) == 3)
        segment <- image[seq_len(dim(image)[1]), rowcuts[y]:rowcuts[y + 2], ]

      # Computing the linear function for top and bottom border of the row.
      lin_fun_1 <- get_lin_fun(1, dim(segment)[2] / 2, dim(segment)[1], 1)

      # Removing out of bound pixels
      x_px <- seq_len(dim(segment)[1])
      y_px <- lin_fun_1(x_px)

      for (i in x_px) {
        if (length(dim(segment)) == 2) {
          segment[i, 1:y_px[i]] <- 1
          segment[i, dim(segment)[2]:(dim(segment)[2] - rev(y_px)[i])] <- 1
        } else if (length(dim(segment)) == 3) {
          segment[i, 1:y_px[i], ] <- 1
          segment[i, dim(segment)[2]:(dim(segment)[2] - rev(y_px)[i]), ] <- 1
        }
      }

      # Computing the cutpoints in the row
      row_cutpoints <- seq(1, dim(segment)[1], length.out = ncols + 1)
      w <- row_cutpoints[2] - row_cutpoints[1]
      h <- dim(segment)[2] - lin_fun_1(row_cutpoints[2])
      if (length(dim(segment)) == 3) {
        # Extracting spots from the row
        for (i in 1:(length(row_cutpoints) - 1)){
          sub_img <- segment[row_cutpoints[i]:(row_cutpoints[i] + w), , ]
          # Reducing the NAs by cutting top and bottom empty parts
          y_cut <- lin_fun_1(row_cutpoints[i + 1])
          sub_img <- sub_img[, y_cut:(y_cut + h), ]
          tmp_row[[i]] <- sub_img
        }
      } else if (length(dim(segment)) == 2) {
        # Extracting spots from the row
        for (i in 1:(length(row_cutpoints) - 1)){
          sub_img <- segment[row_cutpoints[i]:(row_cutpoints[i] + w), ]
          # Reducing the NAs by cutting top and bottom empty parts
          y_cut <- lin_fun_1(row_cutpoints[i + 1])
          sub_img <- sub_img[, y_cut:(y_cut + h)]
          tmp_row[[i]] <- sub_img
        }
      }
      # Add the row to segmentation list
      segmentation_list[[y]] <- tmp_row
    }
    fix_segments(segmentation_list)
  }
}
