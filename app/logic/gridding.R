box::use(
  graphics[lines, text],
)

box::use(
  app/logic/const[LETTERS_EXT],
  app/logic/utils[get_lin_fun],
)

#' @export
render_grid <- function(image, nrows, ncols, roi = NULL, grid_mode = "rect", analyte_grid = NULL,
                        analyte_names = NULL, selected_analyte = "", fp_grid = NULL, id_list = NULL,
                        show_checkerboard = FALSE, show_an = FALSE, show_fp = FALSE,
                        show_id = FALSE) {

  if (grid_mode == "rect") { # Rectangular mode
    if (is.null(roi)) {
      colcuts <- seq(1, dim(image)[1], length.out = ncols + 1)
      rowcuts <- seq(1, dim(image)[2], length.out = nrows + 1)
    } else {
      colcuts <- seq(roi$xmin, roi$xmax, length.out = ncols + 1)
      rowcuts <- seq(roi$ymin, roi$ymax, length.out = nrows + 1)
    }
    for (x in colcuts) {
      lines(x = rep(x, 2), y = c(min(rowcuts), max(rowcuts)), col = "red")
    }
    for (y in rowcuts) {
      lines(x = c(min(colcuts), max(colcuts)), y = rep(y, 2), col = "red")
    }
  }

  if (grid_mode == "par") { # Parallelogram mode
    if (is.null(roi)) {
      colcuts <- seq(1, dim(image)[1], length.out = ncols + 1)
      rowcuts <- seq(1, dim(image)[2], length.out = nrows + 2)
    } else {
      colcuts <- seq(roi$xmin, roi$xmax, length.out = ncols + 1)
      rowcuts <- seq(roi$ymin, roi$ymax, length.out = nrows + 2)
    }

    lin_fun_1 <- get_lin_fun(min(colcuts), rowcuts[2], max(colcuts), rowcuts[1])
    lin_fun_2 <- get_lin_fun(min(colcuts), rowcuts[length(rowcuts)],
                             max(colcuts), rowcuts[length(rowcuts) - 1])

    for (x in colcuts) {
      ymin <- lin_fun_1(x)
      ymax <- lin_fun_2(x)
      lines(x = rep(x, 2), y = c(ymin, ymax), col = "red")
    }

    for (i in seq_along(rowcuts)) {
      lines(x = c(min(colcuts), max(colcuts)), y = c(rowcuts[i + 1], rowcuts[i]), col = "red")
    }
  }

  if (!is.null(analyte_grid)) {
    for (cell_y in seq_len(nrows)) {
      for (cell_x in seq_len(ncols)) {
        # Annotation
        analyte_id <- analyte_grid[cell_y, cell_x]
        anno_adj <- c(-0.1, 1.3)
        anno_col <- "red"
        anno_y <- rowcuts[cell_y]

        if (grid_mode == "par")
          anno_y <- get_lin_fun(colcuts[1],
                                rowcuts[cell_y + 1],
                                colcuts[length(colcuts)],
                                rowcuts[cell_y])(colcuts[cell_x])

        if (!is.null(analyte_names) && analyte_id != 0 &&
              analyte_names[analyte_id] == selected_analyte)
          anno_col <- "green"

        if (show_an && !is.null(analyte_names) && analyte_id != 0) {
          text(x = colcuts[cell_x], y = anno_y, adj = anno_adj,
               label = "✱", col = anno_col, cex = 0.8)
        } else if (!is.null(analyte_names) && show_id && !is.null(id_list)) {
          if (analyte_names[analyte_id] %in% id_list$empty_cells) {
            text(x = colcuts[cell_x], y = anno_y, adj = anno_adj,
                 label = "E", col = "darkblue")
          }
          if (analyte_names[analyte_id] %in% id_list$control_cells) {
            text(x = colcuts[cell_x], y = anno_y, adj = anno_adj,
                 label = "C", col = "darkgreen")
          }
        } else if (show_fp && !is.null(fp_grid) && fp_grid[cell_y, cell_x] == 1) {
          text(x = colcuts[cell_x], y = anno_y, adj = anno_adj,
               label = "✱", col = "purple", cex = 0.8)
        }
      }
    }
  }
  if (show_checkerboard) {
    for (cell_x in seq_len(ncols)) {
      lbl_y <- rowcuts[1]
      if (grid_mode == "par") {
        lbl_y <- get_lin_fun(colcuts[1], rowcuts[2],
                             colcuts[length(colcuts)], rowcuts[1])(colcuts[cell_x])
      }
      text(x = colcuts[cell_x], y = lbl_y, adj = c(-0.5, -1), label = LETTERS_EXT[cell_x],
           col = "red")
    }
    for (cell_y in seq_len(nrows)) {
      lbl_y <- rowcuts[cell_y]
      if (grid_mode == "par") lbl_y <- rowcuts[cell_y + 1]
      text(x = colcuts[1], y = lbl_y, adj = c(1.5, 1.5), label = cell_y, col = "red")
    }
  }
}

#' @export
get_one_cell <- function(plot_click, roi, grid_mode = "rect") {
  p <- plot_click

  if (!p$x > dim(roi$image)[1] && !p$y > dim(roi$image)[2] && p$x > 0 && p$y > 0) {
    if (grid_mode == "rect") {
      cell_x <- ceiling(p$x / roi$cell_w)
      cell_y <- ceiling(p$y / roi$cell_h)
    }

    if (grid_mode == "par") {
      colcuts <- seq(0, dim(roi$image)[1], length.out = roi$ncols + 1)
      rowcuts <- seq(0, dim(roi$image)[2], length.out = roi$nrows + 2)

      ymin <- get_lin_fun(colcuts[1], rowcuts[2], colcuts[length(colcuts)], rowcuts[1])(p$x)
      cell_h <- rowcuts[2] - rowcuts[1]

      cell_x <- ceiling(p$x / roi$cell_w)
      cell_y <- ceiling((p$y - ymin) / cell_h)
    }

    list(
      x = cell_x,
      y = cell_y
    )
  }
}

#' @export
get_multiple_cells <- function(plot_brush, roi, grid_mode = "rect") {
  p <- plot_brush

  if (p$xmax > dim(roi$image)[1]) p$xmax <- dim(roi$image)[1]
  if (p$xmin < 0) p$xmin <- 1
  if (p$ymax > dim(roi$image)[2]) p$ymax <- dim(roi$image)[2]
  if (p$ymin < 0) p$ymin <- 1

  if (grid_mode == "rect") {
    cell_x1 <- ceiling(p$xmin / roi$cell_w)
    cell_x2 <- ceiling(p$xmax / roi$cell_w)
    cell_y1 <- ceiling(p$ymin / roi$cell_h)
    cell_y2 <- ceiling(p$ymax / roi$cell_h)
  }

  if (grid_mode == "par") {
    colcuts <- seq(0, dim(roi$image)[1], length.out = roi$ncols + 1)
    rowcuts <- seq(0, dim(roi$image)[2], length.out = roi$nrows + 2)

    ymin1 <- get_lin_fun(colcuts[1], rowcuts[2], colcuts[length(colcuts)], rowcuts[1])(p$xmin)
    ymin2 <- get_lin_fun(colcuts[1], rowcuts[2], colcuts[length(colcuts)], rowcuts[1])(p$xmax)
    cell_h <- rowcuts[2] - rowcuts[1]

    cell_x1 <- ceiling(p$xmin / roi$cell_w)
    cell_x2 <- ceiling(p$xmax / roi$cell_w)
    cell_y1 <- ceiling((p$ymin - ymin1) / cell_h)
    cell_y2 <- ceiling((p$ymax - ymin2) / cell_h)
  }

  if (cell_y1 > roi$nrows) cell_y1 <- roi$nrows
  if (cell_y2 > roi$nrows) cell_y2 <- roi$nrows
  if (cell_x1 > roi$ncols) cell_x1 <- roi$ncols
  if (cell_x2 > roi$ncols) cell_x2 <- roi$ncols

  list(
    xmin = cell_x1,
    xmax = cell_x2,
    ymin = cell_y1,
    ymax = cell_y2
  )
}
