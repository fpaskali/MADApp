box::use(
  utils[read.csv, write.table],
)

box::use(
  app/logic/const[LETTERS_EXT],
)

#' @export
read_labels <- function(datapath, nrows, ncols) {
  labels <- read.csv(datapath, header = FALSE)
  label_matrix <- matrix(0, nrow = nrows, ncol = ncols)

  analyte_names <- NULL
  for (row in seq_len(nrow(labels))){
    pos_str <- strsplit(labels[row, 1], split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE)
    cell <- c(match(pos_str[[1]][1], LETTERS_EXT), as.numeric(pos_str[[1]][2]))
    analyte_name <- labels[row, 2]

    if (!is.na(analyte_name) && analyte_name != "" && cell[2] <= nrows && cell[1] <= ncols) {
      if (analyte_name %in% analyte_names) {
        label_matrix[cell[2], cell[1]] <- match(analyte_name, analyte_names)
      } else {
        analyte_names <- c(analyte_names, analyte_name)
        label_matrix[cell[2], cell[1]] <- length(analyte_names)
      }
    }
  }
  list(
    grid = label_matrix,
    analyte_names = analyte_names
  )
}

#' @export
write_labels <- function(datapath, grid, analytes) {
  labels_df <- data.frame()
  if (!is.null(grid)) {
    for (x in seq_len(grid)) {
      for (y in seq_len(grid)) {
        pos <- grid[y, x]
        analyte_name <- NA
        if (pos != 0) {
          analyte_name <- analytes[grid[y, x]]
        }
        labels_df <- rbind(labels_df, data.frame(pos = paste0(LETTERS_EXT[x], y),
                                                 name = analyte_name))
      }
    }
  }
  write.table(labels_df, datapath, sep = ",", row.names = FALSE, col.names = FALSE)
}
