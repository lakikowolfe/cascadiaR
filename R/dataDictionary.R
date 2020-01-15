#' A function to build simple data dictionaries from a dataframe.
#'
#' This function allows you to input a dataframe and outputs
#' simple descriptors of each field in that dataframe.
#' @param IDcols Are columns in this dataframe used for identification? Defaults to NULL.
#' @param outdir Directory to output a csv of the data dictionary to. If left NULL nothing will save out
#' @keywords data dictionary
#' @export

buildDataDict <- function(dat,
                          IDcols = NULL,
                          outdir = NULL) {
  # check inputs
  if (!("data.frame" %in% class(dat))) {
    stop("Must input a data.frame object")
  }
  if (!is.null(IDcols)) {
    idLog <- names(dat) %in% IDcols
    if (!any(idLog)) {
      stop("Provided ID columns not present in dataframe")
    }
  }

  # get pct NA for each field
  pctNA <- colMeans(is.na(dat))
  type <- unlist(lapply(dat, class))

  # manually update types
  # Mark ID cols
  type[idLog] <- "ID"
  # Mark all NA cols
  type[pctNA == 1] <- "allNA"

  # create new metadata dat
  dict <- rbind(type, pctNA)
  metadata <- lapply(colnames(dict), FUN = function(x) {
    col <- dat[x]
    coltype <- type[x]
    # handle numeric cols
    if (coltype == "numeric") {
      meta <- quantile(pull(col), na.rm = T)
      meta <- paste(names(meta), meta, sep = ": ", collapse = "  ")
    }
    # handle character cols
    if (coltype == "character" | coltype == "logical") {
      # get unique entries + counts
      entriesTbl <- as.data.frame(table(col[[x]]), stringsAsFactors = F)
      if (length(entriesTbl[,1]) <= 15) {
        meta <- paste( entriesTbl[,1], entriesTbl[,2], sep = "; ", collapse= " | ")
      }
      else {
        entriesTbl <- sample_n(entriesTbl, size = 10)
        meta <- paste(entriesTbl[,1], entriesTbl[,2], sep = "; ", collapse= " | ")
        meta <- paste("[[>15 unique entries]]", meta, sep = " ")
      }
    }
    if (coltype == "ID"|coltype == "allNA") {
      meta <- as.character(coltype)
    }
    return(meta)
  })

  metadata <- unlist(metadata)
  dict <- rbind(dict, metadata)

  # output
  if (!is.null(outdir)) {
    write.csv(dict, outdir)
  }

  return(data.table(dict))
}

#' A function to output information about a column or to compare two columns
#'
#' This function allows you to input two columns and outputs
#' simple descriptors of each column for easy comparison
#' @param col_orig Original column
#' @param col_new New column
#' @keywords summary
#' @export
valueSummary <- function(col_orig, col_new) {
  # original data
  # remove NA
  noNaColOrig <- col_orig[complete.cases(col_orig)]
  message("Original Data Type")
  cat(paste0(class(noNaColOrig), "\n"))
  message("First 20 Original Values")
  cat(paste0(head(noNaColOrig, n = 20), collapse = "; "))
  cat("\n")
  message("All Unique Original Values")
  cat(paste0(unique(noNaColOrig), collapse = "; "))
  cat("\n")
  message("Number of Original Unique Values")
  cat(paste0(length(unique(col_orig))))
  cat("\n")
  # new data
  # remove NA
  noNAColNew <- col_new[complete.cases(col_new)]
  message("New Data Type")
  cat(paste0(class(noNAColNew), "\n"))
  message("First 20 New Values")
  cat(paste0(head(noNAColNew, n = 20), collapse = "; "))
  cat("\n")
  message("All Unique New Values")
  cat(paste0(unique(noNAColNew), collapse = "; "))
  cat("\n")
  message("Number of New Unique Values")
  cat(paste0(length(unique(col_new))))
}

