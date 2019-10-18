findReplace <- function(col,
                     current,
                     replacement,
                     upper = TRUE) {

  if (is.integer(dim(col))) {
    stop("col must be an vector")
  }

  if (upper) {
    col <- toupper(col)
  }
  colnum <- seq(ncol(tst))
  lapply(colnum, FUN = function(i) {
    words <- df[,i]
  })
}


standardizeYesNo <- function(col,
                             yes = NULL,
                             no = NULL,
                             upper = TRUE,
                             verbose = TRUE) {
  ### QC ###
  if (is.integer(dim(col))) {
    stop("col must be an vector")
  }

  ### UPDATE VARS ###
  colOrig <- col
  if (upper) {
    col <- toupper(col)
  }

  ### SET UP REPLACEMENT VARS ###
  replaceYes <- "^Y$"
  replaceNo <- "^N$"

  ### MERGE PROVIDED Y/N ###
  if (length(yes) > 0) {
    yes <- paste0("^", yes, "$", collapse = "|")
    replaceYes <- toupper(paste(replaceYes, yes, sep = "|"))
  }
  if (length(no) > 0) {
    no <- paste0("^", no, "$", collapse = "|")
    replaceNo <- toupper(paste(replaceNo, no, sep = "|"))
  }

  ### REPLACE ###
  col <- gsub(replaceYes,
              "YES",
              col)
  col <- gsub(replaceNo,
              "NO",
              col)

  ### PRINT STATS ###
  leftover <- paste0(setdiff(col, c("YES", "NO")), collapse = ", ")

  if (verbose) {
    if (length(leftover) > 0) {
      message(paste0("remaining values in col: ", leftover))

    }
  }
  return(col)
}

standardizeYesNo(tst)
