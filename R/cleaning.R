# findReplace <- function(col,
#                      current,
#                      replacement,
#                      upper = TRUE) {
#
#   if (is.integer(dim(col))) {
#     stop("col must be an vector")
#   }
#
#   if (upper) {
#     col <- toupper(col)
#   }
#   colnum <- seq(ncol(tst))
#   lapply(colnum, FUN = function(i) {
#     words <- df[,i]
#   })
# }

#' A function that standardizes binary columns of Y/N
#'
#' This function allows you to input a vector and strings that should be transformed to
#' YES or NO and outputs a standardized column
#'
#' @param col Column or vector to be standardized
#' @param yes Strings to be transformed to YES
#' @param no Strings to be transformed to NO
#' @param upper Transform all to upper case to make substitution easier
#' @param verbose Output a message that identifies left over strings in the column that are not YES or NO
#' @keywords data cleaning
#' @export

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
