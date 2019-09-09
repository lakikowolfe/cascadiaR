#' A function to build simple data dictionaries from a dataframe.
#'
#' This function allows you to input a dataframe and outputs
#' simple descriptors of each field in that dataframe.
#' @param IDcols Are columns in this dataframe used for identification? Defaults to NULL.
#' @keywords data dictionary
#' @export

buildDataDict <- function(df, IDcols = NULL) {
  # check inputs
  if (!("data.frame" %in% class(df))) {
    stop("Must input a data.frame object")
  }
  idLog <- names(df) %in% IDcols
  if (!any(idLog)) {
    stop("Provided ID columns not present in dataframe")
  }

  # get pct NA for each field
  pctNA <- colMeans(is.na(df))
  type <- unlist(lapply(df, class))

  # manually update types
  # Mark ID cols
  type[idLog] <- "ID"
  # Mark all NA cols
  type[pctNA == 1] <- "allNA"

  # create new metadata df
  dict <- rbind(type, pctNA)
  metadata <- lapply(colnames(dict), FUN = function(x) {
    col <- df[x]
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
  return(data.table(dict))
}
