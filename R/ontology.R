# function to check existance of ontology specified by user
.checkOntology <- function(ebiOntology) {
  ont <- Ontologies()
  ontNames <- names(ont@x)
  return(ebiOntology %in% ontNames)
}

# function to take a term/query/ontology and output a df with annotations
.queryToAnnotation <- function(term, queryRes, ontObj){
  res <- data.frame(origWord = term,
                    label = NA,
                    description = NA,
                    synonyms = NA,
                    mapping = NA,
                    type = NA,
                    stringsAsFactors = FALSE)
  if (!is.null(queryRes)) {
    qrydf <- as(queryRes, "data.frame")
    qrydf <- qrydf[qrydf$type == "class", ]
    id <- qrydf[1,"obo_id"]
    termOnt <- term(ontObj, id)
    description <- unlist(termOnt@description)
    if (is.null(description)) {
      description <- paste0(unlist(termOnt@annotation$ALT_DEFINITION), collapse = "; ")
    }
    res$label <- termOnt@label
    res$description <- description
    res$synonyms <- paste0(unlist(termOnt@synonym), collapse = "; ")
    res$mapping <- paste0(unlist(termOnt@annotation$Maps_To), collapse = "; ")
    res$type <- paste0(unlist(termOnt@annotation$Semantic_Type), collapse = "; ")
  }
  return(res)

}
# function to annotate a single term
.getAnnotation <- function(term,
                           ebiOntology,
                           exact = FALSE,
                           verbose = FALSE) {
  if(!.checkOntology(ebiOntology)) {
    stop(paste0(ebiOntology, " is not a supported ontology"))
  }
  ontObj <- Ontology(ebiOntology)
  query <- OlsSearch(q = term, ontology = ebiOntology , exact = exact)
  if(verbose) {
    message(paste0("Query has ", query@numFound, " results"))
  }
  queryRes <- tryCatch(olsSearch(query),
                  error=function(cond){
                    message(paste0("no match for value: ", query@q))
                    return(NULL)
                  })

  if(!is.null(queryRes)) {
    .queryToAnnotation(term = term,
                       queryRes = queryRes,
                       ontObj = ontObj)
  }
}

# annotate a single term or a vector of terms
#' A function that takes a single term or a vector of terms and queries a specified ontology.
#'
#'
#' @param term a single term or vector of terms
#' @param ebiOntology (ebi ontologies)[https://www.ebi.ac.uk/ols/ontologies]
#' @param exact Strings to be transformed to NO
#' @param verbose Transform all to upper case to make substitution easier
#' @keywords ontology
#' @export

annotateTerms <- function(term,
                          ebiOntology,
                          exact = FALSE,
                          verbose = FALSE) {
  if (length(term == 1)) {
    res <- .getAnnotation(term, ebiOntology, exact, verbose)
  } else {
    res <- lapply(term, .getAnnotation, ebiOntology, exact, verbose)
    res <- bind_rows(res)
  }
  return(res)
}
