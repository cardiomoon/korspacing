#' Spacing Korean Language
#'
#' @param text A character string to check spacing
#' @return A spacing-checked string
#' @export
#' @examples
#' korspacing("아래와같은방식으로API를사용할수있으며,호출건수에대해서별도의제한은없으나,1회 호출에200글자로글자수를제한하고있다.")
#' @importFrom rstudioapi getActiveDocumentContext insertText
#' @importFrom httr PUT content
#' @importFrom utils install.packages
korspacing <- function(text=NULL){

  if(is.null(text)){
  context <- rstudioapi::getActiveDocumentContext()

  # Set the default data to use based on the selection.
  text1 <- context$selection[[1]]$text
  } else{
    text1 <-text
  }

  spacing_ <- function(text){
    if (!requireNamespace("httr")) install.packages("httr")

    res <- httr::PUT(url = "http://35.201.156.140:8080/spacing",
                     body = list(sent = as.character(text)))

    out <- httr::content(res)$sent
  }

  ress <- sapply(text1, spacing_,  USE.NAMES = F)

  if(length(ress) == 1) ress <- ress[[1]]

  if(is.null(text)) {
    rstudioapi::insertText(text=ress)
  }
  ress
}
