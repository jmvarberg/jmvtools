#' Generate a data.frame from list of vectors of uneven length
#'
#' @note For situations where you want to generate a data frame with columns that have different lengths.
#' This works by calculating the maximum length of objects in the list, then padding the remaining list objects
#' to fill with empty spaces so that a data frame can be created.
#'
#' @param list Named list of character vectors to be used to generate data frame, with each vector ending up as a unique column.
#'
#' @return Data frame object with each vector from the input list as a column.
#' @export
#'
#' @examples
#' List <- list(A = sample(letters, 15), B = sample(letters, 20), C = sample(letters, 5))
#' output <- jmv_mixedLengthDF(List)
#'
jmv_mixedLengthDF <- function(list) {

    #get maxlength of up clusters
    maxlength <- max(unlist(lapply(list, function(x) length(x))))

    #add "" to list objects to make equal length
    fill_vector <- function(x, max) {

        filled <- c(x, rep("", max - length(x)))

    }

    #get the max length in list
    list_filled <- lapply(list, fill_vector, max = maxlength)

    #make data frame from the list
    df <- do.call(cbind.data.frame, list_filled)

}
