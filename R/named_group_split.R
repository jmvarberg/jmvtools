
#' Perform a named group split.
#'
#' Splits a dataframe into a list, with each list object named based on the value of the grouping variable.
#'
#' @param .tbl tibble/dataframe input object
#' @param ... additional params passed to dplyr::group_by()
#'
#' @return List of data frames, named based on the value of the grouping variable.
#' @export
#'
#' @examples
#'
#' mtcars |> named_group_split(cyl)
#'
#' @note stolen from https://github.com/tidyverse/dplyr/issues/4223
#'
named_group_split <- function(.tbl, ...) {
    grouped <- dplyr::group_by(.tbl, ...)
    names <- rlang::inject(paste(!!!dplyr::group_keys(grouped), sep = " / "))

    grouped |>
        dplyr::group_split() |>
        rlang::set_names(names)
}


