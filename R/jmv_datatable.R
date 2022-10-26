#' Interactive data tables
#'
#' @param df Data frame
#' @param digits Number of digits to include in formatted numbers.
#' @param ... Additional parameters to pass to `DT::datatable`
#'
#' @return Embedded HTML data table, `DT::datatable` object
#' @export
#'
#' @examples
#'
#' jmv_datatables(mtcars)
#'
#' mtcars |> jmv_datatables()
#'
jmv_datatables <- function(df, digits=2, ...) {

    if (!is.data.frame(df)) {
        df <- as.data.frame(df)
    }

    stopifnot("Input object is not coerrcible to a data frame."= is.data.frame(df))

    df |>
        dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), round, digits)) |>
        DT::datatable(extensions = 'Buttons', options = list(
            scrollY="true",
            scrollX="true",
            pageLength = 10,
            lengthMenu = c(10, 25, 50, 100),
            dom = 'Blfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
            ...
        )
        )
}
