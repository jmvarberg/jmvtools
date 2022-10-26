#' Adding clickable links to data points on ggplot object
#'
#' @param p ggplot object, with URL mapped to `customdata` inside aes()
#'
#' @return Interactive plot, output by `ggplotly()`, with clickable data points that open associated URLs
#' @export
#'
#' @examples
#' p <- mtcars |>
#'     tibble::rownames_to_column(var="vehicle") |>
#'     dplyr::mutate(link = paste0("https://www.google.com/search?q=", vehicle)) |>
#'     ggplot2::ggplot(ggplot2::aes(mpg, hp, customdata=link)) + ggplot2::geom_point()
#' jmv_plotly_linked(p)
#'
#'
jmv_plotly_linked <- function(p) {

    stopifnot("Input must be an object of class `gg` or `ggplot`." = c("gg", "ggplot") %in% class(p))

    pp <- plotly::ggplotly(p)
    ppp <- htmlwidgets::onRender(pp, "
     function(el, x) {
     el.on('plotly_click', function(d) {
     var url = d.points[0].customdata;
     //url
     window.open(url);
     });
     }
     ")
    ppp
}





