#' Rotate a 2D plot by specified number of degrees
#'
#' @param df Original dataframe object
#' @param x Name of column containing data plotted on x-axis
#' @param y Name of column containing data plotted on x-axis
#' @param deg Degrees to rotate the plotted data
#'
#' @return dataframe with x and y coordinates rotated by specified number of degrees
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point theme_void
#' @importFrom rlang := .data
#'
#'
#' @examples
#' library(jmvtools)
#' jmvtools:::spatial_data |>
#'     ggplot2::ggplot(ggplot2::aes(x, y, color=factor(cluster))) +
#'     ggplot2::geom_point() +
#'     ggplot2::theme_void() +
#'     ggsci::scale_color_igv()
#'
#' jmvtools:::spatial_data |>
#'     ggrotate_xy(x, y, deg=90) |>
#'     ggplot2::ggplot(ggplot2::aes(x, y, color=factor(cluster))) +
#'     ggplot2::geom_point() +
#'     ggplot2::theme_void() +
#'     ggsci::scale_color_igv()
#'
#' @note Mostly using this to rotate plots from Slide-seq spatial transcriptomics data for figure prep.
#'

ggrotate_xy <- function(df, x, y, deg=45) {

    deg = deg*pi/180
    cosdeg <- cos(deg)
    sindeg <- sin(deg)

    centx <- df |> dplyr::select({{ x }}) |> dplyr::summarize(mean = mean({{ x }})) |> dplyr::pull(mean)
    centy <- df |> dplyr::select({{ y }}) |> dplyr::summarize(mean = mean({{ y }})) |> dplyr::pull(mean)

    df <- as.data.frame(df) |>
        dplyr::mutate(xnew=({{ x }}-centx)*cosdeg + ({{ y }}-centy)*sindeg + centx,
               ynew=-({{ x }}-centx)*sindeg + ({{ y }}-centy)*cosdeg + centy,
               {{ x }} := .data$xnew,
               {{ y }} := .data$ynew) |>
        dplyr::select(-.data$xnew, -.data$ynew)

}
