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
#' @examples
#'
#' @note Mostly using this to rotate plots from Slide-seq spatial transcriptomics data for figure prep.
#'
#'
#'
ggrotate_xy <- function(df, x, y, deg=45) {

    deg = -deg*pi/180
    cosdeg <- cos(deg)
    sindeg <- sin(deg)

    centx <- df |> dplyr::select({{ x }}) |> dplyr::summarize(mean = mean({{ x }})) |> dplyr::pull(mean)
    centy <- df |> dplyr::select({{ y }}) |> dplyr::summarize(mean = mean({{ y }})) |> dplyr::pull(mean)

    df <- as.data.frame(df) |>
        dplyr::mutate(xnew=({{ x }}-centx)*cosdeg + ({{ y }}-centy)*sindeg + centx,
               ynew=-({{ x }}-centx)*sindeg + ({{ y }}-centy)*cosdeg + centy,
               {{ x }} := xnew,
               {{ y }} := ynew) |>
        dplyr::select(-xnew, -ynew)

}
