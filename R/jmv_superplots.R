#' SuperPlots
#'
#' @param df data frame containing data to be plotted
#' @param groups name of data frame column containing group levels information
#' @param replicate name of data frame column containing replicate levels information
#' @param value name of data frame column containing values to be plotted on y-axis for each group, replicate
#' @param font_size font size parameter passed to cowplot::theme_cowplot() function
#' @param ylab Label for the y-axis of plot
#'
#' @return ggplot object
#' @export
#' @importFrom ggplot2 ggplot aes xlab ylab
#'
#' @examples
#'df_1 <- data.frame(value = sample(100:200, 100, replace=TRUE),
#'                   group = rep(1:4, 25),
#'                   replicate = 1)
#' df_2 <- data.frame(value = sample(100:200, 100, replace=TRUE),
#'                    group = rep(1:4, 25),
#'                    replicate = 2)
#'
#' df_3 <- data.frame(value = sample(100:200, 100, replace=TRUE),
#'                    group = rep(1:4, 25),
#'                    replicate = 3)
#'
#' df <- do.call(rbind, list(df_1, df_2, df_3))
#' jmv_superplots(df, groups = "group", replicate = "replicate", value = "value")



jmv_superplots <- function(df, groups, replicate, value, font_size=20, ylab="") {

    ReplicateAverages <- df |>
        dplyr::group_by({{ groups }}, {{ replicate }}) |>
        dplyr::summarise_if(is.numeric, list(mean))

    ggplot(df, aes(x= {{ groups }}, y= {{ value }}, color=factor({{ replicate }}))) +
        ggbeeswarm::geom_quasirandom(width=0.2, cex=2, alpha=0.2) +
        ggbeeswarm::geom_beeswarm(data=ReplicateAverages, size=4, color="white", pch=21, aes(fill=factor(ReplicateAverages[{{ replicate }}]))) +
        ggplot2::scale_colour_brewer(palette = "Set1", aesthetics = c("color", "fill")) +
        xlab("") +
        ylab(ylab) +
        cowplot::theme_cowplot(font_size = font_size, font_family = "sans") +
        ggpubr::rotate_x_text(angle=45) +
        ggplot2::theme(legend.position="none")
}

