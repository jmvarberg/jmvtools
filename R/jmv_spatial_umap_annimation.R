#' Make animation of data points from spatial to UMAP space
#'
#' @param seurat_object Spatial transcriptomics data in format of Seurat objec that contains UMAP dimensional reduction
#'
#' @return gganimate animation object
#' @export
#' @importFrom rlang := .data
#'
#' @examples
#' \dontrun{
#' library(jmvtools)
#' data <- jmvtools:::spatial_object
#' jmv_spatial_umap_animation(data)
#' }
#'


jmv_spatial_umap_animation <- function(seurat_object) {

    #extract UMAP from Seurat object
    umap <- seurat_object[["umap"]]@cell.embeddings
    clusters <- seurat_object$seurat_clusters
    umap <- umap |>
        as.data.frame() |>
        tibble::rownames_to_column() |>
        dplyr::mutate(UMAP_1 = scale(.data$UMAP_1),
               UMAP_2 = scale(.data$UMAP_2)) |>
        dplyr::rename(x = .data$UMAP_1,
               y = .data$UMAP_2,
               ID = .data$rowname)
    umap$Type <- clusters
    umap$Plot <- "UMAP"

    #extract spatial coordinates
    spatloc <- seurat_object@images[["image"]]@coordinates

    spatloc <- spatloc |>
        tibble::remove_rownames() |>
        dplyr::select(.data$cells, .data$x, .data$y) |>
        dplyr::rename(ID = .data$cells) |>
        dplyr::mutate(x = scale(.data$x),
               y = scale(.data$y))
    spatloc$Type <- clusters
    spatloc$Plot <- "Spatial"

    ## bind the UMAP and Spatial coordinate dataframes together
    df.trans <- rbind(umap, spatloc)

    # build the ggplot object, colored by seurat cluster
    p <- ggplot2::ggplot(df.trans, ggplot2::aes(x = .data$x, y = .data$y, col=factor(.data$Type))) +
        ggplot2::geom_point(size = 0.5, alpha=0.8, show.legend = FALSE, pch=16) +
        ggsci::scale_color_igv() +
        ggplot2::theme_void()

    #convert the ggplot object into an animation, transitioning between coordinates in UMAP vs Spatial state
    anim2 <- p +
        gganimate::transition_states(.data$Plot,
                          transition_length = 6,
                          state_length = 3) +
        ggplot2::labs(title = '{closest_state}') +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 32)) +
        gganimate::enter_fade()

    #render the animation
    out <- gganimate::animate(anim2 + gganimate::ease_aes('cubic-in-out'), height = 4, width = 4, units = "in", res = 400, nframes=100, fps=10)
    out

}
