#' Convert ENSMBL IDs to Gene Names
#'
#' This is a helper function to simplify the conversion of matrix rownames formatted in ENSMBL ID format (i.e., ENSMUSG00000051951) to the corresponding
#' gene name (i.e, Xkr4). This is mostly a helper tool for late stages of analysis with Seurat objects, where you want to make plots showing gene names,
#' or you need to format with gene names as input for other analysis tools. This is also used for preparing the files needed for uploading of datasets
#' to the Single Cell Portal.
#'
#' The approach uses reference tables from the `annotables` package for the conversion purposes. The grcm38 and grch38 are used for conversion for mouse and human, respectively.
#' One-to-many mapping issues are handled by only replacing the ENSMBL ID if it maps to a unique gene name.
#' If multiple ENSMBL IDs map to the same gene symbol, then the feature name is left in ENSMBL format.
#'
#' @param matrix Input matrix with rownames as ENSMBL IDs (i.e, as accessed by Seurat::GetAssayData(object, assay, slot))
#' @param species Character, either "mouse" or "human" are currently supported.
#'
#' @return Output matrix with rownames converted from ENSMBL IDs to gene names.
#' @export
#'
#' @examples
#' \dontrun{
#' data <- jmvtools:::spatial_object
#' orig <- Seurat::GetAssayData(data, slot = "counts", assay = "SCT")
#' new <- swap_ensmbl_for_names(orig, species = "mouse")
#' head(orig)
#' head(new)
#'}
#'
swap_ensmbl_for_names <- function(matrix, species = c("mouse", "human")) {


    #get the annotable for species
    if(species == "mouse") {
        ref_table <- annotables::grcm38
    }

    if(species == "human") {
        ref_table <- annotables::grch38
    }

    if(!species %in% c("mouse", "human")) {
        error("Must select either mouse or human as input species.")
    }

    eids <- rownames(matrix) #get row/feature names from seurat object
    symbols <- ref_table$symbol[match(eids, ref_table$ensgene)] #extract names from ids using genedata table

    #make data frame and handle multi-mapping issues
    df <- data.frame(Original = eids,
                     New = symbols)

    #find the symbols that match to multiple IDs
    multi_matches <- unique(df$New[duplicated(df$New)])

    #make new column with symbols - if it is in multi match then keep the ensmbl ID
    df <- df |>
        dplyr::mutate(Final = dplyr::if_else(New %in% multi_matches, Original, New))

    #replace original matrix rownames with updated rownames
    rownames(matrix) <- df$Final
    return(matrix)
}
