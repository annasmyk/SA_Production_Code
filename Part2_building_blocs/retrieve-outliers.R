
retrieve_outliers <- function(ws_path) {

    jws <- .jws_open(file = ws_path)
    ws <- read_workspace(jws, compute = FALSE)
    ws_name <- ws_path |> basename() |> tools::file_path_sans_ext()

    sap <- ws[["processing"]][[1L]]
    ps_outliers <- list()

    for (id_sai in seq_along(sap)) {
        series_name <- names(sap)[id_sai]
        cat(paste0("Série ", series_name, ", ", id_sai, "/", length(sap), "\n"))

        sai <- sap[[id_sai]]
        regression_section <- sai[["domainSpec"]][["regarima"]][["regression"]]

        outliers <- regression_section[["outliers"]] |> unique()
        ps_outliers[[series_name]] <- sapply(X = outliers, FUN = `[[`, "name")
    }

    return(ps_outliers)
}

export_outliers <- function(x, ws_name, path, verbose = TRUE) {
    if (missing(path)) {
        file_name <- paste0("outliers_", ws_name, ".yaml")
        path <- file.path("model", file_name)
    }
    if (verbose) {
        cat("The outliers will be written at ", path, "\n")
    }
    yaml::write_yaml(
        x = x,
        file = path
    )
    return(invisible(path))
}

import_outliers <- function(x, ws_name, path) {
    if (missing(path)) {
        file_name <- paste0("outliers_", ws_name, ".yaml")
        path <- file.path("model", file_name)
    }
    if (verbose) {
        cat("The outliers will be read at ", path, "\n")
    }
    outliers <- yaml::read_yaml(
        file = path
    )
}
