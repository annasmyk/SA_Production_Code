
retrieve_cjo <- function(ws_path) {

    jws <- .jws_open(file = ws_path)
    ws <- read_workspace(jws, compute = FALSE)
    ws_name <- ws_path |> basename() |> tools::file_path_sans_ext()
    sap <- ws[["processing"]][[1L]]

    cjo <- data.frame(series = names(sap),
                        regs = character(length(sap)),
                        series_span = character(length(sap)),
                        stringsAsFactors = FALSE)

    for (id_sai in seq_along(sap)) {
        series_name <- names(sap)[id_sai]
        cat(paste0("Série ", series_name, ", ", id_sai, "/", length(sap), "\n"))

        sai <- sap[[id_sai]]
        regression_section <- sai[["domainSpec"]][["regarima"]][["regression"]]
        regressors <- regression_section[["td"]][["users"]]

        regs_cjo <- "Pas_CJO"
        if (any(grepl(pattern = "REG1", x = regressors, fixed = TRUE))) {
            regs_cjo <- "REG1"
        } else if (any(grepl(pattern = "REG5", x = regressors, fixed = TRUE))) {
            regs_cjo <- "REG5"
        } else if (any(grepl(pattern = "REG2", x = regressors, fixed = TRUE))) {
            regs_cjo <- "REG2"
        } else if (any(grepl(pattern = "REG3", x = regressors, fixed = TRUE))) {
            regs_cjo <- "REG3"
        } else if (any(grepl(pattern = "REG6", x = regressors, fixed = TRUE))) {
            regs_cjo <- "REG6"
        }
        if (any(grepl(pattern = "LeapYear", x = regressors, fixed = TRUE)
                | grepl(pattern = "LY", x = regressors, fixed = TRUE))) {
            regs_cjo <- paste0(regs_cjo, "_LY")
        }
        cjo[id_sai, "regs"] <- regs_cjo
    }

    return(cjo)
}

export_cjo <- function(x, ws_name, path, verbose = TRUE) {
    if (missing(path)) {
        file_name <- paste0("cjo_", ws_name, ".yaml")
        path <- file.path("model", file_name)
    }
    if (verbose) {
        cat("The cjo will be written at ", path, "\n")
    }
    yaml::write_yaml(
        x = x,
        file = path
    )
    return(invisible(path))
}

import_cjo <- function(x, ws_name, path, verbose = TRUE) {
    if (missing(path)) {
        file_name <- paste0("cjo_", ws_name, ".yaml")
        path <- file.path("model", file_name)
    }
    if (verbose) {
        cat("The cjo will be read at ", path, "\n")
    }
    cjo <- yaml::read_yaml(
        file = path
    )
}


