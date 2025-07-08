
## Fonctions utiles ------------------------------------------------------------

create_specs_set <- function(span_start = "2012-01-01", context, outliers = NULL) {
    all_vars <- context$variables
    named_vars <- lapply(seq_along(all_vars), \(k) paste0(names(all_vars)[k], ".", names(all_vars[[k]])))
    names(named_vars) <- names(all_vars)

    spec_0 <- rjd3x13::x13_spec(name = "RSA3") |>
        rjd3toolkit::set_estimate(type = "From", d0 = span_start)

    if (!is.null(outliers)) {
        spec_0 <- rjd3toolkit::add_outlier(
            x = spec_0,
            type = outliers$type,
            date = outliers$date |> as.character()
        )
    }

    specs_set <- c(
        list(Pas_CJO = spec_0),
        lapply(
            X = named_vars,
            FUN = rjd3toolkit::set_tradingdays,
            x = spec_0,
            option = "UserDefined",
            test = "None",
            calendar.name = NA
        )
    )

    return(specs_set)
}

get_LY_info <- function(smod) {

    reg_table <- smod$preprocessing$xregs
    idx <- which(grepl(pattern = ".LY", x = rownames(reg_table), fixed = TRUE))
    if (length(idx) == 0L) {
        return(data.frame(LY_coeff = NA, LY_p_value = NA))
    } else if (length(idx) > 1L) {
        stop("Plusieurs variables portent le nom LY.")
    }
    LY_coeff <- reg_table[idx, "Estimate"]
    LY_p_value <- reg_table[idx, "Pr(>|t|)"]

    return(data.frame(LY_coeff = LY_coeff, LY_p_value = LY_p_value))
}

one_diagnostic <- function(series, spec, context) {

    mod <- rjd3x13::x13(ts = series, spec = spec, context = context,
                        userdefined = c("diagnostics.td-sa-all", "diagnostics.td-i-all"))

    res_td <- sapply(
        X = mod$user_defined,
        FUN = `[[`,
        "pvalue"
    )

    note <- sum((res_td < 0.05) * 2L:1L)
    aicc <- mod$result$preprocessing$estimation$likelihood$aicc
    mode <- c("Additive", "Multiplicative")[mod$result$preprocessing$description$log + 1L]

    LY_info <- get_LY_info(summary(mod))

    diag <- cbind(
        data.frame(note = note, aicc = aicc, mode = mode),
        LY_info
    )

    return(diag)
}


all_diagnostics <- function(series,
                            specs_set,
                            context) {
    diags <- lapply(X = seq_along(specs_set), FUN = function(k) {
        spec <- specs_set[[k]]
        cat("Computing spec", names(specs_set)[k], "...")
        diag <- one_diagnostic(series = series, spec = spec, context = context)
        cat("Done !\n")
        return(diag)
    })

    diags <- diags |> do.call(what = rbind)
    diags <- cbind(
        regs = names(specs_set),
        diags
    )
    rownames(diags) <- diags$regs

    return(diags)
}

# Ici jeu contient LY
verif_LY <- function(jeu, diags) {
    if (!grepl(pattern = "LY", x = jeu)) {
        return(jeu)
    }
    id_jeu <- which(diags$regs == jeu)

    LY_coeff <- diags[id_jeu, "LY_coeff"]
    LY_p_value <- diags[id_jeu, "LY_p_value"]
    mode <- diags[id_jeu, "mode"]

    if (jeu == "LY"){
        jeu_sans_LY <- "Pas_CJO"
    } else {
        jeu_sans_LY <- gsub(pattern = "_LY", replacement = "", x = jeu)
    }
    id_jeu_sans_LY <- which(diags$regs == jeu_sans_LY)

    # On reprend le choix avec et sans LY
    diags_jeu <- diags[c(id_jeu, id_jeu_sans_LY), ]

    if (diags_jeu$note[1] != diags_jeu$note[2]){
        return(rownames(diags_jeu)[which.min(diags_jeu$note)])
    }

    if (mode == "Multiplicatif"){
        LY_coeff <- 100 * LY_coeff
    }
    LY_coeff <- round(LY_coeff)

    # On considere le coeff LY incoherent si negatif ou superieur à 12
    coef_incoherent <- (LY_coeff <= 0) | (LY_coeff > 12)
    # Coeff non signif si pvalue > 10%
    coef_non_signif <- LY_p_value > 0.1

    jeu_final <- ifelse(
        test = coef_incoherent | coef_non_signif,
        yes = jeu_sans_LY,
        no = jeu
    )

    return(jeu_final)
}

#' @importFrom stats time
#' @importFrom utils tail
select_reg_one_series <- function(series,
                                  name = "",
                                  specs_set = NULL,
                                  context = create_context(),
                                  outliers = NULL,
                                  span_start = "2012-01-01") {
    if (as.Date(span_start) > zoo::as.Date(tail(time(series), n = 1L))) {
        stop("Span starts after the end of the series.")
    }

    if (is.null(specs_set)) {
        specs_set <- create_specs_set(span_start = span_start, context = context, outliers = outliers)
    }

    diags <- all_diagnostics(series, specs_set = specs_set, context = context)
    diags_wo_na <- diags |> subset(!is.na(note) & !is.na(aicc))

    if (nrow(diags_wo_na) == 0) {
        stop(
            "Erreur lors du calcul de l'aicc et des p-value.
             Aucun jeu de regresseur n'a pu être sélectionné. ",
            ifelse(name == "", "", paste0("(Série ", name, ")"))
        )
    } else if (all(diags_wo_na$note == 0)) {
        warning(
            "Aucun jeu de regresseur n'est significatif. ",
            ifelse(name == "", "", paste0("(Série ", name, ")"))
        )
    }

    best_regs <- diags_wo_na |>
        subset(note == min(note, na.rm = TRUE)) |>
        subset(aicc == min(aicc, na.rm = TRUE))

    return(verif_LY(jeu = best_regs[1, "regs"], diags = diags))
}


select_regs <- function(series,
                        with_outliers = FALSE,
                        path_ws_xml = NULL,
                        span_start = "2012-01-01",
                        cjo_sets = create_insee_regressors_sets()) {

    context <- rjd3toolkit::modelling_context(variables = cjo_sets)

    specs_set <- create_specs_set(span_start = span_start, context = context)

    # if (with_outliers) {
    #     ws_ref <- RJDemetra::load_workspace(path_ws_xml)
    #     RJDemetra::compute(workspace = ws_ref)
    #     sap_ref <- RJDemetra::get_object(ws_ref)
    #     series_name_ref <- RJDemetra::get_all_names(sap_ref)
    # }

    if (is.null(ncol(series))) {
        return(select_reg_one_series(series, span_start = span_start, specs_set = specs_set, context = context))
    }

    output <- sapply(X = seq_len(ncol(series)), FUN = function(k) {
        series_name <- colnames(series)[k]
        outliers <- NULL

        # if (with_outliers) {
        #     # On récupère les outliers
        #     sai_ref <- sap_ref |> RJDemetra::get_object(which(series_name_ref == series_name))
        #     sai_mod <- sai_ref |> RJDemetra::get_model(workspace = ws_ref)
        #     regressors <- sai_mod$regarima$regression.coefficients |> rownames()
        #     regressors <- regressors[substr(regressors, 1, 2) %in% c("AO", "TC", "LS", "SO")]
        #
        #     if (length(regressors) > 0) {
        #         outliers_type <- regressors |> substr(start = 1, stop = 2)
        #         outliers_date <- regressors |>
        #             substr(start = 5, stop = nchar(regressors) - 1) |>
        #             paste0("01-", ... = _) |>
        #             as.Date(format = "%d-%m-%Y")
        #
        #         outliers_type <- outliers_type[outliers_date >= as.Date(span_start)]
        #         outliers_date <- outliers_date[outliers_date >= as.Date(span_start)]
        #
        #         if (length(outliers_date) > 0) {
        #             outliers <- list(type = outliers_type,
        #                              date = outliers_date)
        #         }
        #     }
        # }

        cat(paste0("\nSérie ", series_name, " en cours... ", k, "/", ncol(series)), "\n")
        return(select_reg_one_series(
            series = series[, k],
            name = series_name,
            context = context,
            span_start = span_start,
            specs_set = specs_set,
            outliers = outliers
        ))
    })

    output <- cbind(series = colnames(series), reg_selected = output)
    return(output)
}
