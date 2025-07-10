
affect_cjo <- function(cjo, ws_path) {

    jws <- .jws_open(file = ws_path)
    ws_name <- ws_path |> basename() |> tools::file_path_sans_ext()
    jsap <- jws_sap(jws, 1L)

    for (id_sai in seq_len(.jsap_sa_count(jsap))) {

        jsai <- .jsap_sa(jsap, idx = id_sai)
        series_name <- .jsa_name(jsai)
        cat(paste0("Série ", series_name, ", ", id_sai, "/", .jsap_sa_count(jsap), "\n"))

        # CJO
        jeu_regresseur <- specs[which(specs[["series"]] == series_name), "regs"]
        cjo_variables <- NULL
        if (any(grepl("REG1", jeu_regresseur, fixed = TRUE))) {
            cjo_variables <- "r.REG1_Semaine"
        } else if (any(grepl("REG2", jeu_regresseur, fixed = TRUE))) {
            cjo_variables <- c("r.REG2_Semaine", "r.REG2_Samedi")
        } else if (any(grepl("REG3", jeu_regresseur, fixed = TRUE))) {
            cjo_variables <- c("r.REG3_Lundi", "r.REG3_Semaine", "r.REG3_Samedi")
        } else if (any(grepl("REG5", jeu_regresseur, fixed = TRUE))) {
            cjo_variables <- c("r.REG5_Lundi", "r.REG5_Mardi", "r.REG5_Mercredi",
                               "r.REG5_Jeudi", "r.REG5_Vendredi")
        } else if (any(grepl("REG6", jeu_regresseur, fixed = TRUE))) {
            cjo_variables <- c("r.REG6_Lundi", "r.REG6_Mardi", "r.REG6_Mercredi",
                               "r.REG6_Jeudi", "r.REG6_Vendredi", "r.REG6_Samedi")
        }
        if (any(grepl("LY", jeu_regresseur, fixed = TRUE))) {
            cjo_variables <- c(cjo_variables, "r.LY")
        }

        # Création de la spec
        new_domainSpec <- domainSpec <- sai$domainSpec

        # Création de la spec
        new_domainSpec <- domainSpec |>
            set_tradingdays(
                option = "UserDefined",
                uservariable = cjo_variables,
                test = "None"
            )

        set_domain_specification(jsap = jsap, idx = id_sai, spec = new_domainSpec)
        set_name(jsap, idx = id_sai, name = series_name)
    }

    # Save WS automatique
    save_workspace(
        jws = jws,
        file = ws_path,
        replace = TRUE
    )
}
