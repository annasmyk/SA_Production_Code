
remove_non_significative_outliers <- function(ws_path, threshold = 0.3) {
    ws_name <- basename(ws_path) |> tools::file_path_sans_ext()
    cat("\n🏷️ WS ", ws_name, "\n")
    jws <- .jws_open(file = ws_path)
    .jws_compute(jws)
    jsap <- .jws_sap(jws, 1L)

    nb_sai <- .jsap_sa_count(jsap)

    for (id_sai in seq_len(nb_sai)) {
        cat("📌 SAI n°", id_sai, "\n")
        jsai <- .jsap_sai(jsap, id_sai)
        sai <- .jsai_read(jsai)
        series_name <- .jsa_name(jsai)

        new_estimationSpec <- estimationSpec <- sai$estimationSpec
        new_domainSpec <- domainSpec <- sai$domainSpec

        outliers <- estimationSpec$regarima$regression$outliers
        outliers_domain <- domainSpec$regarima$regression$outliers
        outliers_name_domain <- lapply(
            X = outliers_domain,
            FUN = \(outlier) paste0(outlier$code, " (", outlier$pos, ")")
        ) |>
            do.call(what = c)

        xregs <- summary(sai)$preprocessing$xregs
        outliers_to_remove <- NULL

        for (id_out in seq_along(outliers)) {
            outlier <- outliers[[id_out]]
            outlier_name <- paste0(outlier$code, " (", outlier$pos, ")")

            if (outlier_name %in% rownames(xregs)
                && !is.na(xregs[outlier_name, "Pr(>|t|)"])
                && xregs[outlier_name, "Pr(>|t|)"] > threshold) {
                cat("❌ Suppression de l'outlier :", outlier_name, "\n")

                new_estimationSpec <- new_estimationSpec |>
                    remove_outlier(type = outlier$code, date = outlier$pos)
                if (outlier_name %in% outliers_name_domain) {
                    new_domainSpec <- new_domainSpec |>
                        remove_outlier(type = outlier$code, date = outlier$pos)
                    cat("L'outlier est dans la domainSpec.\n")
                }
                outliers_to_remove <- c(outlier_name, outliers_to_remove)
            }
        }

        set_specification(jsap, id_sai, new_estimationSpec)
        set_domain_specification(jsap, id_sai, new_domainSpec)
        set_name(jsap, idx = id_sai, name = series_name)
    }

    cat("💾 Saving WS file\n")
    save_workspace(
        jws = jws,
        file = ws_path,
        replace = TRUE
    )
}
