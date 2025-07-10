
affect_outliers <- function(outliers, ws_path) {

    jws <- .jws_open(file = ws_path)
    ws_name <- ws_path |> basename() |> tools::file_path_sans_ext()
    jsap <- jws_sap(jws, 1L)

    for (id_sai in seq_len(.jsap_sa_count(jsap))) {

        jsai <- .jsap_sa(jsap, idx = id_sai)
        series_name <- .jsa_name(jsai)
        cat(paste0("Série ", series_name, ", ", id_sai, "/", .jsap_sa_count(jsap), "\n"))

        # Outliers
        outliers_series <- outliers[[series_name]]

        # Création de la spec
        new_domainSpec <- domainSpec <- sai$domainSpec

        if (!is.null(outliers_series) && length(outliers_series) > 0L) {
            new_domainSpec <- domainSpec |>
                add_outlier(
                    type = outliers_type_pattern |>
                        gregexpr(text = outliers_series) |>
                        regmatches(x = outliers_series) |>
                        do.call(what = c),
                    date = date_pattern |>
                        gregexpr(text = outliers_series) |>
                        regmatches(x = outliers_series) |>
                        do.call(what = c)
                )
        }

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
