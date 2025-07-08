# Adding outliers to a WS or SAP

# add_outlier_sai <- function(jsai, spec = c("domainSpec", "estimationSpec"), ...) {
#     if (spec == "domainSpec") {
#         old_spec <- sai$domainSpec
#     } else if (spec == "estimationSpec") {
#         old_spec <- sai$estimationSpec
#     } else {
#         stop("Argument spec unknown.")
#     }
#
    # new_spec <- old_spec |>
    #     rjd3toolkit::add_outlier(...)
#
    # if (spec == "domainSpec") {
    #     rjd3workspace::set_domain_specification(jsap = jsap, idx = id_sai, spec = new_spec)
    #     rjd3workspace::set_name(jsap, idx = id_sai, name = series_name)
    # } else if (spec == "estimationSpec") {
    #     rjd3workspace::set_specification(jsap = jsap, idx = id_sai, spec = new_spec)
    # } else {
    #     stop("Argument spec unknown.")
    # }
    # return(jsai)
# }

# add_outlier_sap <- function(jsap, spec = c("domainSpec", "estimationSpec"), ...) {
#     nb_sai_ <- .jsap_sai_count(jsap)
#     for (id_sai in seq_len(nb_sai_)) {
#         jsai <- .jsap_sai(jsap, idx = 1L)
#         add_outlier_sai(jsai, spec = spec, ...)
#     }
#     return(jsap)
# }

# add_outlier_workspace <- function(jws, spec = c("domainSpec", "estimationSpec"), ...) {
#     nb_sap <- seq_len(.jws_sap_count(jws))
#     for (id_sap in nb_sap) {
#         jsap <- .jws_sap(jws, idx = idk_sap)
#         add_outlier_sap(jsap, spec = spec, ...)
#     }
#     return(jws)
# }

add_outlier_workspace <- function(jws, id_sap = NULL, id_sai = NULL, spec = c("domainSpec", "estimationSpec"), ...) {
    if (is.null(id_sap)) id_sap <- seq_len(.jws_sap_count(jws))
    for (idk_sap in id_sap) {
        jsap <- .jws_sap(jws, idx = idk_sap)
        if (is.null(id_sai)) id_sai <- seq_len(.jsap_sai_count(jsap))
        for (idk_sai in id_sai) {

            jsai <- jsap_sai(jsap, idk_sai)
            sai <- read_sai(jsai)
            series_name <- rjd3workspace::sai_name(jsai)

            if (spec == "domainSpec") {
                old_spec <- sai$domainSpec
            } else if (spec == "estimationSpec") {
                old_spec <- sai$estimationSpec
            } else {
                stop("Argument spec unknown.")
            }

            new_spec <- old_spec |>
                rjd3toolkit::add_outlier(...)

            if (spec == "domainSpec") {
                rjd3workspace::set_domain_specification(jsap = jsap, idx = idk_sai, spec = new_spec)
                rjd3workspace::set_name(jsap, idx = idk_sai, name = series_name)
            } else if (spec == "estimationSpec") {
                rjd3workspace::set_specification(jsap = jsap, idx = idk_sai, spec = new_spec)
            } else {
                stop("Argument spec unknown.")
            }

        }
    }
    return(jws)
}
