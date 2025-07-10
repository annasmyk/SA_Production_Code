
create_french_calendar <- function() {
    cal <- rjd3toolkit::national_calendar(
        days = list(
            Bastille_day = rjd3toolkit::fixed_day(7, 14), # Bastille Day
            Victory_day = rjd3toolkit::fixed_day(5, 8, validity = list(start = "1982-05-08")), # Victoire 2nd guerre mondiale
            NEWYEAR = rjd3toolkit::special_day("NEWYEAR"), # Nouvelle année
            CHRISTMAS = rjd3toolkit::special_day("CHRISTMAS"), # Noël
            MAYDAY = rjd3toolkit::special_day("MAYDAY"), # 1er mai
            EASTERMONDAY = rjd3toolkit::special_day("EASTERMONDAY"), # Lundi de Pâques
            ASCENSION = rjd3toolkit::special_day("ASCENSION"), # attention +39 et pas 40 jeudi ascension
            WHITMONDAY = rjd3toolkit::special_day("WHITMONDAY"), # Lundi de Pentecôte (1/2 en 2005 a verif)
            ASSUMPTION = rjd3toolkit::special_day("ASSUMPTION"), # Assomption
            ALLSAINTSDAY = rjd3toolkit::special_day("ALLSAINTSDAY"), # Toussaint
            ARMISTICE = rjd3toolkit::special_day("ARMISTICE")
        )
    )

    return(cal)
}

create_insee_regressors <- function(
        start = c(1990L, 1L),
        frequency = 12L,
        length = 492L,
        s = NULL
) {

    cal_FR <- create_french_calendar()

    groups <- list(REG1 = c(1L, 1L, 1L, 1L, 1L, 0L, 0L),
                   REG2 = c(1L, 1L, 1L, 1L, 1L, 2L, 0L),
                   REG3 = c(1L, 2L, 2L, 2L, 2L, 3L, 0L),
                   REG5 = c(1L, 2L, 3L, 4L, 5L, 0L, 0L),
                   REG6 = c(1L, 2L, 3L, 4L, 5L, 6L, 0L))

    regs_cjo <- lapply(
        X = groups,
        FUN = rjd3toolkit::calendar_td,
        calendar = cal_FR,
        frequency = frequency,
        start = start,
        length = length,
        s = s
    ) |>
        do.call(what = cbind)
    cols <- colnames(regs_cjo) |> gsub(pattern = ".", replacement = "_", fixed = TRUE)
    regs_cjo <- cbind(
        LY = rjd3toolkit::lp_variable(
            frequency = frequency,
            start = start,
            length = length,
            s = s,
            type = "LeapYear"
        ),
        regs_cjo
    )
    colnames(regs_cjo)[-1] <- cols

    return(regs_cjo)
}

create_insee_regressors_sets <- function(
        start = c(1990L, 1L),
        frequency = 12L,
        length = 492L,
        s = NULL
) {

    regs_cjo <- create_insee_regressors(
        frequency = frequency,
        start = start,
        length = length,
        s = s
    )

    n <- colnames(regs_cjo)

    REG1 <-  regs_cjo[, startsWith(n, prefix = "REG1"), drop = FALSE]
    attr(REG1, "class") <- c("mts", "ts", "matrix", "array")

    LY <- regs_cjo[, startsWith(n, prefix = "LY"), drop = FALSE]
    attr(LY, "class") <- c("mts", "ts", "matrix", "array")

    sets <- list(
        REG1 = REG1,
        REG2 = regs_cjo[, startsWith(n, prefix = "REG2")],
        REG3 = regs_cjo[, startsWith(n, prefix = "REG3")],
        REG5 = regs_cjo[, startsWith(n, prefix = "REG5")],
        REG6 = regs_cjo[, startsWith(n, prefix = "REG6")],
        LY = LY,
        REG1_LY = regs_cjo[, startsWith(n, prefix = "REG1") |
                               startsWith(n, prefix = "LY")],
        REG2_LY = regs_cjo[, startsWith(n, prefix = "REG2") |
                               startsWith(n, prefix = "LY")],
        REG3_LY = regs_cjo[, startsWith(n, prefix = "REG3") |
                                startsWith(n, prefix = "LY")],
        REG5_LY = regs_cjo[, startsWith(n, prefix = "REG5") |
                               startsWith(n, prefix = "LY")],
        REG6_LY = regs_cjo[, startsWith(n, prefix = "REG6") |
                               startsWith(n, prefix = "LY")]
    )

    return(sets)
}
