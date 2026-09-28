# =============================================================================
# Loss Reserving - Vergleich gaengiger Reservierungsverfahren (R-Version)
# =============================================================================
#
# Verfahren
#   1. Chain Ladder (volumengewichtet)
#   2. Mack-Modell (Standardfehler, Quantile ueber Lognormal-Naeherung)
#   3. Expected Loss Ratio (ELR)
#   4. Bornhuetter-Ferguson (BF)
#   5. Cape Cod (Stanard-Buehlmann)
#   6. Additives Verfahren
#   7. ODP-Bootstrap (England & Verrall)
#
# Nutzung als Skript (Rscript):
#   Rscript loss_reserving.R                                   # Demo mit RAA-Dreieck
#   Rscript loss_reserving.R --excel daten.xlsx --elr 0.65
#
# Nutzung als Bibliothek:  source("loss_reserving.R"); d <- read_excel_input("daten.xlsx")
#                          r <- compute_all(d$tri, d$premiums, benchmark = d$benchmark)
#
# Eingabe: Excel-Datei mit drei Blaettern (Reihenfolge entscheidet, nicht der Name):
#   Blatt 1 Schaeden:         erste Spalte Anfallperiode, weitere Spalten Abwicklungsperioden,
#                             zukuenftige Zellen leer
#   Blatt 2 Beitraege:        Anfallperiode, Praemie                   (optional)
#   Blatt 3 Manuelle Reserve: Anfallperiode, Manuelle Reserve          (optional)
# Pakete:  readxl (Excel-Upload), openxlsx (Excel-Export).
# =============================================================================

# -----------------------------------------------------------------------------
# Beispieldaten: RAA-Dreieck (kumuliert). Referenz: CL-Reserve 52.135, Mack-SE 26.909.
# Praemien und A-priori-Quote sind FIKTIV.
# -----------------------------------------------------------------------------
RAA <- list(
  c(5012, 8269, 10907, 11805, 13539, 16181, 18009, 18608, 18662, 18834),
  c(106, 4285, 5396, 10666, 13782, 15599, 15496, 16169, 16704),
  c(3410, 8992, 13873, 16141, 18735, 22214, 22863, 23466),
  c(5655, 11555, 15766, 21266, 23425, 26083, 27067),
  c(1092, 9565, 15836, 22169, 25955, 26180),
  c(1513, 6445, 11702, 12935, 15852),
  c(557, 4020, 10946, 12314),
  c(1351, 6947, 13112),
  c(3133, 5395),
  c(2063)
)
RAA_ORIGINS   <- 2016:2025
DEMO_PREMIUMS <- c(30000, 30500, 31000, 32000, 33000, 34000, 35000, 36000, 37000, 38000)
DEMO_ELR      <- 0.70
BENCHMARK     <- "Manuelle Reserve"
Z_QUANTILES   <- c("75%" = 0.674490, "95%" = 1.644854, "99.5%" = 2.575829)

# -----------------------------------------------------------------------------
# Dreieck
# -----------------------------------------------------------------------------
to_cumulative <- function(inc) {
  cum <- t(apply(inc, 1, function(r) { o <- cumsum(ifelse(is.na(r), 0, r)); o[is.na(r)] <- NA; o }))
  if (ncol(inc) == 1) cum <- t(cum)
  dimnames(cum) <- dimnames(inc)
  cum
}

to_incremental <- function(cum) {
  inc <- cum
  if (ncol(cum) > 1) inc[, -1] <- cum[, -1, drop = FALSE] - cum[, -ncol(cum), drop = FALSE]
  inc
}

make_triangle <- function(cum, origins = rownames(cum), devs = colnames(cum), period = "year") {
  cum <- as.matrix(cum)
  storage.mode(cum) <- "double"
  if (is.null(origins)) origins <- seq_len(nrow(cum))
  if (is.null(devs)) devs <- seq_len(ncol(cum))
  dimnames(cum) <- list(as.character(origins), as.character(devs))
  if (nrow(cum) < 2 || ncol(cum) < 2)
    stop("Das Dreieck braucht mindestens 2 Anfall- und 2 Abwicklungsjahre.")
  obs <- !is.na(cum)
  for (i in seq_len(nrow(cum))) {
    if (!any(obs[i, ])) stop(sprintf("Anfalljahr %s enth\u00e4lt keine Werte.", origins[i]))
    last <- max(which(obs[i, ]))
    if (!all(obs[i, seq_len(last)])) stop(sprintf("Anfalljahr %s: L\u00fccke im beobachteten Bereich.", origins[i]))
  }
  latest_idx <- unname(rowSums(obs))                       # 1-basiert
  structure(list(
    cum = cum, origins = as.character(origins), devs = as.character(devs),
    I = nrow(cum), J = ncol(cum), obs = obs, latest_idx = latest_idx, period = period,
    labels = PERIOD_LABELS[[period]],
    latest = unname(cum[cbind(seq_len(nrow(cum)), latest_idx)]), inc = to_incremental(cum)
  ), class = "triangle")
}

triangle_from_list <- function(rows, origins) {
  J <- max(lengths(rows))
  m <- t(vapply(rows, function(r) c(r, rep(NA, J - length(r))), numeric(J)))
  make_triangle(m, origins, seq_len(J))
}

demo_triangle <- function() triangle_from_list(RAA, RAA_ORIGINS)

# Zahl aus Text (deutsches oder englisches Format)
parse_num <- function(x, decimal = ".") {
  x <- trimws(gsub("[[:space:]\u00a0]", "", as.character(x)))
  x[x == ""] <- NA
  if (decimal == ",") {
    x <- gsub(".", "", x, fixed = TRUE)
    x <- sub(",", ".", x, fixed = TRUE)
  } else {
    x <- gsub(",", "", x, fixed = TRUE)
  }
  suppressWarnings(as.numeric(x))
}

# Excel-Spalte -> Zahlen: numerische Zellen direkt, Text im deutschen oder englischen Format
cell_num <- function(col) {
  if (is.numeric(col)) return(as.numeric(col))
  x <- trimws(as.character(col)); x[x == ""] <- NA
  txt <- x[!is.na(x)]
  dec <- if (any(grepl(",", txt)) || (length(txt) && all(grepl("^-?\\d{1,3}(\\.\\d{3})+$|^-?\\d+$", txt)))) "," else "."
  parse_num(x, dec)
}

# Tabelle (data.frame, erste Spalte = Anfalljahr) -> Dreieck
triangle_from_frame <- function(df, incremental = FALSE) {
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  origins <- trimws(as.character(df[[1]]))
  vals <- as.matrix(as.data.frame(lapply(df[-1], cell_num), check.names = FALSE))
  keep_r <- rowSums(!is.na(vals)) > 0 & origins != "" & !is.na(origins)
  keep_c <- colSums(!is.na(vals)) > 0
  vals <- vals[keep_r, keep_c, drop = FALSE]
  devs <- colnames(df)[-1][keep_c]
  origins <- origins[keep_r]
  if (incremental) vals <- to_cumulative(vals)
  make_triangle(vals, origins, devs)
}

# -----------------------------------------------------------------------------
# Excel-Upload: Blatt 1 Schaeden, Blatt 2 Beitraege, Blatt 3 Manuelle Reserve
# -----------------------------------------------------------------------------
EXCEL_SHEETS <- c("Sch\u00e4den", "Beitr\u00e4ge", "Manuelle Reserve")

# Blatt i als data.frame (erste Zeile = Ueberschriften); NULL, wenn es fehlt oder leer ist
read_sheet <- function(path, i) {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("F\u00fcr den Excel-Upload wird das Paket 'readxl' ben\u00f6tigt (install.packages(\"readxl\")).")
  if (length(readxl::excel_sheets(path)) < i) return(NULL)
  df <- suppressMessages(readxl::read_excel(path, sheet = i, col_names = TRUE, .name_repair = "minimal"))
  df <- as.data.frame(df, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(df) || ncol(df) < 2) return(NULL)
  df
}

# Zwei Spalten (Anfallperiode, Wert) -> Vektor passend zum Dreieck
vector_from_frame <- function(df, tri, what, positive = TRUE) {
  lab <- trimws(as.character(df[[1]])); v <- cell_num(df[[2]])
  keep <- !is.na(lab) & lab != ""
  lab <- lab[keep]; v <- v[keep]
  if (!length(v) || all(is.na(v))) return(NULL)
  names(v) <- lab
  if (tri$period == "year" && all(vapply(lab, function(o) !is.null(parse_quarter(o)), TRUE))) {
    y <- vapply(lab, function(o) parse_quarter(o)[1], 0L)
    v <- tapply(v, as.character(y), sum)                    # Quartalswerte -> Jahre
  }
  if (all(tri$origins %in% names(v))) v <- v[tri$origins]
  else if (length(v) == tri$I) names(v) <- tri$origins
  else stop(sprintf("Die %s passen weder nach %s noch nach Anzahl zum Dreieck (%d Werte, %d %s).",
                    what, tri$labels$origin, length(v), tri$I, tri$labels$origin_pl))
  if (anyNA(v)) stop("F\u00fcr jede Anfallperiode wird ein Wert ben\u00f6tigt.")
  if (positive && any(v <= 0)) stop("Alle Werte m\u00fcssen positiv sein.")
  unname(v)
}

# Liest die Excel-Datei. Fehler in Blatt 2 oder 3 werden als Warnung gemeldet, die
# Werte dann nicht verwendet; ein Fehler in Blatt 1 bricht ab.
read_excel_input <- function(path, incremental = FALSE, period = "auto", to_years = FALSE, name = path) {
  if (!grepl("\\.xls[xm]?$", tolower(name)))
    stop("Bitte eine Excel-Datei (.xlsx oder .xls) hochladen.")
  df <- read_sheet(path, 1)
  if (is.null(df)) stop("Blatt 1 (Sch\u00e4den) ist leer oder fehlt.")
  prep <- prepare_triangle(triangle_from_frame(df, incremental), period, to_years)
  tri <- prep$tri; warnings <- character(0)
  get <- function(i, what, positive) {
    d <- read_sheet(path, i)
    if (is.null(d)) return(NULL)
    tryCatch(vector_from_frame(d, tri, what, positive), error = function(e) {
      warnings <<- c(warnings, sprintf("Blatt %d (%s): %s Die Werte werden nicht verwendet.",
                                       i, EXCEL_SHEETS[i], conditionMessage(e)))
      NULL
    })
  }
  premiums <- get(2, "Beitr\u00e4ge", TRUE)
  benchmark <- get(3, "Werte der manuellen Reserve", FALSE)
  list(tri = tri, premiums = premiums, benchmark = benchmark, notes = prep$notes, warnings = warnings)
}

# -----------------------------------------------------------------------------
# Jahres- und Quartalsdaten
# -----------------------------------------------------------------------------
PERIOD_LABELS <- list(
  year = list(origin = "Anfalljahr", origin_pl = "Anfalljahre", origin_dat = "Anfalljahren",
              dev = "Abwicklungsjahr", dev_pl = "Abwicklungsjahre", unit = "Jahr"),
  quarter = list(origin = "Anfallquartal", origin_pl = "Anfallquartale", origin_dat = "Anfallquartalen",
                 dev = "Abwicklungsquartal", dev_pl = "Abwicklungsquartale", unit = "Quartal"))

# '2023Q1', '2023-Q1', 'Q1 2023', '2023/1', '1. Quartal 2023' -> c(2023, 1); sonst NULL
parse_quarter <- function(label) {
  x <- trimws(as.character(label))
  pats <- list(
    list("^([0-9]{4})\\s*[-/ ]?\\s*[Qq]\\s*([1-4])$", 1, 2),
    list("^[Qq]\\s*([1-4])\\s*[-/ ]?\\s*([0-9]{4})$", 2, 1),
    list("^([0-9]{4})\\s*[-/.]\\s*([1-4])$", 1, 2),
    list("^([1-4])\\s*\\.?\\s*(Quartal|Qu\\.?|Q|q)\\s*([0-9]{4})$", 3, 1))
  for (p in pats) {
    m <- regmatches(x, regexec(p[[1]], x, perl = TRUE))[[1]]
    if (length(m)) return(c(as.integer(m[p[[2]] + 1]), as.integer(m[p[[3]] + 1])))
  }
  NULL
}

detect_period <- function(origins)
  if (length(origins) && all(vapply(origins, function(o) !is.null(parse_quarter(o)), TRUE))) "quarter" else "year"

# Verdichtet ein Quartalsdreieck zu einem Jahresdreieck. Der Jahreswert eines Anfalljahres im
# Abwicklungsjahr k ist die Summe seiner vier Anfallquartale zum Stand Ende des Kalenderjahres
# (Anfalljahr + k - 1). Nur Stichtage zum Jahresende werden verwendet.
aggregate_to_years <- function(tri) {
  qs <- lapply(tri$origins, parse_quarter)
  if (any(vapply(qs, is.null, TRUE)))
    stop("F\u00fcr die Verdichtung auf Jahre m\u00fcssen alle Anfallperioden Quartale sein (z. B. 2023Q1).")
  yrs <- vapply(qs, `[`, 0L, 1); qn <- vapply(qs, `[`, 0L, 2)
  t0 <- yrs * 4 + qn - 1                                   # Kalenderquartal des Anfalls
  last_cal <- max(t0 + tri$latest_idx - 1)                 # Datenstand
  end <- last_cal - ((last_cal - 3) %% 4)                  # letztes Jahresende <= Datenstand
  notes <- character(0)
  if (end != last_cal)
    notes <- c(notes, sprintf("Der Datenstand endet im Quartal %dQ%d. Die Jahresverdichtung verwendet den Stand zum 31.12.%d; sp\u00e4tere Quartale gehen nicht ein.",
                              last_cal %/% 4, last_cal %% 4 + 1, end %/% 4))
  years <- sort(unique(yrs[t0 <= end]))
  K <- max((end - (years * 4 + 3)) %/% 4 + 1)
  cum <- matrix(NA_real_, length(years), K)
  incomplete <- character(0)
  for (a in seq_along(years)) {
    rows <- which(yrs == years[a])
    if (length(rows) < 4) incomplete <- c(incomplete, as.character(years[a]))
    for (k in seq_len(K)) {
      e <- years[a] * 4 + 3 + 4 * (k - 1)
      if (e > end) break
      total <- 0
      for (i in rows) {
        j <- min(e - t0[i] + 1, tri$J)                     # Abwicklungsquartal (1-basiert)
        v <- tri$cum[i, j]
        if (is.na(v)) v <- tri$cum[i, tri$latest_idx[i]]
        total <- total + v
      }
      cum[a, k] <- total
    }
  }
  if (length(incomplete))
    notes <- c(notes, paste0("Anfalljahre mit weniger als vier Quartalen im Dreieck: ", paste(incomplete, collapse = ", "), "."))
  list(tri = make_triangle(cum, as.character(years), as.character(seq_len(K)), period = "year"), notes = notes)
}

# Periodizitaet setzen ("auto", "year", "quarter") und auf Wunsch Quartale zu Jahren verdichten
prepare_triangle <- function(tri, period = "auto", to_years = FALSE) {
  per <- if (period == "auto") detect_period(tri$origins) else period
  tri <- make_triangle(tri$cum, tri$origins, tri$devs, period = per)
  notes <- character(0)
  if (per == "quarter" && isTRUE(to_years)) {
    ag <- aggregate_to_years(tri)
    tri <- ag$tri; notes <- c("Quartalsdaten wurden zu einem Jahresdreieck verdichtet.", ag$notes)
  }
  list(tri = tri, notes = notes)
}

# Ordner dieses Skripts (fuer die Beispieldateien)
script_dir <- function() {
  f <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NULL)
  if (is.null(f)) {
    a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    if (length(a)) f <- normalizePath(sub("^--file=", "", a[1]))
  }
  if (is.null(f)) getwd() else dirname(f)
}
DATA_DIR <- script_dir()

# Fiktives Quartalsdreieck (Beispieldatei neben dem Skript) mit Quartalspraemien
demo_quarterly <- function(to_years = FALSE, dir = DATA_DIR)
  read_excel_input(file.path(dir, "beispiel_quartale.xlsx"), period = "quarter", to_years = to_years)

# -----------------------------------------------------------------------------
# 1) Chain Ladder
# -----------------------------------------------------------------------------
chain_ladder <- function(tri, tail = 1) {
  C <- tri$cum; I <- tri$I; J <- tri$J
  f <- rep(1, J - 1); S <- numeric(J - 1); n <- integer(J - 1)
  for (j in seq_len(J - 1)) {
    m <- tri$obs[, j + 1]
    S[j] <- sum(C[m, j]); n[j] <- sum(m)
    f[j] <- if (S[j] > 0) sum(C[m, j + 1]) / S[j] else 1
  }
  cdf <- c(rev(cumprod(rev(f))), 1) * tail          # cdf[j] = f_j * ... * f_{J-1} * tail
  full <- C
  for (i in seq_len(I)) {
    if (tri$latest_idx[i] < J)
      for (k in (tri$latest_idx[i] + 1):J) full[i, k] <- full[i, k - 1] * f[k - 1]
  }
  ult <- unname(tri$latest * cdf[tri$latest_idx])
  list(f = f, S = S, n = n, cdf = cdf, full = full, ultimate = ult,
       reserve = ult - tri$latest, tail = tail)
}

# -----------------------------------------------------------------------------
# 2) Mack
# -----------------------------------------------------------------------------
mack <- function(tri, cl) {
  C <- tri$cum; I <- tri$I; J <- tri$J
  f <- cl$f; S <- cl$S; n <- cl$n; full <- cl$full
  ult <- full[, J]
  sigma2 <- rep(NA_real_, J - 1)
  for (j in seq_len(J - 1)) {
    if (n[j] > 1) {
      m <- tri$obs[, j + 1]
      Fj <- C[m, j + 1] / C[m, j]
      sigma2[j] <- sum(C[m, j] * (Fj - f[j])^2) / (n[j] - 1)
    }
  }
  # Extrapolation nach Mack (1993)
  for (j in seq_len(J - 1)) {
    if (is.na(sigma2[j])) {
      if (j >= 3 && sigma2[j - 2] > 0) {
        sigma2[j] <- min(sigma2[j - 1]^2 / sigma2[j - 2], sigma2[j - 2], sigma2[j - 1])
      } else if (j >= 2) sigma2[j] <- sigma2[j - 1] else sigma2[j] <- 0
    }
  }
  term <- function(k, c_ik) {
    a <- if (c_ik > 0) 1 / c_ik else 0
    b <- if (S[k] > 0) 1 / S[k] else 0
    sigma2[k] / f[k]^2 * (a + b)
  }
  mse <- numeric(I)
  for (i in seq_len(I)) {
    d <- tri$latest_idx[i]
    if (d < J) mse[i] <- ult[i]^2 * sum(vapply(d:(J - 1), function(k) term(k, full[i, k]), 0))
  }
  total <- sum(mse)
  for (i in seq_len(I)) {
    d <- tri$latest_idx[i]
    if (d < J && i < I) {
      ks <- d:(J - 1); ks <- ks[S[ks] > 0]
      total <- total + ult[i] * sum(ult[(i + 1):I]) * sum(2 * sigma2[ks] / f[ks]^2 / S[ks])
    }
  }
  list(sigma2 = sigma2, se = unname(sqrt(mse)) * cl$tail, se_total = unname(sqrt(total)) * cl$tail)
}

# -----------------------------------------------------------------------------
# 3-7) Praemienbasierte Verfahren
# -----------------------------------------------------------------------------
cape_cod <- function(tri, cdf_d, premiums) {
  elr_cc <- sum(tri$latest) / sum(premiums / cdf_d)
  ult <- tri$latest + elr_cc * premiums * (1 - 1 / cdf_d)
  list(elr = elr_cc, ultimate = ult, reserve = ult - tri$latest)
}

expected_loss_ratio <- function(tri, premiums, elr) {
  ult <- premiums * elr
  list(ultimate = ult, reserve = ult - tri$latest)
}

# Bornhuetter-Ferguson: Reserve = A-priori-Endschaden x noch ausstehender Anteil (1 - 1/CDF)
bornhuetter_ferguson <- function(tri, cdf_d, prior_ult) {
  u <- tri$latest + (1 - 1 / cdf_d) * prior_ult
  list(ultimate = u, reserve = u - tri$latest)
}

additive <- function(tri, premiums, tail = 1) {
  m <- vapply(seq_len(tri$J), function(k) {
    msk <- tri$obs[, k]; sum(tri$inc[msk, k]) / sum(premiums[msk])
  }, 0)
  res <- vapply(seq_len(tri$I), function(i) {
    d <- tri$latest_idx[i]; if (d < tri$J) premiums[i] * sum(m[(d + 1):tri$J]) else 0
  }, 0)
  ult <- (tri$latest + res) * tail
  list(m = m, ultimate = ult, reserve = ult - tri$latest)
}

# -----------------------------------------------------------------------------
# 8) ODP-Bootstrap (England & Verrall 2002)
# -----------------------------------------------------------------------------
odp_bootstrap <- function(tri, cl, n_sims = 5000, seed = 42) {
  I <- tri$I; J <- tri$J; d <- tri$latest_idx; obs <- tri$obs; f <- cl$f
  fit <- matrix(NA_real_, I, J)
  for (i in seq_len(I)) {
    fit[i, d[i]] <- tri$latest[i]
    if (d[i] > 1) for (k in (d[i] - 1):1) fit[i, k] <- fit[i, k + 1] / f[k]
  }
  m <- to_incremental(fit)
  r <- ifelse(obs & !is.na(m) & m != 0, (tri$inc - m) / sqrt(abs(m)), NA)
  N <- sum(obs); p <- I + J - 1; dof <- N - p
  if (dof <= 0) stop("Dreieck zu klein f\u00fcr den Bootstrap (keine Freiheitsgrade).")
  phi <- sum(r^2, na.rm = TRUE) / dof
  pool <- r[obs] * sqrt(N / dof)
  pool <- pool[!is.na(pool)]
  nz <- pool[abs(pool) > 1e-10]
  if (length(nz)) pool <- nz

  set.seed(seed)
  mf <- ifelse(is.na(m), 0, m)
  cells <- which(obs)                                   # beobachtete Zellen (spaltenweise)
  # Pseudo-Inkremente: n_sims x I x J
  X <- array(NA_real_, c(n_sims, I, J))
  rs <- matrix(sample(pool, n_sims * length(cells), replace = TRUE), n_sims)
  Xobs <- sweep(rs, 2, sqrt(abs(mf[cells])), "*") + rep(mf[cells], each = n_sims)
  ij <- arrayInd(cells, c(I, J))
  for (c in seq_along(cells)) X[, ij[c, 1], ij[c, 2]] <- Xobs[, c]
  Cs <- X
  for (k in 2:J) Cs[, , k] <- Cs[, , k - 1] + X[, , k]

  fs <- matrix(1, n_sims, J - 1)
  for (j in seq_len(J - 1)) {
    msk <- which(obs[, j + 1])
    num <- rowSums(Cs[, msk, j + 1, drop = FALSE]); den <- rowSums(Cs[, msk, j, drop = FALSE])
    fs[, j] <- ifelse(den > 0, num / ifelse(den > 0, den, 1), 1)
  }
  full <- Cs
  for (k in 2:J) {
    rows <- which(d < k)
    if (length(rows)) full[, rows, k] <- full[, rows, k - 1] * fs[, k - 1]
  }
  res_origin <- matrix(0, n_sims, I)
  for (i in seq_len(I)) {
    if (d[i] < J) for (k in (d[i] + 1):J) {
      mu <- full[, i, k] - full[, i, k - 1]
      res_origin[, i] <- res_origin[, i] +
        sign(mu) * stats::rgamma(n_sims, shape = pmax(abs(mu) / phi, 1e-12), scale = phi)
    }
  }
  res_origin <- res_origin + full[, , J] * (cl$tail - 1)
  list(phi = phi, res_origin = res_origin, res_total = rowSums(res_origin))
}

# -----------------------------------------------------------------------------
# Alles rechnen
# -----------------------------------------------------------------------------
lognormal_quantiles <- function(mean, se) {
  if (is.na(se) || mean <= 0 || se <= 0) return(setNames(rep(NA_real_, 3), names(Z_QUANTILES)))
  s2 <- log(1 + (se / mean)^2); mu <- log(mean) - s2 / 2
  exp(mu + Z_QUANTILES * sqrt(s2))
}

compute_all <- function(tri, premiums = NULL, elr = NULL, tail = 1,
                        n_sims = 5000, seed = 42, bootstrap = NULL, benchmark = NULL) {
  cl <- chain_ladder(tri, tail)
  mk <- mack(tri, cl)
  cdf_d <- cl$cdf[tri$latest_idx]
  fac <- data.frame(`Faktor f_j` = c(cl$f, tail), CDF = cl$cdf, `Mack sigma^2` = c(mk$sigma2, NA),
                    check.names = FALSE,
                    row.names = c(paste0(head(tri$devs, -1), "->", tri$devs[-1]), "Tail"))
  names(fac)[3] <- "Mack sigma\u00b2"
  reserves <- list(`Chain Ladder` = cl$reserve)
  ultimates <- list(`Chain Ladder` = cl$ultimate)
  se_total <- c(`Chain Ladder` = mk$se_total)
  notes <- character(0); elr_used <- cc_elr <- NULL

  if (!is.null(premiums)) {
    cc <- cape_cod(tri, cdf_d, premiums); cc_elr <- cc$elr
    elr_used <- if (is.null(elr)) cc_elr else elr
    if (is.null(elr)) notes <- c(notes, sprintf(paste0(
      "A-priori-Schadenquote nicht angegeben -> Cape-Cod-Quote %.2f%% verwendet. Dann sind die ",
      "Gesamtreserven von ELR, BF und Cape Cod identisch (mathematische Identit\u00e4t); f\u00fcr einen ",
      "aussagekr\u00e4ftigen Vergleich eine eigene Quote setzen."), 100 * cc_elr))
    prior <- premiums * elr_used
    meth <- list(
      ELR = expected_loss_ratio(tri, premiums, elr_used),
      `Bornhuetter-Ferguson` = bornhuetter_ferguson(tri, cdf_d, prior),
      `Cape Cod` = cc,
      Additiv = additive(tri, premiums, tail))
    for (nm in names(meth)) { reserves[[nm]] <- meth[[nm]]$reserve; ultimates[[nm]] <- meth[[nm]]$ultimate }
  } else {
    notes <- c(notes, "Keine Pr\u00e4mien angegeben -> ELR, BF, Cape Cod und Additiv \u00fcbersprungen.")
  }

  bs <- bootstrap
  if (is.null(bs) && n_sims > 0) bs <- odp_bootstrap(tri, cl, n_sims, seed)
  if (!is.null(bs)) {
    nm <- "ODP-Bootstrap (Mittel)"
    reserves[[nm]] <- colMeans(bs$res_origin)
    ultimates[[nm]] <- tri$latest + colMeans(bs$res_origin)
    se_total[nm] <- stats::sd(bs$res_total)
  }

  if (!is.null(benchmark)) {
    benchmark <- as.numeric(benchmark)
    if (length(benchmark) != tri$I || anyNA(benchmark))
      stop("Manuelle Reserve: f\u00fcr jedes Anfalljahr wird genau ein Wert ben\u00f6tigt.")
    reserves[[BENCHMARK]] <- benchmark
    ultimates[[BENCHMARK]] <- tri$latest + benchmark
  }

  res_df <- data.frame(Diagonale = tri$latest, `Entw.-Grad CL` = 1 / cdf_d, reserves,
                       `Mack SE` = mk$se, check.names = FALSE, row.names = tri$origins)
  if (!is.null(bs)) res_df$`Bootstrap SE` <- apply(bs$res_origin, 2, stats::sd)
  tot <- colSums(res_df)
  tot["Entw.-Grad CL"] <- sum(tri$latest) / sum(cl$ultimate)
  tot["Mack SE"] <- mk$se_total
  if (!is.null(bs)) tot["Bootstrap SE"] <- stats::sd(bs$res_total)
  res_df["Summe", ] <- tot

  ult_df <- data.frame(ultimates, check.names = FALSE, row.names = tri$origins)
  ult_df["Summe", ] <- colSums(ult_df)

  cl_total <- sum(reserves[["Chain Ladder"]])
  comp <- do.call(rbind, lapply(names(reserves), function(nm) {
    t <- sum(reserves[[nm]]); se <- if (nm %in% names(se_total)) se_total[[nm]] else NA
    q <- setNames(rep(NA_real_, 3), paste("Q", names(Z_QUANTILES)))
    if (nm == "Chain Ladder") q[] <- lognormal_quantiles(t, mk$se_total)
    if (startsWith(nm, "ODP-Bootstrap"))
      q[] <- stats::quantile(bs$res_total, c(0.75, 0.95, 0.995), names = FALSE, type = 7)
    row <- data.frame(Reserve = t, Ultimate = t + sum(tri$latest),
                      `Abw. zu CL` = if (cl_total != 0) t / cl_total - 1 else NA,
                      SE = se, VK = if (!is.na(se) && t != 0) se / t else NA,
                      check.names = FALSE, row.names = nm)
    if (!is.null(benchmark)) row$`Abw. zu Benchmark` <- if (sum(benchmark) != 0) t / sum(benchmark) - 1 else NA
    cbind(row, t(q))
  }))

  bench <- if (!is.null(benchmark)) benchmark_assessment(tri, reserves, mk, bs, benchmark)
  list(tri = tri, cl = cl, mk = mk, bs = bs, cdf_d = cdf_d, fac = fac, reserves = reserves,
       ultimates = ultimates, res_df = res_df, ult_df = ult_df, comp = comp, notes = notes,
       elr_used = elr_used, cc_elr = cc_elr, premiums = premiums, benchmark = benchmark, bench = bench,
       period = tri$period, labels = tri$labels)
}

# -----------------------------------------------------------------------------
# Manuelle Reserve (Benchmark): Einordnung
# -----------------------------------------------------------------------------
# P(Reserve <= x) unter Lognormalverteilung mit gegebenem Mittelwert und Standardfehler
lognormal_cdf <- function(x, mean, se) {
  if (is.na(se) || mean <= 0 || se <= 0) return(NA_real_)
  if (x <= 0) return(0)
  s2 <- log(1 + (se / mean)^2)
  stats::plnorm(x, meanlog = log(mean) - s2 / 2, sdlog = sqrt(s2))
}

# Je Anfalljahr und gesamt: Abweichung zu Chain Ladder und Sicherheitsniveau
# (Anteil der Szenarien, den die manuelle Reserve abdeckt)
benchmark_assessment <- function(tri, reserves, mk, bs, benchmark) {
  cl <- reserves[["Chain Ladder"]]
  rows <- lapply(c(seq_len(tri$I), 0), function(i) {
    if (i == 0) { b <- sum(benchmark); c <- sum(cl); se <- mk$se_total; sim <- if (!is.null(bs)) bs$res_total }
    else { b <- benchmark[i]; c <- cl[i]; se <- mk$se[i]; sim <- if (!is.null(bs)) bs$res_origin[, i] }
    data.frame(`Manuelle Reserve` = b, `Chain Ladder` = c, `Differenz zu CL` = b - c,
               `Abw. zu CL` = if (abs(c) > 1e-9) b / c - 1 else NA,
               `Niveau Mack` = if (c > 0) lognormal_cdf(b, c, se) else NA,
               `Niveau Bootstrap` = if (!is.null(sim) && stats::sd(sim) > 0) mean(sim <= b + 1e-9) else NA,
               check.names = FALSE, row.names = if (i == 0) "Summe" else tri$origins[i])
  })
  do.call(rbind, rows)
}

# Kurze Aussagen zur manuellen Reserve
benchmark_summary <- function(r) {
  b <- r$bench
  if (is.null(b)) return(character(0))
  tot <- b["Summe", ]
  pct0 <- function(x) paste0(formatC(100 * x, format = "f", digits = 0), "%")
  out <- sprintf("Die manuelle Reserve von %s liegt %s gegen\u00fcber Chain Ladder (%s).",
                 de(tot$`Manuelle Reserve`), de_pct(tot$`Abw. zu CL`, sign = TRUE), de(tot$`Chain Ladder`))
  lv <- c(if (!is.na(tot$`Niveau Bootstrap`)) paste(pct0(tot$`Niveau Bootstrap`), "der Bootstrap-Szenarien"),
          if (!is.na(tot$`Niveau Mack`)) paste(pct0(tot$`Niveau Mack`), "nach Mack (Lognormal)"))
  if (length(lv)) out <- c(out, paste0("Sie deckt ", paste(lv, collapse = " bzw. "), " ab."))
  others <- r$comp$Reserve[rownames(r$comp) != BENCHMARK]
  below <- sum(others < tot$`Manuelle Reserve`)
  out <- c(out, sprintf("%d von %d Verfahren liefern eine niedrigere Gesamtreserve, %d eine h\u00f6here (Spanne %s bis %s).",
                        below, length(others), length(others) - below, de(min(others)), de(max(others))))
  per <- b[rownames(b) != "Summe", ]
  d <- setNames(per$`Differenz zu CL`, rownames(per))
  if (max(abs(d)) > 0) {
    top <- names(sort(abs(d), decreasing = TRUE))[1:min(2, length(d))]
    parts <- sprintf("%s (%s%s)", top, ifelse(d[top] >= 0, "+", "\u2212"), de(abs(d[top])))
    out <- c(out, paste0("Die gr\u00f6\u00dften Abweichungen zu Chain Ladder bestehen in den ", r$labels$origin_dat, " ",
                         paste(parts, collapse = " und "), "."))
  }
  out
}

# Einzelnes Anfalljahr: alle Verfahren
origin_detail <- function(r, i) {
  cl_res <- r$reserves[["Chain Ladder"]][i]
  se <- c(`Chain Ladder` = r$mk$se[i])
  if (!is.null(r$bs)) se["ODP-Bootstrap (Mittel)"] <- stats::sd(r$bs$res_origin[, i])
  df <- do.call(rbind, lapply(names(r$reserves), function(nm) {
    res <- r$reserves[[nm]][i]; ult <- r$ultimates[[nm]][i]
    bm <- if (!is.null(r$benchmark)) r$benchmark[i] else NA
    data.frame(Reserve = res, Ultimate = ult,
               `Abw. zu CL` = if (abs(cl_res) > 1e-9) res / cl_res - 1 else NA,
               `Abw. zu Benchmark` = if (!is.na(bm) && abs(bm) > 1e-9) res / bm - 1 else NA,
               SE = if (nm %in% names(se)) se[[nm]] else NA,
               Schadenquote = if (!is.null(r$premiums)) ult / r$premiums[i] else NA,
               check.names = FALSE, row.names = nm)
  }))
  if (is.null(r$premiums)) df$Schadenquote <- NULL
  if (is.null(r$benchmark)) df$`Abw. zu Benchmark` <- NULL
  df
}

# -----------------------------------------------------------------------------
# Formatierung (deutsches Zahlenformat) und Export
# -----------------------------------------------------------------------------
de <- function(x, digits = 0) {
  out <- formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
  out[is.na(x)] <- ""
  out
}

de_pct <- function(x, digits = 1, sign = FALSE) {
  out <- paste0(formatC(100 * x, format = "f", digits = digits, decimal.mark = ",",
                        flag = if (sign) "+" else ""), "%")
  out[is.na(x)] <- ""
  out
}

write_excel <- function(r, path, params = NULL) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("F\u00fcr den Excel-Export wird 'openxlsx' ben\u00f6tigt.")
  wb <- openxlsx::createWorkbook()
  add <- function(name, df) {
    openxlsx::addWorksheet(wb, name)
    openxlsx::writeData(wb, name, df, rowNames = TRUE)
  }
  if (!is.null(params)) add("Parameter", data.frame(Wert = unlist(lapply(params, function(v)
    if (is.null(v)) NA else v)), row.names = names(params)))
  add("Dreieck", as.data.frame(r$tri$cum, check.names = FALSE))
  add("CL-Projektion", as.data.frame(r$cl$full, check.names = FALSE))
  add("Faktoren", r$fac)
  add("Reserven", r$res_df)
  add("Ultimates", r$ult_df)
  add("Vergleich", r$comp)
  if (!is.null(r$bench)) add("Benchmark", r$bench)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

print_table <- function(title, df, digits = 0, pct = character(0), spct = character(0)) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
  out <- df
  for (c in names(df)) {
    out[[c]] <- if (c %in% pct) de_pct(df[[c]]) else if (c %in% spct) de_pct(df[[c]], sign = TRUE)
                else de(df[[c]], digits)
  }
  print(out, right = TRUE)
}

# -----------------------------------------------------------------------------
# Kommandozeile
# -----------------------------------------------------------------------------
parse_args <- function(args) {
  opt <- list(excel = NULL, period = "auto", `to-years` = FALSE, elr = NULL, tail = 1,
              `n-sims` = 5000, seed = 42, incremental = FALSE, `output-dir` = "reserving_output")
  i <- 1
  while (i <= length(args)) {
    key <- sub("^--", "", args[i])
    if (key == "incremental") { opt$incremental <- TRUE; i <- i + 1; next }
    if (key == "to-years") { opt$`to-years` <- TRUE; i <- i + 1; next }
    if (key %in% c("help", "h")) { opt$help <- TRUE; i <- i + 1; next }
    opt[[key]] <- args[i + 1]; i <- i + 2
  }
  for (k in c("elr", "tail", "n-sims", "seed"))
    if (!is.null(opt[[k]])) opt[[k]] <- as.numeric(opt[[k]])
  opt
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  options(width = 200)
  opt <- parse_args(args)
  if (isTRUE(opt$help)) {
    cat("Rscript loss_reserving.R [--excel daten.xlsx] [--elr 0.65] [--period auto|year|quarter] [--to-years]\n",
        "  [--tail 1.0] [--n-sims 5000] [--seed 42] [--incremental] [--output-dir reserving_output]\n",
        "Excel-Datei: Blatt 1 Sch\u00e4den (Dreieck), Blatt 2 Beitr\u00e4ge, Blatt 3 Manuelle Reserve.\n")
    return(invisible())
  }
  benchmark <- NULL
  if (!is.null(opt$excel)) {
    d <- read_excel_input(opt$excel, opt$incremental, opt$period, opt$`to-years`)
    tri <- d$tri; premiums <- d$premiums; benchmark <- d$benchmark
    for (n in c(d$notes, d$warnings)) cat("Hinweis:", n, "\n")
    cat(sprintf("Excel-Datei geladen: %s (%d %s x %d %s; Beitr\u00e4ge: %s; manuelle Reserve: %s)\n", opt$excel,
                tri$I, tri$labels$origin_pl, tri$J, tri$labels$dev_pl,
                if (is.null(premiums)) "keine" else "ja", if (is.null(benchmark)) "keine" else "ja"))
  } else {
    tri <- demo_triangle(); premiums <- DEMO_PREMIUMS
    if (is.null(opt$elr)) opt$elr <- DEMO_ELR
    cat(sprintf("Demo-Modus: RAA-Dreieck mit FIKTIVEN Pr\u00e4mien und A-priori-Quote %.0f%%.\n", 100 * opt$elr))
  }
  r <- compute_all(tri, premiums, opt$elr, opt$tail, n_sims = opt$`n-sims`, seed = opt$seed,
                   benchmark = benchmark)

  print_table("Kumuliertes Schadendreieck", as.data.frame(tri$cum, check.names = FALSE))
  print_table("Chain-Ladder-Abwicklungsfaktoren", r$fac, digits = 4)
  if (!is.null(r$elr_used))
    cat(sprintf("\nA-priori-Schadenquote: %s   Cape-Cod-Quote: %s\n", de_pct(r$elr_used, 2), de_pct(r$cc_elr, 2)))
  print_table("Reserven je Anfalljahr", r$res_df, pct = "Entw.-Grad CL")
  print_table("Endschadenst\u00e4nde (Ultimates)", r$ult_df)
  print_table("VERGLEICH DER VERFAHREN (Gesamt)", r$comp, pct = "VK", spct = c("Abw. zu CL", "Abw. zu Benchmark"))
  tots <- r$comp$Reserve; names(tots) <- rownames(r$comp)
  tots <- tots[names(tots) != BENCHMARK]
  cat(sprintf("\nSpanne der Punktsch\u00e4tzer: %s (%s) bis %s (%s), Median %s.\n",
              de(min(tots)), names(which.min(tots)), de(max(tots)), names(which.max(tots)), de(stats::median(tots))))
  for (n in r$notes) cat("Hinweis:", n, "\n")
  if (!is.null(r$bench)) {
    print_table("EINORDNUNG DER MANUELLEN RESERVE (Benchmark)", r$bench,
                pct = c("Niveau Mack", "Niveau Bootstrap"), spct = "Abw. zu CL")
    cat("Niveau = Anteil der Szenarien, den die manuelle Reserve abdeckt",
        "(Mack: Lognormal-N\u00e4herung, Bootstrap: simulierte Verteilung).\n")
    for (l in benchmark_summary(r)) cat("-", l, "\n")
  }

  dir.create(opt$`output-dir`, showWarnings = FALSE, recursive = TRUE)
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    p <- file.path(opt$`output-dir`, "reserving_ergebnisse.xlsx"); write_excel(r, p)
    cat("\nExcel-Export:", p, "\n")
  } else {
    utils::write.csv2(r$comp, file.path(opt$`output-dir`, "vergleich.csv"))
    cat("\nCSV-Export nach", opt$`output-dir`, "(openxlsx nicht installiert)\n")
  }
  invisible(r)
}

if (sys.nframe() == 0L) main()
