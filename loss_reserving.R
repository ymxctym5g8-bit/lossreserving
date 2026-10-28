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
#   8. Munich Chain Ladder (Quarg & Mack)
#   Dazu: Ein-Jahres-Sicht (Merz & Wuethrich 2008, Re-Reserving im Bootstrap),
#         Rueckblick ueber mehrere Jahre (rollierend Actual vs Expected und CDR)
#
# Nutzung als Skript (Rscript):
#   Rscript loss_reserving.R                                   # Demo mit RAA-Dreieck
#   Rscript loss_reserving.R --excel daten.xlsx --elr 0.65
#
# Nutzung als Bibliothek:  source("loss_reserving.R"); d <- read_excel_input("daten.xlsx")
#                          r <- compute_all(d$tri, d$premiums, benchmark = d$benchmark)
#
# Eingabe: Excel-Datei mit bis zu fuenf Blaettern (Reihenfolge entscheidet, nicht der Name):
#   Blatt 1 Schaeden:              erste Spalte Anfallperiode, weitere Spalten Abwicklungsperioden,
#                                  zukuenftige Zellen leer
#   Blatt 2 Beitraege:             Anfallperiode, Praemie                   (optional)
#   Blatt 3 Manuelle Reserve:      Anfallperiode, Manuelle Reserve          (optional)
#   Blatt 4 Gemeldete Schaeden:    Dreieck wie Blatt 1 (Munich Chain Ladder) (optional)
#   Blatt 5 Manuelle AvE und CDR:  Kalenderjahr, Erwartete Zahlung, CDR      (optional, Rueckblick)
# Pakete:  readxl (Excel-Upload), openxlsx (Excel-Export).
# =============================================================================

# -----------------------------------------------------------------------------
# Beispieldaten: RAA-Dreieck (kumuliert). Referenz: CL-Reserve 52.135, Mack-SE 26.909.
# Praemien und A-priori-Quote sind FIKTIV.
# -----------------------------------------------------------------------------
# Texte enthalten Umlaute: ohne UTF-8-Zeichensatz (z. B. Unix mit Locale "C") auf UTF-8 umstellen
if (!isTRUE(l10n_info()[["UTF-8"]]) && .Platform$OS.type == "unix")
  invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "C.UTF-8")))

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
RAA_COUNT <- list(
  c(56, 92, 121, 131, 150, 180, 200, 207, 207, 209),
  c(1, 48, 60, 119, 153, 173, 172, 180, 186),
  c(38, 100, 154, 179, 208, 247, 254, 261),
  c(63, 128, 175, 236, 260, 290, 301),
  c(12, 106, 176, 246, 288, 291),
  c(17, 72, 130, 144, 176),
  c(6, 45, 122, 137),
  c(15, 77, 146),
  c(35, 60),
  c(23)
)
RAA_ORIGINS   <- 2016:2025
DEMO_PREMIUMS <- c(30000, 30500, 31000, 32000, 33000, 34000, 35000, 36000, 37000, 38000)
DEMO_ELR      <- 0.70
BENCHMARK     <- "Manuelle Reserve"
DEMO_BENCHMARK = c(0,200,700,1800,2900,4000,6500,11500,15000,20000)
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
demo_triangle_count = function() triangle_from_list(RAA_COUNT, RAA_ORIGINS)

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
EXCEL_SHEETS <- c("Sch\u00e4den", "Beitr\u00e4ge", "Manuelle Reserve", "Gemeldete Sch\u00e4den", "Manuelle AvE und CDR")

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

# Blatt 5: manuelle erwartete Zahlungen und manuelles CDR je Kalenderjahr (Summe ueber alle Anfallperioden).
# Spalten nach Reihenfolge: Kalenderjahr, Erwartete Zahlung, CDR (eine der beiden Wertspalten darf leer sein).
manual_ave_from_frame <- function(df) {
  lab <- trimws(as.character(df[[1]]))
  lab <- ifelse(grepl("^[0-9]+(\\.0+)?$", lab), sub("\\.0+$", "", lab), lab)
  erw <- cell_num(df[[2]]); cdr <- if (ncol(df) >= 3) cell_num(df[[3]]) else rep(NA_real_, length(erw))
  keep <- !is.na(lab) & lab != "" & (!is.na(erw) | !is.na(cdr))
  if (!any(keep)) return(NULL)
  lab <- lab[keep]
  if (anyDuplicated(lab)) stop(paste0("Kalenderjahr doppelt: ", paste(unique(lab[duplicated(lab)]), collapse = ", "), "."))
  data.frame(Kalenderjahr = lab, Erwartet = erw[keep], CDR = cdr[keep], stringsAsFactors = FALSE)
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
  # Blatt 4 (optional): gemeldete Schaeden im gleichen Aufbau wie Blatt 1 -> Munich Chain Ladder
  incurred <- NULL
  d4 <- read_sheet(path, 4)
  if (!is.null(d4)) incurred <- tryCatch({
    ti <- prepare_triangle(triangle_from_frame(d4, incremental), period, to_years)$tri
    if (!identical(ti$origins, tri$origins) || ti$J != tri$J || !identical(unname(ti$obs), unname(tri$obs)))
      stop("Anfall- und Abwicklungsperioden m\u00fcssen genau zu Blatt 1 passen.")
    ti
  }, error = function(e) {
    warnings <<- c(warnings, sprintf("Blatt 4 (Gemeldete Sch\u00e4den): %s Munich Chain Ladder entf\u00e4llt.", conditionMessage(e)))
    NULL
  })
  # Blatt 5 (optional): manuelle AvE und CDR je Kalenderjahr -> Rueckblick
  manual_ave <- NULL
  d5 <- read_sheet(path, 5)
  if (!is.null(d5)) manual_ave <- tryCatch(manual_ave_from_frame(d5), error = function(e) {
    warnings <<- c(warnings, paste0("Blatt 5 (Manuelle AvE und CDR): ", conditionMessage(e), " Die Werte werden nicht verwendet."))
    NULL
  })
  list(tri = tri, premiums = premiums, benchmark = benchmark, incurred = incurred, manual_ave = manual_ave,
       notes = prep$notes, warnings = warnings)
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
  read_excel_input(file.path( "beispiel_quartale.xlsx"), period = "quarter", to_years = to_years)

# -----------------------------------------------------------------------------
# 1) Chain Ladder
# -----------------------------------------------------------------------------
# Faktorauswahl: w[i, j] = TRUE, wenn der Einzelfaktor C[i, j+1] / C[i, j] in die Schaetzung eingeht.
# last_n: nur Faktoren aus den letzten n Kalenderdiagonalen; exclude: manuell ausgeschlossene Faktoren.
factor_weights <- function(tri, exclude = NULL, last_n = NULL) {
  I <- tri$I; J <- tri$J
  w <- tri$obs[, -1, drop = FALSE]
  if (!is.null(last_n) && length(last_n) == 1 && !is.na(last_n) && last_n >= 1) {
    D <- max(seq_len(I) + tri$latest_idx - 1)                     # juengste Diagonale
    diag_of <- outer(seq_len(I), seq_len(J - 1), "+")             # Diagonale der Zelle (i, j+1)
    w <- w & diag_of > D - last_n
  }
  if (!is.null(exclude) && identical(dim(exclude), dim(w))) w <- w & !exclude
  w
}

# Einzelfaktoren (NA, wo nicht beobachtet; Inf/NaN bei Wert 0 im Nenner)
link_ratios <- function(tri) {
  F <- tri$cum[, -1, drop = FALSE] / tri$cum[, -tri$J, drop = FALSE]
  F[!tri$obs[, -1, drop = FALSE]] <- NA
  F
}

chain_ladder <- function(tri, tail = 1, w = NULL) {
  C <- tri$cum; I <- tri$I; J <- tri$J
  if (is.null(w)) w <- tri$obs[, -1, drop = FALSE]
  f <- rep(1, J - 1); S <- numeric(J - 1); n <- integer(J - 1); fallback <- integer(0)
  for (j in seq_len(J - 1)) {
    m <- w[, j]
    if (!any(m) && any(tri$obs[, j + 1])) {                     # alles ausgeschlossen -> alle verwenden
      m <- tri$obs[, j + 1]; w[, j] <- m; fallback <- c(fallback, j)
    }
    S[j] <- sum(C[m, j]); n[j] <- sum(m)
    f[j] <- if (S[j] > 0) sum(C[m, j + 1]) / S[j] else 1
  }
  cdf <- c(rev(cumprod(rev(f))), 1) * tail          # cdf[j] = f_j * ... * f_{J-1} * tail
  full <- C
  for (i in seq_len(I)) {
    if (tri$latest_idx[i] < J)
      for (k in (tri$latest_idx[i] + 1):J) full[i, k] <- full[i, k - 1] * f[k - 1]
  }
  if (tail != 1) {                                  # Tail als eigene Spalte
    full <- cbind(full, full[, J] * tail)
    colnames(full)[J + 1] <- "Tail"
  }
  ult <- unname(tri$latest * cdf[tri$latest_idx])
  list(f = f, S = S, n = n, cdf = cdf, full = full, ultimate = ult,
       reserve = ult - tri$latest, tail = tail, w = w, fallback = fallback)
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
    m <- cl$w[, j] & C[, j] > 0
    if (sum(m) > 1) {
      Fj <- C[m, j + 1] / C[m, j]
      sigma2[j] <- sum(C[m, j] * (Fj - f[j])^2) / (sum(m) - 1)
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
# 2b) Ein-Jahres-Sicht: Merz & Wuethrich (2008), MSEP des Claims Development Result (CDR)
# -----------------------------------------------------------------------------
# CDR_i = Reserve heute - (Zahlung im naechsten Jahr + neu geschaetzte Reserve in einem Jahr).
# Formeln wie ChainLadder::CDR (Autor: M. Wuethrich), verallgemeinert auf die Faktorauswahl w:
#   msep_i = U_i^2 * [ s_d / C_{i,d}  +  s_d / S_d  +  sum_{j > d} a_j * s_j / S_j ]
#   mit s_j = sigma_j^2 / f_j^2, S_j = Summe der verwendeten C_{.,j}, a_j = D_j / (S_j + D_j),
#   D_j = Summe der Diagonalwerte in Spalte j (sie gehen im naechsten Jahr in f_j ein), d = aktuelle Spalte.
# Gesamt: Summe der Prozessvarianzen + sum_{i,k} U_i U_k * Parameterterm des aelteren Jahres.
# Tail-Faktor wie bei Mack als feste Skalierung.
merz_wuethrich <- function(tri, cl, mk) {
  I <- tri$I; J <- tri$J; d <- tri$latest_idx; C <- tri$cum
  if (J < 2) return(NULL)
  f <- cl$f; S <- cl$S; s <- ifelse(f > 0, mk$sigma2 / f^2, 0)
  D <- vapply(seq_len(J - 1), function(j) sum(tri$latest[d == j]), 0)
  a <- ifelse(S + D > 0, D / (S + D), 0)
  inv <- function(x) ifelse(x > 0, 1 / x, 0)
  U <- cl$full[, J]
  par_coef <- function(dd) if (dd >= J) 0 else
    s[dd] * inv(S[dd]) + (if (dd < J - 1) sum(a[(dd + 1):(J - 1)] * s[(dd + 1):(J - 1)] * inv(S[(dd + 1):(J - 1)])) else 0)
  proc <- vapply(seq_len(I), function(i) if (d[i] < J) U[i]^2 * s[d[i]] * inv(C[i, d[i]]) else 0, 0)
  pc <- vapply(d, par_coef, 0)
  param <- pc * U^2
  # Gesamt: Parameterterm des jeweils aelteren Jahres (= groessere Entwicklungsspalte)
  P <- outer(seq_len(I), seq_len(I), function(i, k) pc[ifelse(d[i] >= d[k], i, k)])
  total_param <- sum(P * outer(U, U))
  tl <- cl$tail
  list(se = sqrt(proc + param) * tl, se_process = sqrt(proc) * tl, se_param = sqrt(param) * tl,
       se_total = sqrt(sum(proc) + total_param) * tl, se_total_process = sqrt(sum(proc)) * tl,
       se_total_param = sqrt(total_param) * tl)
}

# Ein-Jahres-Sicht zusammenstellen: Merz-Wuethrich (analytisch) und Bootstrap (Re-Reserving)
one_year_view <- function(tri, cl, mk, bs = NULL) {
  if (identical(tri$period, "quarter"))
    return(list(available = FALSE, reason = paste(
      "Die Ein-Jahres-Sicht setzt ein Jahresdreieck voraus \u2013 bei Quartalsdaten w\u00e4re der n\u00e4chste Kalenderschritt nur",
      "ein Quartal. F\u00fcr die Ein-Jahres-Sicht das Dreieck als Jahresdreieck laden (Kommandozeile: --to-years).")))
  mw <- merz_wuethrich(tri, cl, mk)
  if (is.null(mw)) return(list(available = FALSE, reason = "Das Dreieck ist zu klein f\u00fcr die Ein-Jahres-Sicht."))
  R <- cl$reserve
  tab <- data.frame(`Reserve CL` = R, `SE Ultimo (Mack)` = mk$se, `SE 1 Jahr (MW)` = mw$se,
                    check.names = FALSE, row.names = tri$origins)
  tab[["Anteil 1 Jahr / Ultimo"]] <- ifelse(mk$se > 0, mw$se / mk$se, NA)
  tab[["davon Prozess (MW)"]] <- mw$se_process
  tab[["davon Parameter (MW)"]] <- mw$se_param
  cdr_total <- NULL
  if (!is.null(bs) && !is.null(bs$cdr_origin)) {
    tab[["SE 1 Jahr (Bootstrap)"]] <- apply(bs$cdr_origin, 2, stats::sd)
    tab[["SE Ultimo (Bootstrap)"]] <- apply(bs$res_origin, 2, stats::sd)
    cdr_total <- rowSums(bs$cdr_origin)
  }
  tot <- colSums(tab)
  tot["SE Ultimo (Mack)"] <- mk$se_total; tot["SE 1 Jahr (MW)"] <- mw$se_total
  tot["Anteil 1 Jahr / Ultimo"] <- if (mk$se_total > 0) mw$se_total / mk$se_total else NA
  tot["davon Prozess (MW)"] <- mw$se_total_process; tot["davon Parameter (MW)"] <- mw$se_total_param
  if (!is.null(cdr_total)) {
    tot["SE 1 Jahr (Bootstrap)"] <- stats::sd(cdr_total); tot["SE Ultimo (Bootstrap)"] <- stats::sd(bs$res_total)
  }
  tab["Summe", ] <- tot
  Rt <- sum(R)   # alle Verluste gegenueber der heutigen Chain-Ladder-Reserve
  q995 <- function(se) { q <- lognormal_quantiles(Rt, se); if (all(is.na(q))) NA else q[[3]] - Rt }
  sm <- c(`Reserve CL` = Rt, `SE 1 Jahr (MW)` = mw$se_total, `SE Ultimo (Mack)` = mk$se_total,
          `Anteil 1 Jahr / Ultimo` = tot[["Anteil 1 Jahr / Ultimo"]],
          `Verlust 99,5 % 1 Jahr (MW, lognormal)` = q995(mw$se_total),
          `Verlust 99,5 % Ultimo (Mack, lognormal)` = q995(mk$se_total))
  if (!is.null(cdr_total)) sm <- c(sm, `CDR Mittel (Bootstrap)` = mean(cdr_total),
          `SE 1 Jahr (Bootstrap)` = stats::sd(cdr_total),
          `Verlust 99,5 % 1 Jahr (Bootstrap)` = -unname(stats::quantile(cdr_total, 0.005)),
          `Verlust 99,5 % Ultimo (Bootstrap)` = unname(stats::quantile(bs$res_total, 0.995)) - Rt)
  list(available = TRUE, mw = mw, table = tab, summary = sm, cdr_total = cdr_total,
       cdr_origin = if (!is.null(bs)) bs$cdr_origin)
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

# ELR-Verfahren: Vorjahre = Chain-Ladder-Endschaden, juengstes Anfalljahr = Praemie x Quote
elr_ultimate_mixed <- function(tri, cl, premiums, elr, n_recent = 1) {
  u <- cl$ultimate
  idx <- seq.int(tri$I - n_recent + 1, tri$I)
  u[idx] <- premiums[idx] * elr
  u
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
# Skalierte Pearson-Residuen des ODP-Modells mit den (ausgewaehlten) Chain-Ladder-Faktoren
pearson_residuals <- function(tri, cl) {
  I <- tri$I; J <- tri$J; d <- tri$latest_idx; obs <- tri$obs; f <- cl$f
  fit <- matrix(NA_real_, I, J)
  for (i in seq_len(I)) {
    fit[i, d[i]] <- tri$latest[i]
    if (d[i] > 1) for (k in (d[i] - 1):1) fit[i, k] <- fit[i, k + 1] / f[k]
  }
  m <- to_incremental(fit)
  r <- ifelse(obs & !is.na(m) & m != 0, (tri$inc - m) / sqrt(abs(m)), NA)
  N <- sum(obs); dof <- N - (I + J - 1)
  list(fit_inc = m, r = r, N = N, dof = dof,
       phi = if (dof > 0) sum(r^2, na.rm = TRUE) / dof else NA_real_,
       r_adj = if (dof > 0) r * sqrt(N / dof) else r)
}

odp_bootstrap <- function(tri, cl, n_sims = 5000, seed = 42) {
  I <- tri$I; J <- tri$J; d <- tri$latest_idx; obs <- tri$obs
  pr <- pearson_residuals(tri, cl)
  if (pr$dof <= 0) stop("Dreieck zu klein f\u00fcr den Bootstrap (keine Freiheitsgrade).")
  phi <- pr$phi; m <- pr$fit_inc
  pool <- pr$r_adj[obs]
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
    msk <- which(cl$w[, j])                             # gleiche Faktorauswahl wie im Original
    num <- rowSums(Cs[, msk, j + 1, drop = FALSE]); den <- rowSums(Cs[, msk, j, drop = FALSE])
    fs[, j] <- ifelse(den > 0, num / ifelse(den > 0, den, 1), 1)
  }
  full <- Cs
  for (k in 2:J) {
    rows <- which(d < k)
    if (length(rows)) full[, rows, k] <- full[, rows, k - 1] * fs[, k - 1]
  }
  res_origin <- matrix(0, n_sims, I)
  nxt <- matrix(0, n_sims, I)                           # simulierte Zahlung im naechsten Kalenderjahr
  for (i in seq_len(I)) {
    if (d[i] < J) for (k in (d[i] + 1):J) {
      mu <- full[, i, k] - full[, i, k - 1]
      x <- sign(mu) * stats::rgamma(n_sims, shape = pmax(abs(mu) / phi, 1e-12), scale = phi)
      if (k == d[i] + 1) nxt[, i] <- x
      res_origin[, i] <- res_origin[, i] + x
    }
  }
  res_origin <- res_origin + full[, , J] * (cl$tail - 1)
  list(phi = phi, res_origin = res_origin, res_total = rowSums(res_origin),
       cdr_origin = one_year_rereserve(tri, cl, nxt))
}

# Ein-Jahres-Sicht per Bootstrap ("actuary in the box", wie ChainLadder::CDR.BootChainLadder):
# Originaldreieck + simulierte naechste Diagonale -> Chain Ladder neu schaetzen ->
# CDR_i = Reserve heute - (Zahlung im naechsten Jahr + neue Reserve).
one_year_rereserve <- function(tri, cl, nxt) {
  I <- tri$I; J <- tri$J; d <- tri$latest_idx; C <- tri$cum; n <- nrow(nxt)
  if (J < 2) return(matrix(0, n, I))
  L1 <- sweep(nxt, 2, tri$latest, "+")                  # neue Diagonale (kumuliert)
  f1 <- matrix(1, n, J - 1)
  for (j in seq_len(J - 1)) {
    m <- cl$w[, j]                                      # bisherige Paare (gleiche Faktorauswahl)
    num <- sum(C[m, j + 1]); den <- sum(C[m, j])
    newp <- which(d == j)                               # neue Paare: Diagonale j -> j+1
    if (length(newp)) {
      num <- num + rowSums(L1[, newp, drop = FALSE]); den <- den + sum(C[newp, j])
    }
    f1[, j] <- if (den > 0) num / den else 1
  }
  cdr <- matrix(0, n, I)
  for (i in seq_len(I)) if (d[i] < J) {
    ult1 <- L1[, i]
    if (d[i] + 1 <= J - 1) for (k in (d[i] + 1):(J - 1)) ult1 <- ult1 * f1[, k]
    ult1 <- ult1 * cl$tail
    cdr[, i] <- cl$reserve[i] - (ult1 - tri$latest[i])  # positiv = Abwicklungsgewinn
  }
  cdr
}

# -----------------------------------------------------------------------------
# Alles rechnen
# -----------------------------------------------------------------------------
lognormal_quantiles <- function(mean, se) {
  if (is.na(se) || mean <= 0 || se <= 0) return(setNames(rep(NA_real_, 3), names(Z_QUANTILES)))
  s2 <- log(1 + (se / mean)^2); mu <- log(mean) - s2 / 2
  exp(mu + Z_QUANTILES * sqrt(s2))
}

compute_all <- function(tri, premiums = NULL, elr = NULL, tail = 1.05,
                        n_sims = 5000, seed = 42, bootstrap = NULL, benchmark = NULL, weights = NULL,
                        incurred = NULL) {
  cl <- chain_ladder(tri, tail, weights)
  mk <- mack(tri, cl)
  cdf_d <- cl$cdf[tri$latest_idx]
  fac <- data.frame(`CL-Faktor` = c(cl$f, tail), CDF = cl$cdf, `Mack SE` = c((mk$sigma2)^(1/2), NA),
                    `Anzahl Faktoren` = c(cl$n, NA), check.names = FALSE,
                    row.names = c(paste0(head(tri$devs, -1), "->", tri$devs[-1]), "Tail"))
  names(fac)[3] <- "Mack SE"
  reserves <- list(`Chain Ladder` = cl$reserve)
  ultimates <- list(`Chain Ladder` = cl$ultimate)
  se_total <- c(`Chain Ladder` = mk$se_total)
  notes <- character(0); elr_used <- cc_elr <- NULL
  if (length(cl$fallback))
    notes <- c(notes, sprintf("\u00dcbergang %s: alle Einzelfaktoren ausgeschlossen \u2013 es werden alle verwendet.",
                              paste(paste0(tri$devs[cl$fallback], "->", tri$devs[cl$fallback + 1]), collapse = ", ")))

  if (!is.null(premiums)) {
    cc <- cape_cod(tri, cdf_d, premiums); cc_elr <- cc$elr
    elr_used <- if (is.null(elr)) cc_elr else elr
    if (is.null(elr)) notes <- c(notes, sprintf(paste0(
      "A-priori-Schadenquote nicht angegeben -> Cape-Cod-Quote %.2f%% verwendet. Dann sind die ",
      "Gesamtreserven von ELR, BF und Cape Cod identisch; f\u00fcr einen ",
      "aussagekr\u00e4ftigen Vergleich eine eigene Quote setzen."), 100 * cc_elr))
    prior <- premiums * elr_used
    elr_ult   <- elr_ultimate_mixed(tri, cl, premiums, elr_used, n_recent = 1)
    meth <- list(
      #ELR = expected_loss_ratio(tri, premiums, elr_used),
      ELR = list(ultimate = elr_ult, reserve = elr_ult - tri$latest),
      `Bornhuetter-Ferguson` = bornhuetter_ferguson(tri, cdf_d, prior),
      `Cape Cod` = cc,
      Additiv = additive(tri, premiums, tail))
    for (nm in names(meth)) { reserves[[nm]] <- meth[[nm]]$reserve; ultimates[[nm]] <- meth[[nm]]$ultimate }
  } else {
    notes <- c(notes, "Keine Pr\u00e4mien angegeben -> ELR, BF, Cape Cod und Additiv \u00fcbersprungen.")
  }

  bs <- bootstrap
  if (is.null(bs) && n_sims > 0)
    bs <- tryCatch(odp_bootstrap(tri, cl, n_sims, seed), error = function(e) {
      notes <<- c(notes, paste("Bootstrap nicht m\u00f6glich:", conditionMessage(e)))
      NULL
    })
  if (!is.null(bs)) {
    nm <- "ODP-Bootstrap (Mittel)"
    reserves[[nm]] <- colMeans(bs$res_origin)
    ultimates[[nm]] <- tri$latest + colMeans(bs$res_origin)
    se_total[nm] <- stats::sd(bs$res_total)
  }

  mcl <- NULL
  if (!is.null(incurred)) {
    mcl <- tryCatch(munich_chain_ladder(tri, incurred), error = function(e) {
      notes <<- c(notes, paste("Munich Chain Ladder nicht m\u00f6glich:", conditionMessage(e))); NULL })
    if (!is.null(mcl)) {
      nm <- "Munich Chain Ladder"
      ultimates[[nm]] <- mcl$ultimate_p * tail                  # Tail wie bei Chain Ladder
      reserves[[nm]] <- ultimates[[nm]] - tri$latest
    }
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
  oy <- tryCatch(one_year_view(tri, cl, mk, bs), error = function(e)
    list(available = FALSE, reason = paste("Ein-Jahres-Sicht nicht m\u00f6glich:", conditionMessage(e))))
  list(tri = tri, cl = cl, mk = mk, bs = bs, oy = oy, cdf_d = cdf_d, fac = fac, reserves = reserves,
       ultimates = ultimates, res_df = res_df, ult_df = ult_df, comp = comp, notes = notes,
       elr_used = elr_used, cc_elr = cc_elr, premiums = premiums, benchmark = benchmark, bench = bench, w = cl$w,
       mcl = mcl, incurred = incurred,
       period = tri$period, labels = tri$labels)
}

# -----------------------------------------------------------------------------
# Munich Chain Ladder (Quarg & Mack 2004): bezahlte und gemeldete Schaeden gemeinsam
# -----------------------------------------------------------------------------
# Streuungsparameter wie Mack: Schaetzung je Spalte, fehlende Werte nach Mack (1993) extrapoliert
mack_extrapolate <- function(v) {
  for (j in seq_along(v)) if (is.na(v[j])) {
    if (j >= 3 && isTRUE(v[j - 2] > 0)) v[j] <- min(v[j - 1]^2 / v[j - 2], v[j - 2], v[j - 1])
    else if (j >= 2) v[j] <- v[j - 1] else v[j] <- 0
  }
  v
}

munich_chain_ladder <- function(tri_p, tri_i) {
  if (!identical(tri_p$origins, tri_i$origins) || tri_p$J != tri_i$J)
    stop("Bezahlte und gemeldete Sch\u00e4den m\u00fcssen dieselben Anfall- und Abwicklungsperioden haben.")
  if (!identical(tri_p$obs, tri_i$obs))
    stop("Bezahlte und gemeldete Sch\u00e4den m\u00fcssen dieselben Zellen enthalten.")
  P <- tri_p$cum; Inc <- tri_i$cum; I <- tri_p$I; J <- tri_p$J; obs <- tri_p$obs
  clp <- chain_ladder(tri_p); cli <- chain_ladder(tri_i)
  sp <- sqrt(mack(tri_p, clp)$sigma2); si <- sqrt(mack(tri_i, cli)$sigma2)
  fp <- clp$f; fi <- cli$f
  # Verhaeltnisse Q = P/I und Q^-1 = I/P je Abwicklungsperiode
  q <- qinv <- rho_i2 <- rho_p2 <- rep(NA_real_, J)
  for (s in seq_len(J)) {
    m <- obs[, s] & P[, s] > 0 & Inc[, s] > 0
    if (!any(m)) next
    q[s] <- sum(P[m, s]) / sum(Inc[m, s]); qinv[s] <- 1 / q[s]
    if (sum(m) > 1) {
      rho_i2[s] <- sum(Inc[m, s] * (P[m, s] / Inc[m, s] - q[s])^2) / (sum(m) - 1)
      rho_p2[s] <- sum(P[m, s] * (Inc[m, s] / P[m, s] - qinv[s])^2) / (sum(m) - 1)
    }
  }
  rho_i <- sqrt(mack_extrapolate(rho_i2)); rho_p <- sqrt(mack_extrapolate(rho_p2))
  # Residuen und Korrelationsparameter lambda
  res <- list(); rp <- rq <- ri <- rqi <- numeric(0)
  for (s in seq_len(J - 1)) for (i in which(obs[, s + 1] & P[, s] > 0 & Inc[, s] > 0)) {
    if (sp[s] > 0 && rho_p[s] > 0) {
      rp <- c(rp, (P[i, s + 1] / P[i, s] - fp[s]) / sp[s] * sqrt(P[i, s]))
      rq <- c(rq, (Inc[i, s] / P[i, s] - qinv[s]) / rho_p[s] * sqrt(P[i, s]))
    }
    if (si[s] > 0 && rho_i[s] > 0) {
      ri <- c(ri, (Inc[i, s + 1] / Inc[i, s] - fi[s]) / si[s] * sqrt(Inc[i, s]))
      rqi <- c(rqi, (P[i, s] / Inc[i, s] - q[s]) / rho_i[s] * sqrt(Inc[i, s]))
    }
  }
  # Paare mit Faktor-Residuum 0 (Uebergang mit nur einem Einzelfaktor) tragen keine Information
  kp <- abs(rp) > 1e-12; ki <- abs(ri) > 1e-12
  rp <- rp[kp]; rq <- rq[kp]; ri <- ri[ki]; rqi <- rqi[ki]
  lam_p <- if (length(rq) && sum(rq^2) > 0) sum(rq * rp) / sum(rq^2) else 0
  lam_i <- if (length(rqi) && sum(rqi^2) > 0) sum(rqi * ri) / sum(rqi^2) else 0
  # Projektion: beide Dreiecke gemeinsam, Zelle fuer Zelle
  fullP <- P; fullI <- Inc
  for (i in seq_len(I)) {
    d <- tri_p$latest_idx[i]
    if (d < J) for (s in d:(J - 1)) {
      ps <- fullP[i, s]; is <- fullI[i, s]
      adj_p <- if (rho_p[s] > 0 && ps > 0) lam_p * sp[s] / rho_p[s] * (is / ps - qinv[s]) else 0
      adj_i <- if (rho_i[s] > 0 && is > 0) lam_i * si[s] / rho_i[s] * (ps / is - q[s]) else 0
      fullP[i, s + 1] <- ps * (fp[s] + adj_p)
      fullI[i, s + 1] <- is * (fi[s] + adj_i)
    }
  }
  ult_p <- unname(fullP[, J]); ult_i <- unname(fullI[, J])
  tab <- data.frame(`Bezahlt` = tri_p$latest, `Gemeldet` = tri_i$latest,
                    `P/I aktuell` = tri_p$latest / tri_i$latest,
                    `CL bezahlt` = clp$ultimate, `CL gemeldet` = cli$ultimate,
                    `MCL bezahlt` = ult_p, `MCL gemeldet` = ult_i,
                    `P/I CL` = clp$ultimate / cli$ultimate, `P/I MCL` = ult_p / ult_i,
                    `Reserve MCL` = ult_p - tri_p$latest, check.names = FALSE, row.names = tri_p$origins)
  tot <- colSums(tab); tot["P/I aktuell"] <- sum(tri_p$latest) / sum(tri_i$latest)
  tot["P/I CL"] <- sum(clp$ultimate) / sum(cli$ultimate); tot["P/I MCL"] <- sum(ult_p) / sum(ult_i)
  tab["Summe", ] <- tot
  list(lambda_p = lam_p, lambda_i = lam_i, q = q, rho_p = rho_p, rho_i = rho_i, sigma_p = sp, sigma_i = si,
       full_p = fullP, full_i = fullI, ultimate_p = ult_p, ultimate_i = ult_i, reserve = ult_p - tri_p$latest,
       ibnr_i = ult_i - tri_i$latest, cl_p = clp, cl_i = cli, table = tab,
       resid = list(paid = data.frame(q = rq, f = rp), incurred = data.frame(q = rqi, f = ri)))
}

# -----------------------------------------------------------------------------
# Plausibilitaetspruefungen der Eingangsdaten
# -----------------------------------------------------------------------------
SEVERITY <- c("Warnung", "Pr\u00fcfen", "Hinweis")

plausibility_checks <- function(tri, cl, premiums = NULL, benchmark = NULL, incurred = NULL) {
  I <- tri$I; J <- tri$J; C <- tri$cum; L <- tri$labels
  rows <- list()
  add <- function(sev, check, per, dev, val, hint)
    rows[[length(rows) + 1]] <<- data.frame(sev, check, per, dev, val, hint, stringsAsFactors = FALSE)
  neg <- matrix(FALSE, I, J); odd <- matrix(FALSE, I, J - 1)
  steps <- paste0(head(tri$devs, -1), "->", tri$devs[-1])

  for (i in seq_len(I)) for (k in seq_len(J)[-1]) if (tri$obs[i, k] && tri$inc[i, k] < 0) {
    neg[i, k] <- TRUE
    add("Hinweis", "Negatives Inkrement", tri$origins[i], tri$devs[k], de(tri$inc[i, k]),
        "Der kumulierte Wert sinkt (z. B. Regress oder Reserveaufl\u00f6sung). Bei bezahlten Sch\u00e4den kl\u00e4ren.")
  }
  F <- link_ratios(tri)
  for (j in seq_len(J - 1)) {
    zero <- which(tri$obs[, j + 1] & C[, j] == 0)
    for (i in zero) add("Warnung", "Wert 0 vor einem \u00dcbergang", tri$origins[i], steps[j], "0",
                        "Der Einzelfaktor ist nicht berechenbar und geht nicht in die Streuung nach Mack ein.")
    ok <- which(is.finite(F[, j]))
    if (length(ok) >= 3 && cl$f[j] > 0) for (i in ok) {
      q <- F[i, j] / cl$f[j]
      if (q > 2 || q < 0.5) {
        odd[i, j] <- TRUE
        add("Pr\u00fcfen", "Auff\u00e4lliger Einzelfaktor", tri$origins[i], steps[j], de(F[i, j], 3),
            paste0(de(q, 1), "-facher CL-Faktor (", de(cl$f[j], 3), ")",
                   if (!cl$w[i, j]) " \u2013 bereits ausgeschlossen" else ""))
      }
    }
  }
  for (i in which(tri$latest == 0 & tri$latest_idx < J))
    add("Warnung", "Aktueller Stand 0", tri$origins[i], tri$devs[tri$latest_idx[i]], "0",
        "Chain Ladder sch\u00e4tzt hier keine Reserve; BF oder Cape Cod verwenden.")
  if (!is.null(incurred) && identical(dim(incurred$cum), dim(C))) {
    big <- which(tri$obs & C > incurred$cum * 1.0001, arr.ind = TRUE)
    for (r in seq_len(nrow(big))) {
      i <- big[r, 1]; k <- big[r, 2]
      add("Pr\u00fcfen", "Bezahlt gr\u00f6\u00dfer als gemeldet", tri$origins[i], tri$devs[k], de(C[i, k] / incurred$cum[i, k], 3),
          "Verh\u00e4ltnis bezahlt / gemeldet \u00fcber 1 \u2013 Einzelreserven negativ? Blatt 1 und Blatt 4 pr\u00fcfen.")
    }
  }
  if (I < 6 || J < 6)
    add("Hinweis", "Kleines Dreieck", "", "", sprintf("%d x %d", I, J),
        "Faktoren, Mack-Parameter und Bootstrap beruhen auf wenigen Werten und sind selbst unsicher.")
  if (!is.null(premiums)) {
    for (i in seq_len(I)[-1]) {
      ch <- premiums[i] / premiums[i - 1] - 1
      if (is.finite(ch) && abs(ch) > 0.3)
        add("Hinweis", "Beitragssprung", tri$origins[i], "", de_pct(ch, 1, sign = TRUE),
            "Starke Ver\u00e4nderung zum Vorperiode \u2013 Tarif- oder Bestands\u00e4nderung? F\u00fcr Cape Cod und Additiv relevant.")
    }
    lr <- cl$ultimate / premiums
    for (i in which(is.finite(lr) & (lr > 1.5 | lr < 0.2)))
      add("Pr\u00fcfen", "Implizite Schadenquote (CL)", tri$origins[i], "", de_pct(lr[i], 1),
          "Endschaden nach Chain Ladder im Verh\u00e4ltnis zur Pr\u00e4mie ungew\u00f6hnlich hoch oder niedrig.")
  }
  if (!is.null(benchmark)) for (i in seq_len(I)) {
    b <- benchmark[i]; c0 <- cl$reserve[i]
    if (b < 0) add("Warnung", "Negative manuelle Reserve", tri$origins[i], "", de(b), "Negative Reserven fachlich pr\u00fcfen.")
    else if (c0 > 0 && (b / c0 > 2 || b / c0 < 0.5))
      add("Hinweis", "Manuelle Reserve weicht stark von CL ab", tri$origins[i], "", de(b),
          paste0(de(b / c0, 2), "-fache Chain-Ladder-Reserve (", de(c0), ")"))
    else if (c0 == 0 && b > 0)
      add("Hinweis", "Manuelle Reserve bei abgeschlossener Abwicklung", tri$origins[i], "", de(b),
          "Chain Ladder erwartet keine weitere Entwicklung.")
  }
  tab <- if (length(rows)) do.call(rbind, rows) else
    data.frame(character(0), character(0), character(0), character(0), character(0), character(0))
  names(tab) <- c("Schwere", "Pr\u00fcfung", "Periode", "Abwicklung", "Wert", "Hinweis")
  tab <- tab[order(match(tab$Schwere, SEVERITY)), , drop = FALSE]
  rownames(tab) <- NULL
  list(table = tab, neg = neg, odd = odd)
}

# -----------------------------------------------------------------------------
# Diagnose der Pearson-Residuen (Anfall-, Abwicklungs-, Kalenderperiode)
# -----------------------------------------------------------------------------
calendar_labels <- function(tri) {
  n <- tri$I + tri$J - 1
  o1 <- tri$origins[1]
  if (tri$period == "quarter") {
    q <- parse_quarter(o1)
    if (!is.null(q)) { t <- q[1] * 4 + q[2] - 1 + seq_len(n) - 1; return(sprintf("%dQ%d", t %/% 4, t %% 4 + 1)) }
  }
  if (grepl("^[0-9]{4}$", o1)) return(as.character(as.integer(o1) + seq_len(n) - 1))
  as.character(seq_len(n))
}

residual_diagnostics <- function(tri, cl) {
  pr <- pearson_residuals(tri, cl)
  if (pr$dof <= 0) return(NULL)
  idx <- which(tri$obs & !is.na(pr$r_adj), arr.ind = TRUE)
  df <- data.frame(i = idx[, 1], k = idx[, 2], resid = pr$r_adj[idx] / sqrt(pr$phi))   # standardisiert
  df <- df[abs(df$resid) > 1e-10, , drop = FALSE]              # Eckzellen (Residuum 0) weglassen
  df$cal <- df$i + df$k - 1
  cl_lab <- calendar_labels(tri)
  df$origin <- tri$origins[df$i]; df$dev <- tri$devs[df$k]; df$cal_label <- cl_lab[df$cal]
  trend <- NULL
  if (length(unique(df$cal)) >= 3 && nrow(df) > 3) {
    co <- summary(stats::lm(resid ~ cal, df))$coefficients
    if ("cal" %in% rownames(co)) trend <- c(slope = co["cal", 1], t = co["cal", 3], p = co["cal", 4])
  }
  list(data = df, phi = pr$phi, trend = trend, cal_labels = cl_lab)
}

# -----------------------------------------------------------------------------
# Sensitivitaeten: Wirkung einzelner Annahmen auf die Gesamtreserve eines Verfahrens
# -----------------------------------------------------------------------------
sensitivity <- function(tri, premiums = NULL, elr = NULL, tail = 1, w = NULL, method = "Chain Ladder") {
  base_w <- if (is.null(w)) tri$obs[, -1, drop = FALSE] else w
  run <- function(elr_ = elr, tail_ = tail, w_ = base_w) {
    r <- compute_all(tri, premiums, elr_, tail_, n_sims = 0, weights = w_)
    if (method %in% names(r$reserves)) sum(r$reserves[[method]]) else NA_real_
  }
  base <- run()
  sc <- list()
  if (!is.null(premiums) && !is.null(elr)) {
    sc[["A-priori-Schadenquote \u22125 Punkte"]] <- run(elr_ = elr - 0.05)
    sc[["A-priori-Schadenquote +5 Punkte"]] <- run(elr_ = elr + 0.05)
  }
  if (tail > 1) sc[["Tail-Faktor 1,000 (keine Nachentwicklung)"]] <- run(tail_ = 1)
  sc[["Tail-Faktor +0,02"]] <- run(tail_ = tail + 0.02)
  sc[["Tail-Faktor +0,05"]] <- run(tail_ = tail + 0.05)
  F <- link_ratios(tri)
  drop_ext <- function(fun) {
    w2 <- base_w
    for (j in seq_len(ncol(w2))) {
      u <- which(w2[, j] & is.finite(F[, j]))
      if (length(u) >= 2) w2[u[fun(F[u, j])], j] <- FALSE
    }
    w2
  }
  sc[["Gr\u00f6\u00dfter Einzelfaktor je \u00dcbergang ausgeschlossen"]] <- run(w_ = drop_ext(which.max))
  sc[["Kleinster Einzelfaktor je \u00dcbergang ausgeschlossen"]] <- run(w_ = drop_ext(which.min))
  if (tri$J > 6) sc[["Nur die letzten 5 Diagonalen"]] <- run(w_ = base_w & factor_weights(tri, last_n = 5))
  sc[["Faktoren ohne j\u00fcngste Diagonale"]] <- run(w_ = base_w & !factor_weights(tri, last_n = 1))
  v <- unlist(sc)
  out <- data.frame(Reserve = v, Abweichung = v - base, `Abweichung %` = if (base != 0) v / base - 1 else NA,
                    check.names = FALSE, row.names = names(sc))
  attr(out, "base") <- base
  out
}

# -----------------------------------------------------------------------------
# Actual vs. Expected: Prognose der juengsten Diagonale mit dem Stand davor
# -----------------------------------------------------------------------------
drop_latest_diagonal <- function(tri) {
  cum <- tri$cum
  for (i in seq_len(tri$I)) cum[i, tri$latest_idx[i]] <- NA
  keep <- rowSums(!is.na(cum)) > 0
  make_triangle(cum[keep, , drop = FALSE], tri$origins[keep], tri$devs, period = tri$period)
}

# prev: Dreieck des Vorstands (NULL = aus dem aktuellen Dreieck durch Weglassen der juengsten Diagonale)
actual_vs_expected <- function(tri, premiums = NULL, elr = NULL, prev = NULL, prev_premiums = NULL,
                               last_n = NULL, exclude = NULL) {
  backtest <- is.null(prev)
  if (backtest) {
    prev <- tryCatch(drop_latest_diagonal(tri), error = function(e)
      stop("F\u00fcr den Backtest ist das Dreieck zu klein: ohne die j\u00fcngste Diagonale bleiben zu wenige Werte."))
    ex <- if (!is.null(exclude)) exclude[match(prev$origins, tri$origins), , drop = FALSE] else NULL
  } else {
    if (!all(prev$devs %in% tri$devs))
      stop("Die Abwicklungsperioden der Vorjahresdatei passen nicht zum aktuellen Dreieck.")
    if (prev$J != tri$J) {                                   # fehlende (leere) Spalten ergaenzen
      cum <- matrix(NA_real_, prev$I, tri$J, dimnames = list(prev$origins, tri$devs))
      cum[, match(prev$devs, tri$devs)] <- prev$cum
      prev <- make_triangle(cum, prev$origins, tri$devs, period = prev$period)
    }
    ex <- NULL
  }
  clp <- chain_ladder(prev, 1, factor_weights(prev, exclude = ex, last_n = last_n))
  pos <- match(prev$origins, tri$origins)
  pp <- if (!is.null(prev_premiums)) prev_premiums else if (!is.null(premiums)) premiums[pos] else NULL
  elr_bf <- NULL
  if (!is.null(pp) && !anyNA(pp)) elr_bf <- if (!is.null(elr)) elr else cape_cod(prev, clp$cdf[prev$latest_idx], pp)$elr
  rows <- list()
  for (p in seq_len(prev$I)) {
    ic <- pos[p]; d <- prev$latest_idx[p]
    if (is.na(ic) || d >= tri$J || !tri$obs[ic, d + 1]) next
    base <- prev$latest[p]
    e_cl <- if (clp$n[d] > 0) base * (clp$f[d] - 1) else NA_real_
    e_bf <- if (!is.null(elr_bf) && clp$n[d] > 0) pp[p] * elr_bf * (1 / clp$cdf[d + 1] - 1 / clp$cdf[d]) else NA_real_
    act <- tri$cum[ic, d + 1] - base
    rows[[length(rows) + 1]] <- data.frame(Stand = base, step = paste0(tri$devs[d], "->", tri$devs[d + 1]),
      ecl = e_cl, ebf = e_bf, act = act, row.names = prev$origins[p], stringsAsFactors = FALSE)
  }
  if (!length(rows)) stop("Keine Anfallperiode hat einen neueren Wert als im Vorstand.")
  tab <- do.call(rbind, rows)
  tab$dcl <- tab$act - tab$ecl
  tab$aecl <- ifelse(!is.na(tab$ecl) & tab$ecl != 0, tab$act / tab$ecl, NA)
  tab$dbf <- tab$act - tab$ebf
  tab$aebf <- ifelse(!is.na(tab$ebf) & tab$ebf != 0, tab$act / tab$ebf, NA)
  sm <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)
  ok_cl <- !is.na(tab$ecl); ok_bf <- !is.na(tab$ebf)
  tot <- data.frame(Stand = sum(tab$Stand), step = "", ecl = sm(tab$ecl), ebf = sm(tab$ebf), act = sum(tab$act),
                    dcl = sum(tab$act[ok_cl]) - sm(tab$ecl), aecl = sum(tab$act[ok_cl]) / sm(tab$ecl),
                    dbf = if (any(ok_bf)) sum(tab$act[ok_bf]) - sm(tab$ebf) else NA,
                    aebf = if (any(ok_bf)) sum(tab$act[ok_bf]) / sm(tab$ebf) else NA, row.names = "Summe")
  out <- rbind(tab, tot)
  for (c in setdiff(names(out), "step")) out[[c]] <- as.numeric(out[[c]])   # leere Spalten (ohne Praemien) numerisch halten
  names(out) <- c("Stand", "\u00dcbergang", "Erwartet CL", "Erwartet BF", "Tats\u00e4chlich", "Abw. CL", "A/E CL",
                  "Abw. BF", "A/E BF")
  list(table = out, backtest = backtest, elr_bf = elr_bf, new_origins = setdiff(tri$origins, prev$origins),
       no_factor = rownames(tab)[!ok_cl])
}

# -----------------------------------------------------------------------------
# Rueckblick ueber mehrere Jahre: Modell vor n Jahren festlegen, dann rollierend AvE und CDR
# -----------------------------------------------------------------------------
# Dreieck zum Stand vor k Kalenderjahren (die k juengsten Diagonalen entfernen)
triangle_as_of <- function(tri, k) {
  if (k <= 0) return(tri)
  D <- max(seq_len(tri$I) + tri$latest_idx - 1)
  cum <- tri$cum
  cum[outer(seq_len(tri$I), seq_len(tri$J), "+") - 1 > D - k] <- NA
  keep <- rowSums(!is.na(cum)) > 0
  make_triangle(cum[keep, , drop = FALSE], tri$origins[keep], tri$devs, period = tri$period)
}

# refit = TRUE: gleiche Einstellungen (Faktorauswahl, Tail, A-priori-Quote), Faktoren jedes Jahr neu geschaetzt.
# refit = FALSE: Faktoren (und CDF) vom Startzeitpunkt werden festgehalten.
# Je Kalenderjahr: erwartete vs. tatsaechliche Zahlung (AvE) und beobachtetes CDR = Reserve Anfang - (Zahlung + Reserve Ende),
# jeweils ueber die Anfallperioden, die zu Jahresbeginn schon bekannt waren.
rolling_backtest <- function(tri, premiums = NULL, elr = NULL, years = 5, refit = TRUE,
                             exclude = NULL, last_n = NULL, tail = 1, manual = NULL) {
  if (identical(tri$period, "quarter"))
    stop("Der R\u00fcckblick setzt ein Jahresdreieck voraus \u2013 Quartalsdaten vorher zu Jahren verdichten (Kommandozeile: --to-years).")
  years <- as.integer(years)
  D <- max(seq_len(tri$I) + tri$latest_idx - 1)
  if (is.na(years) || years < 1) stop("Mindestens ein Jahr zur\u00fcck w\u00e4hlen.")
  st0 <- tryCatch(triangle_as_of(tri, years), error = function(e) NULL)
  if (is.null(st0) || !any(st0$obs[, 2]))
    stop(paste0(years, " Jahre zur\u00fcck ist das Dreieck zu klein (h\u00f6chstens ", max_backtest_years(tri), " Jahre m\u00f6glich)."))
  states <- lapply(years:0, function(k) triangle_as_of(tri, k))          # [[1]] = Start, [[years+1]] = heute
  wts <- function(st) factor_weights(st, exclude = if (!is.null(exclude)) exclude[match(st$origins, tri$origins), , drop = FALSE],
                                     last_n = last_n)
  fits <- lapply(states, function(st) chain_ladder(st, tail, wts(st)))
  model <- function(t) if (refit) fits[[t]] else fits[[1]]
  prem_of <- function(st) if (!is.null(premiums)) premiums[match(st$origins, tri$origins)] else NULL
  p0 <- prem_of(states[[1]])
  elr_bf <- NULL
  if (!is.null(p0)) elr_bf <- if (!is.null(elr)) elr else cape_cod(states[[1]], fits[[1]]$cdf[states[[1]]$latest_idx], p0)$elr
  cal <- calendar_labels(tri)
  J <- tri$J
  detail <- list(); summ <- list()
  for (t in seq_len(years)) {
    P <- states[[t]]; N <- states[[t + 1]]; mP <- model(t); mN <- model(t + 1)
    pp <- prem_of(P)
    mw <- if (refit) tryCatch(merz_wuethrich(P, mP, mack(P, mP)), error = function(e) NULL) else NULL
    rows <- list()
    for (p in seq_len(P$I)) {
      d <- P$latest_idx[p]; ic <- match(P$origins[p], N$origins); dn <- N$latest_idx[ic]
      base <- P$latest[p]; newv <- N$latest[ic]; X <- newv - base
      if (d >= J && X == 0) next
      nf <- d < J && mP$n[d] > 0
      e_cl <- if (d < J) base * (mP$f[d] - 1) else 0
      R0 <- base * mP$cdf[d] - base; R1 <- newv * mN$cdf[dn] - newv
      r <- data.frame(Jahr = cal[D - years + t], Anfall = P$origins[p], Uebergang = if (d < J) paste0(tri$devs[d], "->", tri$devs[d + 1]) else "",
                      `Faktor vorhanden` = nf, `Reserve Anfang CL` = R0, `Erwartet CL` = e_cl, Tatsaechlich = X,
                      `Reserve Ende CL` = R1, `CDR CL` = R0 - (X + R1),
                      `SE CDR (MW)` = if (!is.null(mw)) mw$se[p] else NA_real_, check.names = FALSE, stringsAsFactors = FALSE)
      if (!is.null(elr_bf)) {
        prior <- pp[p] * elr_bf
        r$`Erwartet BF` <- if (d < J) prior * (1 / mP$cdf[d + 1] - 1 / mP$cdf[d]) else 0
        r$`Reserve Anfang BF` <- prior * (1 - 1 / mP$cdf[d])
        r$`Reserve Ende BF` <- prior * (1 - 1 / mN$cdf[dn])
        r$`CDR BF` <- r$`Reserve Anfang BF` - (X + r$`Reserve Ende BF`)
      }
      rows[[length(rows) + 1]] <- r
    }
    dt <- do.call(rbind, rows); detail[[t]] <- dt
    ok <- dt$`Faktor vorhanden` & dt$`Erwartet CL` != 0
    s <- data.frame(`Reserve Anfang CL` = sum(dt$`Reserve Anfang CL`), `Erwartet CL` = sum(dt$`Erwartet CL`),
                    Tatsaechlich = sum(dt$Tatsaechlich),
                    `A/E CL` = if (any(ok)) sum(dt$Tatsaechlich[ok]) / sum(dt$`Erwartet CL`[ok]) else NA_real_,
                    `Reserve Ende CL` = sum(dt$`Reserve Ende CL`), `CDR CL` = sum(dt$`CDR CL`),
                    `SE CDR (MW)` = if (!is.null(mw)) mw$se_total else NA_real_,
                    check.names = FALSE, row.names = cal[D - years + t])
    s$`CDR / SE` <- ifelse(!is.na(s$`SE CDR (MW)`) & s$`SE CDR (MW)` > 0, s$`CDR CL` / s$`SE CDR (MW)`, NA_real_)
    s$`ohne Faktor` <- sum(!dt$`Faktor vorhanden` & dt$Tatsaechlich != 0)
    if (!is.null(elr_bf)) {
      okb <- dt$`Erwartet BF` != 0
      s$`Erwartet BF` <- sum(dt$`Erwartet BF`)
      s$`A/E BF` <- if (any(okb)) sum(dt$Tatsaechlich[okb]) / sum(dt$`Erwartet BF`[okb]) else NA_real_
      s$`CDR BF` <- sum(dt$`CDR BF`)
    }
    summ[[t]] <- s
  }
  sm <- do.call(rbind, summ)
  sm$`Kum. CDR CL` <- cumsum(sm$`CDR CL`)
  if (!is.null(elr_bf)) sm$`Kum. CDR BF` <- cumsum(sm$`CDR BF`)
  # Manuelle Werte (Blatt 5) den Kalenderjahren zuordnen; tatsaechliche Zahlungen kommen aus dem Dreieck
  man_used <- man_unused <- character(0)
  if (!is.null(manual) && nrow(manual)) {
    i <- match(rownames(sm), manual$Kalenderjahr)
    man_used <- manual$Kalenderjahr[!is.na(match(manual$Kalenderjahr, rownames(sm)))]
    man_unused <- setdiff(manual$Kalenderjahr, rownames(sm))
    if (length(man_used)) {
      sm$`Erwartet manuell` <- manual$Erwartet[i]
      sm$`A/E manuell` <- ifelse(!is.na(sm$`Erwartet manuell`) & sm$`Erwartet manuell` != 0,
                                 sm$Tatsaechlich / sm$`Erwartet manuell`, NA_real_)
      cm <- manual$CDR[i]
      sm$`CDR manuell` <- cm
      sm$`Kum. CDR manuell` <- ifelse(is.na(cm), NA_real_, cumsum(ifelse(is.na(cm), 0, cm)))
    }
  }
  tot <- sm[1, ]; tot[] <- NA
  for (c in c("Erwartet CL", "Tatsaechlich", "CDR CL", "Erwartet BF", "CDR BF")) if (c %in% names(sm)) tot[[c]] <- sum(sm[[c]])
  if ("Erwartet manuell" %in% names(sm)) {
    okm <- !is.na(sm$`Erwartet manuell`) & sm$`Erwartet manuell` != 0
    tot$`Erwartet manuell` <- if (any(okm)) sum(sm$`Erwartet manuell`[okm]) else NA
    tot$`A/E manuell` <- if (any(okm)) sum(sm$Tatsaechlich[okm]) / sum(sm$`Erwartet manuell`[okm]) else NA
    tot$`CDR manuell` <- if (any(!is.na(sm$`CDR manuell`))) sum(sm$`CDR manuell`, na.rm = TRUE) else NA
  }
  okc <- sm$`ohne Faktor` == 0
  det_all <- do.call(rbind, detail)
  okd <- det_all$`Faktor vorhanden` & det_all$`Erwartet CL` != 0
  tot$`A/E CL` <- sum(det_all$Tatsaechlich[okd]) / sum(det_all$`Erwartet CL`[okd])
  if (!is.null(elr_bf)) { okb <- det_all$`Erwartet BF` != 0; tot$`A/E BF` <- sum(det_all$Tatsaechlich[okb]) / sum(det_all$`Erwartet BF`[okb]) }
  tot$`SE CDR (MW)` <- if (refit) sqrt(sum(sm$`SE CDR (MW)`^2)) else NA   # CDRs verschiedener Jahre sind unkorreliert
  tot$`CDR / SE` <- if (refit) tot$`CDR CL` / tot$`SE CDR (MW)` else NA
  tot$`ohne Faktor` <- sum(sm$`ohne Faktor`)
  sm["Summe", ] <- tot

  # Endschadenschaetzung (CL) je Bewertungsjahr fuer die Anfallperioden, die es zum Start schon gab
  o0 <- states[[1]]$origins
  ult <- sapply(seq_along(states), function(t) {
    st <- states[[t]]; m <- model(t); i <- match(o0, st$origins); st$latest[i] * m$cdf[st$latest_idx[i]]
  })
  ult <- matrix(ult, nrow = length(o0), dimnames = list(o0, cal[D - years + 0:years]))
  ult_df <- as.data.frame(ult, check.names = FALSE); ult_df["Summe", ] <- colSums(ult_df)
  fac <- data.frame(`Faktor Start` = fits[[1]]$f, `Anzahl Start` = fits[[1]]$n,
                    `Faktor heute` = fits[[years + 1]]$f, `Anzahl heute` = fits[[years + 1]]$n,
                    check.names = FALSE, row.names = paste0(head(tri$devs, -1), "->", tri$devs[-1]))
  fac$`Abw.` <- ifelse(fac$`Anzahl Start` > 0, fac$`Faktor heute` / fac$`Faktor Start` - 1, NA)
  de_names <- function(df) { n <- names(df); n[n == "Tatsaechlich"] <- "Tats\u00e4chlich"; n[n == "Uebergang"] <- "\u00dcbergang"; names(df) <- n; df }
  list(summary = de_names(sm), detail = de_names(det_all), ultimates = ult_df, factors = fac, refit = refit, years = years,
       start = cal[D - years], today = cal[D], elr_bf = elr_bf, start_reserve = sum(fits[[1]]$reserve),
       start_origins = o0, manual_used = man_used, manual_unused = man_unused)
}

max_backtest_years <- function(tri) {
  D <- max(seq_len(tri$I) + tri$latest_idx - 1)
  k <- 0
  for (y in seq_len(D)) {
    st <- tryCatch(triangle_as_of(tri, y), error = function(e) NULL)
    if (is.null(st) || !any(st$obs[, 2])) break
    k <- y
  }
  k
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

write_excel <- function(r, path, params = NULL, extra = list()) {
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
  if (isTRUE(r$oy$available)) {
    add("Ein-Jahres-Sicht", r$oy$table)
    openxlsx::writeData(wb, "Ein-Jahres-Sicht", data.frame(Wert = r$oy$summary, row.names = names(r$oy$summary)),
                        startRow = nrow(r$oy$table) + 4, rowNames = TRUE)
  }
  for (nm in names(extra)) if (!is.null(extra[[nm]]) && nrow(extra[[nm]])) add(nm, extra[[nm]])
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

print_table <- function(title, df, digits = 0, pct = character(0), spct = character(0), dec = list()) {
  cat("\n", title, "\n", strrep("-", nchar(title)), "\n", sep = "")
  out <- df
  for (c in names(df)) {
    out[[c]] <- if (c %in% pct) de_pct(df[[c]]) else if (c %in% spct) de_pct(df[[c]], sign = TRUE)
                else de(df[[c]], if (!is.null(dec[[c]])) dec[[c]] else digits)
  }
  print(out, right = TRUE)
}

# -----------------------------------------------------------------------------
# Kommandozeile
# -----------------------------------------------------------------------------
parse_args <- function(args) {
  opt <- list(excel = NULL, period = "auto", `to-years` = FALSE, elr = NULL, tail = 1,
              `n-sims` = 5000, seed = 42, incremental = FALSE, `output-dir` = "reserving_output",
              rueckblick = 5, `faktoren-fest` = FALSE)
  i <- 1
  while (i <= length(args)) {
    key <- sub("^--", "", args[i])
    if (key == "incremental") { opt$incremental <- TRUE; i <- i + 1; next }
    if (key == "to-years") { opt$`to-years` <- TRUE; i <- i + 1; next }
    if (key == "faktoren-fest") { opt$`faktoren-fest` <- TRUE; i <- i + 1; next }
    if (key %in% c("help", "h")) { opt$help <- TRUE; i <- i + 1; next }
    opt[[key]] <- args[i + 1]; i <- i + 2
  }
  for (k in c("elr", "tail", "n-sims", "seed", "rueckblick"))
    if (!is.null(opt[[k]])) opt[[k]] <- as.numeric(opt[[k]])
  opt
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  options(width = 200)
  opt <- parse_args(args)
  if (isTRUE(opt$help)) {
    cat("Rscript loss_reserving.R [--excel daten.xlsx] [--elr 0.65] [--period auto|year|quarter] [--to-years]\n",
        "  [--tail 1.0] [--n-sims 5000] [--seed 42] [--incremental] [--output-dir reserving_output]\n",
        "  [--rueckblick 5] [--faktoren-fest]   R\u00fcckblick: Modell vor n Jahren, dann rollierend AvE und CDR (0 = aus)\n",
        "Excel-Datei: Blatt 1 Sch\u00e4den (Dreieck), Blatt 2 Beitr\u00e4ge, Blatt 3 Manuelle Reserve.\n")
    return(invisible())
  }
  benchmark <- incurred <- manual_ave <- NULL
  if (!is.null(opt$excel)) {
    d <- read_excel_input(opt$excel, opt$incremental, opt$period, opt$`to-years`)
    tri <- d$tri; premiums <- d$premiums; benchmark <- d$benchmark; incurred <- d$incurred; manual_ave <- d$manual_ave
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
                   benchmark = benchmark, incurred = incurred)

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
  if (!is.null(r$mcl)) {
    print_table("MUNICH CHAIN LADDER (bezahlt / gemeldet)", r$mcl$table, pct = c("P/I aktuell", "P/I CL", "P/I MCL"))
    cat(sprintf("Korrelation lambda: bezahlt %s, gemeldet %s\n", de(r$mcl$lambda_p, 3), de(r$mcl$lambda_i, 3)))
  }
  if (isTRUE(r$oy$available)) {
    print_table("EIN-JAHRES-SICHT (Merz-W\u00fcthrich / CDR)", r$oy$table, pct = "Anteil 1 Jahr / Ultimo")
    sm <- r$oy$summary
    for (nm in names(sm)) cat(sprintf("  %-42s %s\n", nm, if (grepl("Anteil", nm)) de_pct(sm[[nm]]) else de(sm[[nm]])))
  } else if (!is.null(r$oy$reason)) cat("\nHinweis:", r$oy$reason, "\n")
  if (!is.null(r$bench)) {
    print_table("EINORDNUNG DER MANUELLEN RESERVE (Benchmark)", r$bench,
                pct = c("Niveau Mack", "Niveau Bootstrap"), spct = "Abw. zu CL")
    cat("Niveau = Anteil der Szenarien, den die manuelle Reserve abdeckt",
        "(Mack: Lognormal-N\u00e4herung, Bootstrap: simulierte Verteilung).\n")
    for (l in benchmark_summary(r)) cat("-", l, "\n")
  }

  rb <- NULL
  if (isTRUE(opt$rueckblick > 0) && !identical(tri$period, "quarter")) {
    yrs <- min(opt$rueckblick, max_backtest_years(tri))
    rb <- tryCatch(rolling_backtest(tri, premiums, opt$elr, yrs, refit = !isTRUE(opt$`faktoren-fest`), tail = opt$tail,
                                    manual = manual_ave),
                   error = function(e) { cat("\nHinweis: R\u00fcckblick nicht m\u00f6glich:", conditionMessage(e), "\n"); NULL })
    if (!is.null(rb)) {
      print_table(paste0("R\u00dcCKBLICK: Modell Stand ", rb$start, ", ", if (rb$refit) "Faktoren j\u00e4hrlich neu gesch\u00e4tzt" else "Faktoren festgehalten"),
                  rb$summary, dec = list(`A/E CL` = 2, `A/E BF` = 2, `A/E manuell` = 2, `CDR / SE` = 2))
      if (length(rb$manual_unused)) cat("Hinweis: manuelle Werte au\u00dferhalb des R\u00fcckblicks nicht verwendet:", paste(rb$manual_unused, collapse = ", "), "\n")
      print_table("Endschadensch\u00e4tzung je Bewertungsjahr (Chain Ladder)", rb$ultimates)
    }
  }

  dir.create(opt$`output-dir`, showWarnings = FALSE, recursive = TRUE)
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    ex <- if (!is.null(rb)) { dd <- rb$detail; rownames(dd) <- NULL
      setNames(list(rb$summary, dd, rb$ultimates), c("R\u00fcckblick", "R\u00fcckblick Detail", "R\u00fcckblick Endschaden")) } else list()
    p <- file.path(opt$`output-dir`, "reserving_ergebnisse.xlsx"); write_excel(r, p, extra = ex)
    cat("\nExcel-Export:", p, "\n")
  } else {
    utils::write.csv2(r$comp, file.path(opt$`output-dir`, "vergleich.csv"))
    cat("\nCSV-Export nach", opt$`output-dir`, "(openxlsx nicht installiert)\n")
  }
  invisible(r)
}

if (sys.nframe() == 0L) main()
