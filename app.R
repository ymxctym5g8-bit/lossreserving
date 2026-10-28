# =============================================================================
# Loss Reserving - R-Shiny-App
# =============================================================================
# Start (in R):        shiny::runApp()            # im Ordner mit app.R
# Start (Terminal):    Rscript -e "shiny::runApp(launch.browser = TRUE)"
# Benoetigt loss_reserving.R im selben Ordner.
# Pakete: shiny, bslib, plotly, readxl (Excel-Upload), openxlsx (Excel-Export)
#
# Reaktiv: Der Bootstrap haengt nur von Dreieck, Tail, Simulationen und Seed ab und
# wird bei Aenderungen an Schadenquote oder Praemien nicht
# neu gerechnet.
# =============================================================================

library(shiny)
library(bslib)
library(plotly)
source("loss_reserving.R", encoding = "UTF-8", local = TRUE)

# Feste Farbe je Verfahren (Farbe folgt dem Verfahren, nicht der Reihenfolge)
METHOD_COLORS <- c(
  "Chain Ladder" = "#2a78d6", "Bornhuetter-Ferguson" = "#eb6834", "Cape Cod" = "#1baf7a",
  "ELR" = "#e87ba4", "Additiv" = "#008300", "Munich Chain Ladder" = "#8e44ad",
  "ODP-Bootstrap (Mittel)" = "#4a3aa7", "Manuelle Reserve" = "#3d3c38")
BENCH_COLOR <- "#3d3c38"
MANUAL_COLOR <- "#b8860b"   # manuelle AvE / CDR (Blatt 5)
ACCENT <- "#2a78d6"; INK_2 <- "#52514e"; MUTED <- "#898781"; GRID <- "#e1e0d9"
SHADE <- "rgba(42,120,214,0.14)"
SIM_CHOICES <- c(0, 1000, 10000)

`%||%` <- function(a, b) if (is.null(a)) b else a

color_for <- function(nm) vapply(nm, function(n) {
  hit <- names(METHOD_COLORS)[startsWith(n, names(METHOD_COLORS))]
  if (length(hit)) METHOD_COLORS[[hit[1]]] else MUTED
}, "")

METHODS_INFO <- list(
  c("Chain Ladder", "Volumengewichtete Abwicklungsfaktoren, Hochrechnung der aktuellen Diagonale", "nein"),
  c("Mack", "Verteilungsfreies Modell hinter Chain Ladder; liefert den Standardfehler", "nein"),
  c("Expected Loss Ratio", "Endschaden = Pr\u00e4mie \u00d7 A-priori-Schadenquote; F\u00fcr Vorjahre \u00fcbernahme aus Chain Ladder", "ja"),
  c("Bornhuetter-Ferguson", "Reserve = A-priori-Endschaden \u00d7 noch ausstehender Anteil (1 \u2212 1/ Cumulative Development Factor (CDF))", "ja"),
  c("Cape Cod", "Wie BF, aber die Schadenquote wird aus den Daten gesch\u00e4tzt (Diagonale / verbrauchte Pr\u00e4mie)", "ja"),
  c("Additiv", "Inkrementelle Schadenquoten je Abwicklungsjahr, auf die Pr\u00e4mie angewendet", "ja"),
  c("ODP-Bootstrap", "Resampling der Pearson-Residuen (nach England & Verrall)", "nein")
  )
#c("Munich Chain Ladder", "Chain Ladder f\u00fcr bezahlte und gemeldete Sch\u00e4den gemeinsam; nutzt den Zusammenhang zwischen Faktoren und dem Verh\u00e4ltnis bezahlt/gemeldet (Quarg & Mack 2004)", "nein \u2013 braucht Blatt 4"),
# c("Ein-Jahres-Sicht (Merz-W\u00fcthrich)", "Standardfehler des Claims Development Result (CDR): wie stark sich die Chain-Ladder-Sch\u00e4tzung innerhalb eines Jahres \u00e4ndern kann (Solvency II); zus\u00e4tzlich als Verteilung aus dem Bootstrap", "nein")


METHOD_NOTES <- c(
  "Chain Ladder reagiert stark auf junge Anfalljahre mit kleinen Diagonalwerten; BF und Cape Cod stabilisieren dort \u00fcber die Pr\u00e4mie.",
  "Wird die Cape-Cod-Quote als A-priori-Quote verwendet, sind die Gesamtreserven von ELR, BF und Cape Cod rechnerisch identisch.",
  "Der Bootstrap setzt ein ODP-Modell voraus; viele negative Werte oder volatile Entwicklungen machen ihn unzuverl\u00e4ssig.",
  "Der Bootstrap ist zufallsbasiert; mit gleichem Zufallsstartwert und gleicher Anzahl Simulationen sind die Ergebnisse reproduzierbar.",
  "Manuelle Reserve (Benchmark): Das Niveau ist der Anteil der Szenarien, in denen die Reserve den eingetragenen Wert nicht \u00fcbersteigt (Mack: Lognormal-N\u00e4herung, Bootstrap: simulierte Verteilung). 50 % entspricht dem Median.",
  "Faktorauswahl (Seite \u201eChain Ladder & Mack\u201c): ausgeschlossene Einzelfaktoren und die Beschr\u00e4nkung auf die letzten n Diagonalen gelten einheitlich f\u00fcr Chain Ladder, Mack, Bootstrap und alle darauf aufbauenden Verfahren.",
  "Actual vs. Expected vergleicht die tats\u00e4chliche Entwicklung der j\u00fcngsten Diagonale mit der Prognose aus dem Stand davor. A/E deutlich \u00fcber 1 in mehreren Perioden spricht f\u00fcr zu niedrige, deutlich unter 1 f\u00fcr zu hohe Faktoren.",
  "R\u00fcckblick: Das Dreieck wird auf den Stand vor n Jahren zur\u00fcckgeschnitten; dort wird das Modell festgelegt (Faktorauswahl, Tail, A-priori-Schadenquote). Danach wird Jahr f\u00fcr Jahr die erwartete mit der tats\u00e4chlichen Zahlung verglichen (AvE) und das tats\u00e4chlich eingetretene CDR berechnet \u2013 bei j\u00e4hrlicher Neusch\u00e4tzung mit dem Standardfehler nach Merz-W\u00fcthrich als Ma\u00dfstab.")

# "Ein-Jahres-Sicht: Das Claims Development Result (CDR) ist die Differenz zwischen der Reserve heute und der Summe aus Zahlungen im n\u00e4chsten Jahr und der dann neu gesch\u00e4tzten Reserve. Merz & W\u00fcthrich (2008) geben seinen Standardfehler im Mack-Modell an; der Bootstrap simuliert die n\u00e4chste Diagonale und sch\u00e4tzt Chain Ladder neu (Re-Reserving). Nur f\u00fcr Jahresdreiecke; die Verteilung braucht den Bootstrap (Simulationen > 0).",
# "Munich Chain Ladder braucht zus\u00e4tzlich das Dreieck der gemeldeten Sch\u00e4den (Blatt 4). Es gleicht die Hochrechnungen von bezahlt und gemeldet einander an: Ist das Verh\u00e4ltnis bezahlt/gemeldet einer Periode ungew\u00f6hnlich niedrig, wird bezahlt st\u00e4rker und gemeldet schw\u00e4cher hochgerechnet. Es verwendet alle Einzelfaktoren (ohne Faktorauswahl).",

CSS <- sprintf("
.nav-underline { --bs-nav-underline-gap: 0.42rem; column-gap: 0.42rem; }
.nav-underline .nav-link { font-size: 13px; padding-left: 2px !important; padding-right: 2px !important; }
.caption { color: %1$s; font-size: 13px; margin: 4px 0 10px; }
table.rt { border-collapse: collapse; width: 100%%; font-size: 13px; font-variant-numeric: tabular-nums; }
table.rt th { font-weight: 500; color: %2$s; background: #f6f6f3; padding: 6px 10px; text-align: right;
              border-bottom: 1px solid %3$s; white-space: normal; vertical-align: bottom; }
table.rt td { padding: 6px 10px; text-align: right; border-bottom: 1px solid %3$s; white-space: nowrap; }
table.rt th:first-child, table.rt td:first-child { text-align: left; color: %1$s; }
table.rt td.first-strong { color: %2$s; }
.tbl-wrap { overflow-x: auto; }
.bslib-value-box .value-box-value { font-size: 1.6rem; }
table.ata td.pick { cursor: pointer; }
table.ata td.pick:hover { outline: 1px solid %4$s; }
table.ata td.excl { text-decoration: line-through; color: %1$s; background: #efeee9; }
table.ata td.unused { color: #b9b7b0; }
table.ata td.odd { color: #b3261e; font-weight: 600; }
.sev { display: inline-block; padding: 1px 8px; border-radius: 10px; font-size: 12px; }
.sev-Warnung { background: #fbe3e1; color: #9c1c12; }
.sev-Pr\u00fcfen { background: #fdf0dc; color: #8a4b08; }
.sev-Hinweis { background: #e8f0fb; color: #1d5aa6; }
", MUTED, INK_2, GRID, ACCENT)

# -----------------------------------------------------------------------------
# Tabellen-Helfer: vorformatiert (deutsches Zahlenformat) als HTML
# -----------------------------------------------------------------------------
fmt_frame <- function(df, pct = character(0), spct = character(0), dec = list(), digits = 0,
                      index_name = "") {
  out <- data.frame(x = rownames(df), check.names = FALSE, stringsAsFactors = FALSE)
  names(out) <- index_name
  for (c in names(df)) {
    v <- df[[c]]
    out[[c]] <- if (c %in% pct) de_pct(v) else if (c %in% spct) de_pct(v, sign = TRUE)
                else de(v, if (!is.null(dec[[c]])) dec[[c]] else digits)
  }
  out
}

html_table <- function(fdf, bold_rows = character(0), shade = NULL, first_strong = FALSE, styles = NULL) {
  head <- tags$tr(lapply(names(fdf), tags$th))
  rows <- lapply(seq_len(nrow(fdf)), function(i) {
    bold <- fdf[i, 1] %in% bold_rows
    tags$tr(lapply(seq_along(fdf), function(j) {
      st <- c(if (bold) "font-weight:600",
              if (!is.null(shade) && j > 1 && shade[i, j - 1]) paste0("background:", SHADE),
              if (!is.null(styles) && j > 1 && i <= nrow(styles) && nzchar(styles[i, j - 1])) styles[i, j - 1])
      tags$td(fdf[i, j], class = if (j == 1 && first_strong) "first-strong",
              style = if (length(st)) paste(st, collapse = ";"))
    }))
  })
  div(class = "tbl-wrap", tags$table(class = "rt", tags$thead(head), tags$tbody(rows)))
}

# -----------------------------------------------------------------------------
# Plotly-Helfer
# -----------------------------------------------------------------------------
base_layout <- function(p, ...) {
  p %>% layout(separators = ",.", font = list(family = "system-ui, -apple-system, 'Segoe UI', sans-serif",
                                              size = 13, color = INK_2),
               margin = list(l = 10, r = 20, t = 30, b = 10), paper_bgcolor = "rgba(0,0,0,0)",
               plot_bgcolor = "rgba(0,0,0,0)", ...) %>%
    config(displaylogo = FALSE)
}

vline <- function(x, dash = "dash", color = MUTED) list(type = "line", x0 = x, x1 = x, yref = "paper",
                                                        y0 = 0, y1 = 1, line = list(color = color, dash = dash, width = 1))

fig_bars <- function(df, xtitle) {
  nm <- rownames(df); se <- df$SE
  hover <- sprintf("<b>%s</b><br>Reserve %s%s<br>Abw. zu CL %s<br>Standardfehler %s", nm, de(df$Reserve),
                   if (!is.null(df$Ultimate)) paste0("<br>Ultimate ", de(df$Ultimate)) else "",
                   de_pct(df$`Abw. zu CL`, sign = TRUE), ifelse(is.na(se), "\u2013", de(se)))
  plot_ly(x = df$Reserve, y = nm, type = "bar", orientation = "h", height = 60 + 42 * length(nm),
          marker = list(color = unname(color_for(nm))), hovertext = hover, hoverinfo = "text",
          textposition = "none",
          error_x = list(type = "data", array = ifelse(is.na(se), 0, se), color = INK_2, thickness = 2, width = 5)) %>%
    base_layout(yaxis = list(title = "", categoryorder = "array", categoryarray = rev(nm), gridcolor = GRID),
                xaxis = list(title = xtitle, tickformat = ",.0f", gridcolor = GRID, zeroline = FALSE),
                shapes = c(list(vline(df["Chain Ladder", "Reserve"])),
                           if (BENCHMARK %in% nm) list(vline(df[BENCHMARK, "Reserve"], dash = "dot", color = BENCH_COLOR))),
                bargap = 0.35, showlegend = FALSE)
}

fig_lines <- function(r, sel, origin_label = "Anfalljahr") {
  x <- r$tri$origins
  p <- plot_ly(height = 380)
  for (m in sel) p <- p %>% add_trace(x = x, y = r$reserves[[m]], type = "scatter", mode = "lines+markers",
                                        name = m, line = list(color = color_for(m), width = 2),
                                        marker = list(size = 8, color = color_for(m)),
                                        hovertext = paste0("<b>", m, "</b>: ", de(r$reserves[[m]])), hoverinfo = "text")
  p %>% base_layout(xaxis = list(title = origin_label, type = "category", gridcolor = GRID),
                    yaxis = list(title = "Reserve", tickformat = ",.0f", gridcolor = GRID, zeroline = FALSE),
                    hovermode = "x unified", legend = list(orientation = "h", y = 1.08, x = 0))
}

fig_hist <- function(bs, benchmark_total = NULL, xtitle = "Gesamtreserve") {
  tot <- bs$res_total
  q <- c(Mittel = mean(tot), `99,5 %` = unname(stats::quantile(tot, 0.995)))
  if (!is.null(benchmark_total))
    q[sprintf("Manuelle Reserve (%s%%)", formatC(100 * mean(tot <= benchmark_total), format = "f", digits = 0))] <- benchmark_total
  plot_ly(x = tot, type = "histogram", nbinsx = 60, height = 380,
          marker = list(color = ACCENT, line = list(color = "white", width = 1))) %>%
    base_layout(xaxis = list(title = xtitle, tickformat = ",.0f", gridcolor = GRID),
                yaxis = list(title = "Anzahl Simulationen", gridcolor = GRID), bargap = 0.05,
                shapes = unname(lapply(seq_along(q), function(k) if (startsWith(names(q)[k], "Manuelle"))
                  vline(q[[k]], dash = "dot", color = BENCH_COLOR) else vline(q[[k]], dash = "dash", color = INK_2))),
                annotations = lapply(seq_along(q), function(k) list(
                  x = q[[k]], y = if (startsWith(names(q)[k], "Manuelle")) 0.9 else 1, yref = "paper",
                  xanchor = "left", yanchor = if (startsWith(names(q)[k], "Manuelle")) "top" else "bottom",
                  showarrow = FALSE, bgcolor = "rgba(255,255,255,0.85)",
                  text = paste0(names(q)[k], ": ", de(q[[k]])), font = list(size = 12, color = INK_2))))
}

# Klickbare Tabelle der Einzelfaktoren: Klick schliesst einen Faktor aus bzw. wieder ein
ata_table <- function(t, cl, excl, odd) {
  F <- link_ratios(t); w <- cl$w
  steps <- paste0(head(t$devs, -1), "->", t$devs[-1])
  head <- tags$tr(tags$th(t$labels$origin), lapply(steps, tags$th))
  body <- lapply(seq_len(t$I), function(i) tags$tr(tags$td(t$origins[i]), lapply(seq_len(t$J - 1), function(j) {
    if (!t$obs[i, j + 1]) return(tags$td(""))
    cls <- c("pick", if (excl[i, j]) "excl" else if (!w[i, j]) "unused", if (odd[i, j]) "odd")
    tags$td(if (is.finite(F[i, j])) de(F[i, j], 3) else "\u2013", class = paste(cls, collapse = " "),
            title = if (excl[i, j]) "Ausgeschlossen \u2013 Klick nimmt den Faktor wieder auf" else "Klick schlie\u00dft den Faktor aus",
            onclick = sprintf("Shiny.setInputValue('ata_click', {i: %d, j: %d, n: Math.random()}, {priority: 'event'})", i, j))
  })))
  simple <- vapply(seq_len(t$J - 1), function(j) { v <- F[w[, j], j]; v <- v[is.finite(v)]; if (length(v)) mean(v) else NA }, 0)
  foot <- list(tags$tr(style = "font-weight:600", tags$td("CL-Faktor (Auswahl)"), lapply(cl$f, function(v) tags$td(de(v, 3)))),
               tags$tr(tags$td("einfacher Mittelwert (Auswahl)"), lapply(simple, function(v) tags$td(de(v, 3)))),
               tags$tr(tags$td("Anzahl Faktoren"), lapply(cl$n, function(v) tags$td(v))))
  div(class = "tbl-wrap", tags$table(class = "rt ata", tags$thead(head), tags$tbody(body, foot)))
}

fig_resid <- function(df, x, labels, xtitle) {
  lv <- labels[labels %in% df[[x]]]
  means <- tapply(df$resid, factor(df[[x]], levels = lv), mean)
  plot_ly(height = 300) %>%
    add_trace(x = df[[x]], y = df$resid, type = "scatter", mode = "markers", name = "Residuum",
              marker = list(color = ACCENT, opacity = 0.55, size = 7),
              hovertext = sprintf("%s %s \u00b7 %s %s: %s", "Anfall", df$origin, "Abw.", df$dev, de(df$resid, 2)), hoverinfo = "text") %>%
    add_trace(x = names(means), y = unname(means), type = "scatter", mode = "lines+markers", name = "Mittelwert",
              line = list(color = INK_2, width = 2), marker = list(color = INK_2, size = 6), hoverinfo = "y") %>%
    base_layout(xaxis = list(title = xtitle, type = "category", categoryorder = "array", categoryarray = lv, gridcolor = GRID),
                yaxis = list(title = "std. Residuum", gridcolor = GRID, zeroline = TRUE, zerolinecolor = MUTED),
                showlegend = FALSE)
}

fig_qq <- function(df) {
  r <- sort(df$resid); q <- stats::qnorm(stats::ppoints(length(r)))
  lim <- range(c(q, r)) * 1.05
  plot_ly(height = 300) %>%
    add_trace(x = q, y = r, type = "scatter", mode = "markers", marker = list(color = ACCENT, opacity = 0.6, size = 7),
              hoverinfo = "y") %>%
    add_trace(x = lim, y = lim, type = "scatter", mode = "lines", line = list(color = MUTED, dash = "dash"), hoverinfo = "skip") %>%
    base_layout(xaxis = list(title = "Theoretisches Quantil (Normalverteilung)", gridcolor = GRID, range = lim),
                yaxis = list(title = "std. Residuum (sortiert)", gridcolor = GRID, range = lim), showlegend = FALSE)
}

fig_tornado <- function(sens) {
  s <- sens[order(abs(sens$Abweichung)), , drop = FALSE]
  cols <- ifelse(s$Abweichung >= 0, "#c8651b", ACCENT)
  plot_ly(x = s$Abweichung, y = rownames(s), type = "bar", orientation = "h", height = 90 + 40 * nrow(s),
          marker = list(color = cols), textposition = "none",
          hovertext = sprintf("<b>%s</b><br>Reserve %s<br>Abweichung %s (%s)", rownames(s), de(s$Reserve),
                              de(s$Abweichung), de_pct(s$`Abweichung %`, sign = TRUE)), hoverinfo = "text") %>%
    base_layout(xaxis = list(title = "Abweichung zur Basis-Reserve", tickformat = ",.0f", gridcolor = GRID,
                             zeroline = TRUE, zerolinecolor = INK_2),
                yaxis = list(title = "", categoryorder = "array", categoryarray = rownames(s), gridcolor = GRID),
                bargap = 0.35, showlegend = FALSE)
}

fig_ave <- function(tab) {
  t <- tab[rownames(tab) != "Summe", , drop = FALSE]; x <- rownames(t)
  p <- plot_ly(height = 360)
  for (nm in c("Erwartet CL", "Erwartet BF", "Tats\u00e4chlich")) {
    if (all(is.na(t[[nm]]))) next
    col <- switch(nm, `Erwartet CL` = unname(color_for("Chain Ladder")), `Erwartet BF` = unname(color_for("Bornhuetter-Ferguson")), INK_2)
    ok <- !is.na(t[[nm]])
    p <- p %>% add_trace(x = x[ok], y = t[[nm]][ok], type = "bar", name = nm, marker = list(color = col),
                         hovertext = paste0(nm, ": ", de(t[[nm]][ok])), hoverinfo = "text")
  }
  p %>% base_layout(barmode = "group", xaxis = list(title = "", type = "category", gridcolor = GRID),
                    yaxis = list(title = "Zuwachs in der j\u00fcngsten Periode", tickformat = ",.0f", gridcolor = GRID),
                    legend = list(orientation = "h", y = 1.1, x = 0))
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Loss Reserving \u2013 Verfahrensvergleich",
  theme = bs_theme(version = 5, primary = ACCENT, base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif")),
  fillable = FALSE,
  sidebar = sidebar(
    width = 300,
    h6("Daten"),
    radioButtons("source", NULL, c("Demo" = "demo", 
                                   "Excel-Datei hochladen" = "upload")),
    conditionalPanel("input.source == 'upload'",
                     fileInput("upload", NULL, accept = c(".xlsx", ".xls"),
                               buttonLabel = "Datei w\u00e4hlen", placeholder = "(.xlsx)"),
                     div(class = "caption", style = "margin-top:-8px",
                         "Blatt 1 Sch\u00e4den, Blatt 2 Beitr\u00e4ge, Blatt 3 manuelle Reserve; optional Blatt 4 gemeldete Sch\u00e4den, Blatt 5 manuelle AvE und CDR. ",
                         downloadLink("dl_template", "Vorlage herunterladen"))),
    selectInput("period", "Perioden", c("automatisch erkennen" = "auto" )), # "automatisch erkennen" = "auto",  "Quartale" = "quarter"
    #checkboxInput("to_years", "Quartale zu Jahren verdichten", FALSE),
    #radioButtons("kind", "Werte im Dreieck", c("kumuliert" = "cum", "inkrementell" = "inc"), inline = TRUE),
    h6("Parameter", class = "mt-2"),
    input_switch("use_prem", "Pr\u00e4mienbasierte Verfahren", TRUE),
    checkboxInput("use_cc", "Cape-Cod-Quote als A-priori-Quote", FALSE),
    numericInput("elr", "A-priori-Schadenquote (%)", 50, min = 1, max = 300, step = 0.5),
    numericInput("tail", "Tail-Faktor", 1, min = 1, max = 3, step = 0.005),
    numericInput("seed", "Zufallsstartwert", 42, min = 0, step = 1),
    selectInput("sims", "Bootstrap-Simulationen",
                setNames(SIM_CHOICES, formatC(SIM_CHOICES, big.mark = ".", decimal.mark = ",", format = "d")), selected = 1000)

  ),
  tags$head(tags$style(HTML(CSS))),
  uiOutput("load_msg"),
  navset_underline(
    nav_panel("Daten",
      layout_columns(col_widths = c(8, 4),
        div(h5("Kumuliertes Dreieck", class = "mt-3"), uiOutput("tbl_tri"), uiOutput("tbl_inc"), 
            div(class = "caption", "Die Werte stammen aus Blatt 1 (Sch\u00e4den) der Excel-Datei. Zum \u00c4ndern die Datei anpassen und neu hochladen."),
        ),
        div(h5("Beitr\u00e4ge und manuelle Reserve", class = "mt-3"), uiOutput("tbl_inputs"),
            div(class = "caption", "Die Werte stammen aus Blatt 2 (Beitr\u00e4ge) und Blatt 3 (manuelle Reserve) der Excel-Datei. Zum \u00c4ndern die Datei anpassen und neu hochladen. Ist die manuelle Reserve ausgef\u00fcllt, wird sie als Benchmark mit allen Verfahren verglichen."),
            uiOutput("prem_msg"),
            uiOutput("tbl_manual_ave"))),
      card(class = "mt-3", card_header("Plausibilit\u00e4tspr\u00fcfungen"), uiOutput("tbl_checks"),
           div(class = "caption", "Orange hinterlegt im Dreieck: negative Inkremente. Auff\u00e4llige Einzelfaktoren sind auf der Seite \u201eChain Ladder & Mack\u201c rot markiert und lassen sich dort ausschlie\u00dfen."))
    ),
    nav_panel("\u00dcbersicht",
      uiOutput("check_summary"),
      uiOutput("kpis"),
      layout_columns(col_widths = c(5, 7),
        card(card_header("Gesamtreserve je Verfahren"), plotlyOutput("fig_total", height = "auto"),
             div(class = "caption", "Schwarze Linie: \u00b11 Standardfehler (Mack bzw. Bootstrap). Gestrichelt: Chain-Ladder-Reserve. Gepunktet: manuelle Reserve (falls vorhanden).")),
        card(card_header("Vergleich"), uiOutput("tbl_comp"),
             tags$b("Quantile der Gesamtreserve", class = "mt-2"), uiOutput("tbl_q"),
             div(class = "caption", "SE = Standardfehler, VK = Variationskoeffizient. Quantile f\u00fcr Chain Ladder \u00fcber eine Lognormal-N\u00e4herung, f\u00fcr den Bootstrap empirisch."),
             uiOutput("elr_info"))),
      uiOutput("bench_card")#,
      #div(class = "mt-2", downloadButton("dl_xlsx", "Alle Ergebnisse als Excel herunterladen", class = "btn-primary"))
    ),
    nav_panel("Je Anfallperiode",
      card(class = "mt-3", card_header("Einzelne Anfallperiode \u2013 alle Verfahren"),
           uiOutput("one_pick"), uiOutput("one_facts"),
           layout_columns(col_widths = c(5, 7), plotlyOutput("fig_one", height = "auto"), uiOutput("tbl_one")),
           div(class = "caption", "Schwarze Linie: \u00b11 Standardfehler (Mack bzw. Bootstrap) f\u00fcr diese Anfallperiode. Gestrichelt: Chain Ladder.")),
      h5("Alle Anfallperioden", class = "mt-3"),
      uiOutput("sel_ui"),
      plotlyOutput("fig_origin", height = "auto"),
      h5("Reserven je Anfallperiode", class = "mt-3"), uiOutput("tbl_res"),
      div(class = "caption", "Entw.-Grad CL = Anteil des bereits bekannten Schadens am Chain-Ladder-Endschaden."),
      h5("Endschadenst\u00e4nde (Ultimates)", class = "mt-3"), uiOutput("tbl_ult")
    ),
    nav_panel("Chain Ladder & Mack",
      card(class = "mt-3", card_header("Faktorauswahl"),
        layout_columns(col_widths = c(4, 8),
          div(radioButtons("avg", "Einzelfaktoren aus", c("allen Diagonalen" = "all", "nur den letzten n Diagonalen" = "last")),
              conditionalPanel("input.avg == 'last'", numericInput("last_n", "n (Anzahl Diagonalen)", 5, min = 1, step = 1, width = "180px"))),
          div(p(class = "small", "Ein Klick auf einen Einzelfaktor in der Tabelle \u201eIndividuelle Abwicklungsfaktoren\u201c schlie\u00dft ihn aus; ein weiterer Klick nimmt ihn wieder auf. Die Auswahl gilt f\u00fcr Chain Ladder, Mack, Bootstrap und alle darauf aufbauenden Verfahren."),
              uiOutput("excl_status"),
              actionButton("excl_reset", "Alle Ausschl\u00fcsse aufheben", class = "btn-sm btn-outline-secondary")))),
      h5("Individuelle Abwicklungsfaktoren", class = "mt-3"), uiOutput("tbl_ata"),
      div(class = "caption", "Durchgestrichen: ausgeschlossen. Grau: au\u00dferhalb der gew\u00e4hlten Diagonalen. Rot: auff\u00e4lliger Faktor (mehr als doppelt oder weniger als halb so gro\u00df wie der CL-Faktor) \u2013 Kandidat f\u00fcr einen Ausschluss."),
      h5("Abwicklungsfaktoren", class = "mt-3"), uiOutput("tbl_fac"),
      div(class = "caption", "CDF: Cumulative Development Factor"),
      h5("Projiziertes Dreieck (Chain Ladder)", class = "mt-3"), uiOutput("tbl_proj"),
      div(class = "caption", "Blau hinterlegt: projizierte Werte.")
    ),
    #nav_panel("Munich Chain Ladder",
    #  uiOutput("mcl_body")
    #),
    nav_panel("Bootstrap",
      uiOutput("bs_off"),
      conditionalPanel("input.sims != '0'",
        layout_columns(col_widths = c(8, 4), class = "mt-3",
          card(card_header("Verteilung der Gesamtreserve"), plotlyOutput("fig_hist", height = "auto")),
          card(card_header("Kennzahlen"), uiOutput("tbl_bs_k"), uiOutput("bs_n"))),
        h5("Verteilung je Anfallperiode", class = "mt-4"),
        div(class = "caption", "Simulierte Reserve einer einzelnen Anfallperiode \u2013 Auswahl \u00fcber das Feld rechts, voreingestellt ist die j\u00fcngste Periode. Gestrichelt: Mittelwert und 99,5 %-Quantil; gepunktet: manuelle Reserve (falls vorhanden)."),
        layout_columns(col_widths = c(8, 4),
          card(card_header(uiOutput("bs_one_title", inline = TRUE)), plotlyOutput("fig_hist_one", height = "auto")),
          card(card_header("Kennzahlen"), uiOutput("bs_pick"), uiOutput("tbl_bs_one"))),
        h5("Quantile je Anfallperiode", class = "mt-3"), uiOutput("tbl_bs_o")),
      h5("Residuen-Diagnose", class = "mt-4"),
      div(class = "caption", "Standardisierte Pearson-Residuen des ODP-Modells (Grundlage des Bootstraps). Sie sollten ohne Muster um 0 streuen, \u00fcberwiegend zwischen \u22122 und +2. Ein Trend \u00fcber die Kalenderperioden deutet auf Inflation oder eine ge\u00e4nderte Schadenbearbeitung hin."),
      uiOutput("diag_trend"),
      layout_columns(col_widths = c(6, 6),
        card(card_header("nach Anfallperiode"), plotlyOutput("fig_res_o", height = "auto")),
        card(card_header("nach Abwicklungsperiode"), plotlyOutput("fig_res_d", height = "auto")),
        card(card_header("nach Kalenderperiode"), plotlyOutput("fig_res_c", height = "auto")),
        card(card_header("Q-Q-Diagramm"), plotlyOutput("fig_res_q", height = "auto")))
    ),
    #nav_panel("Ein-Jahres-Sicht",
    #  uiOutput("oy_body")
    #),
    nav_panel("Sensitivit\u00e4t",
      layout_columns(col_widths = c(4, 8), class = "mt-3",
        uiOutput("sens_pick"), uiOutput("sens_base")),
      card(card_header("Wirkung auf die Gesamtreserve"), plotlyOutput("fig_tornado", height = "auto"),
           div(class = "caption", "Orange: Reserve steigt, blau: Reserve sinkt \u2013 jeweils gegen\u00fcber der aktuellen Einstellung (Basis). Jedes Szenario \u00e4ndert genau eine Annahme; der Bootstrap wird daf\u00fcr nicht neu gerechnet.")),
      uiOutput("tbl_sens"), uiOutput("sens_none")
    ),
    nav_panel("Actual vs Expected",
      radioButtons("ave_mode", NULL, setNames(c("bt", "file"), c("Backtest: j\u00fcngste Diagonale herausnehmen", "Vorjahresdatei hochladen")),
                   inline = TRUE, width = "100%"),
      conditionalPanel("input.ave_mode == 'file'",
        fileInput("ave_file", NULL, accept = c(".xlsx", ".xls"), buttonLabel = "Vorjahresdatei", placeholder = "(.xlsx)", width = "360px"),
        div(class = "caption", style = "margin-top:-8px", "Excel-Datei im gleichen Aufbau (Blatt 1 Sch\u00e4den, Blatt 2 Beitr\u00e4ge), Stand eine Periode fr\u00fcher.")),
      uiOutput("ave_msg"), uiOutput("ave_kpis"),
      card(card_header("Erwarteter und tats\u00e4chlicher Zuwachs je Anfallperiode"), plotlyOutput("fig_ave", height = "auto")),
      uiOutput("tbl_ave"),
      div(class = "caption", "Erwartet CL = Stand \u00d7 (CL-Faktor \u2212 1) mit den Faktoren des Vorstands. Erwartet BF = Pr\u00e4mie \u00d7 Schadenquote \u00d7 Anteil, der laut Abwicklungsmuster in dieser Periode hinzukommt. A/E = Tats\u00e4chlich / Erwartet; Werte nahe 1 best\u00e4tigen die Annahmen. Die Summe von A/E bezieht nur Perioden mit Erwartungswert ein.")
    ),
    nav_panel("R\u00fcckblick",
      div(class = "d-flex flex-wrap align-items-end gap-4 mt-3",
        numericInput("rb_years", "Jahre zur\u00fcck (Modellwahl)", value = 5, min = 1, max = 30, step = 1, width = "190px"),
        radioButtons("rb_mode", "Faktoren in den Folgejahren",
                     setNames(c("refit", "fixed"), c("j\u00e4hrlich neu sch\u00e4tzen (Einstellungen von damals)", "vom Startzeitpunkt festhalten")),
                     inline = TRUE)),
      uiOutput("rb_body")
    ),
    nav_panel("Methodik",
      tags$table(class = "table mt-3",
        tags$thead(tags$tr(tags$th("Verfahren"), tags$th("Idee"), tags$th("Braucht Pr\u00e4mien"))),
        tags$tbody(lapply(METHODS_INFO, function(m) tags$tr(tags$td(tags$b(m[1])), tags$td(m[2]), tags$td(m[3]))))),
      h5("Hinweise zur Interpretation"),
      tags$ul(lapply(METHOD_NOTES, tags$li))
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  loaded <- reactive({
    tryCatch({
      inc <- input$kind == "inc"
      if (input$source == "demo")
        return(list(tri = demo_triangle(), premiums = DEMO_PREMIUMS, benchmark = DEMO_BENCHMARK, mcl = demo_triangle_count(),  notes = character(0),
                    warnings = character(0), err = NULL))
      d <- if (input$source == "demo_q") demo_quarterly(isTRUE(input$to_years))
      else if (!is.null(input$upload))
        #read_excel_input(input$upload$datapath, inc, input$period, isTRUE(input$to_years), name = input$upload$name)
        read_excel_input(input$upload$datapath, inc, input$period, isTRUE(input$to_years), name = input$upload$name)

      if (is.null(d)) return(list(tri = NULL, err = NULL))
      c(d, list(err = NULL))
    }, error = function(e) list(tri = NULL, err = conditionMessage(e)))
  })
  tri <- reactive({ req(loaded()$tri); loaded()$tri })

  output$load_msg <- renderUI({
    l <- loaded()
    if (!is.null(l$err)) return(div(class = "alert alert-danger", "Das Dreieck konnte nicht gelesen werden: ", l$err))
    if (is.null(l$tri)) return(div(class = "alert alert-info",
      "Lade links eine Excel-Datei hoch: Blatt 1 Sch\u00e4den (erste Spalte Anfalljahr, weitere Spalten Abwicklungsperioden), Blatt 2 Beitr\u00e4ge (Anfalljahr, Pr\u00e4mie), Blatt 3 manuelle Reserve (Anfalljahr, Wert). Blatt 2 und 3 sind optional. Ebenfalls optional: Blatt 4 gemeldete Sch\u00e4den f\u00fcr Munich Chain Ladder (Aufbau wie Blatt 1) und Blatt 5 manuelle erwartete Zahlung und manuelles CDR je Kalenderjahr (Kalenderjahr, Erwartete Zahlung, CDR) f\u00fcr den R\u00fcckblick. ", downloadLink("dl_template2", "Vorlage herunterladen")))
    cap <- c(demo = "Demo: Jahresdaten; Die Daten sind fiktiv und nur zur Veranschaulichung eingetragen.",
             demo_q = "Demo: Quartalsdaten; Die Daten sind fiktiv und nur zur Veranschaulichung eingetragen.")
    tagList(div(class = "caption", paste(c(if (input$source %in% names(cap)) cap[[input$source]],
                                           paste0("Perioden: ", l$tri$labels$origin_pl, ".")), collapse = " ")),
            lapply(l$notes, function(n) div(class = "alert alert-info py-2 small", n)),
            lapply(l$warnings, function(n) div(class = "alert alert-warning py-2 small", n)))
  })

  # --- Beitraege und manuelle Reserve: aus der Excel-Datei (Demo: fiktive Praemien) ------
  premiums <- reactive(if (isTRUE(input$use_prem)) loaded()$premiums)
  benchmark <- reactive(list(werte = loaded()$benchmark))

  template_handler <- downloadHandler(
    filename = "loss_reserving_vorlage.xlsx",
    content = function(file) file.copy(file.path(DATA_DIR, "beispiel_jahre.xlsx"), file))
  output$dl_template <- template_handler
  output$dl_template2 <- template_handler

  output$tbl_inputs <- renderUI({
    t <- tri(); l <- loaded()
    df <- data.frame(if (is.null(l$premiums)) rep(NA_real_, t$I) else l$premiums,
                     if (is.null(l$benchmark)) rep(NA_real_, t$I) else l$benchmark, row.names = t$origins)
    names(df) <- c("Pr\u00e4mie", "Manuelle Reserve")
    html_table(fmt_frame(df, index_name = t$labels$origin))
  })

  output$tbl_manual_ave <- renderUI({
    m <- loaded()$manual_ave; if (is.null(m)) return(NULL)
    df <- data.frame(m$Erwartet, m$CDR, row.names = m$Kalenderjahr); names(df) <- c("Erwartete Zahlung", "CDR")
    tagList(h5("Manuelle AvE und CDR", class = "mt-3"), html_table(fmt_frame(df, index_name = "Kalenderjahr")),
            div(class = "caption", "Aus Blatt 5: Summe \u00fcber alle Anfallperioden je Kalenderjahr. Verglichen auf der Seite \u201eR\u00fcckblick\u201c."))
  })

  output$prem_msg <- renderUI(tagList(
    if (isTRUE(input$use_prem) && is.null(loaded()$premiums))
      div(class = "alert alert-warning py-2 small",
          "Keine Beitr\u00e4ge vorhanden \u2013 pr\u00e4mienbasierte Verfahren werden \u00fcbersprungen.")
  ))

  # --- Rechnen ---------------------------------------------------------------
  tail_v <- reactive(if (is.na(input$tail)) 1 else input$tail)
  # --- Faktorauswahl ------------------------------------------------------------
  excl <- reactiveVal(NULL)
  observeEvent(tri(), excl(matrix(FALSE, tri()$I, tri()$J - 1)))
  observeEvent(input$excl_reset, excl(matrix(FALSE, tri()$I, tri()$J - 1)))
  observeEvent(input$ata_click, {
    m <- excl(); i <- input$ata_click$i; j <- input$ata_click$j
    if (!is.null(m) && i <= nrow(m) && j <= ncol(m) && tri()$obs[i, j + 1]) { m[i, j] <- !m[i, j]; excl(m) }
  })
  last_n <- reactive(if (identical(input$avg, "last") && isTRUE(input$last_n >= 1)) as.integer(input$last_n) else NULL)
  weights <- reactive(factor_weights(tri(), exclude = excl(), last_n = last_n()))
  selection_text <- reactive({
    m <- excl(); t <- tri(); parts <- character(0)
    if (!is.null(m) && any(m)) {
      idx <- which(m, arr.ind = TRUE)
      parts <- c(parts, sprintf("%d Einzelfaktor%s ausgeschlossen (%s)", nrow(idx), if (nrow(idx) > 1) "en" else "",
                                paste(sprintf("%s %s->%s", t$origins[idx[, 1]], t$devs[idx[, 2]], t$devs[idx[, 2] + 1]), collapse = ", ")))
    }
    if (!is.null(last_n())) parts <- c(parts, sprintf("nur die letzten %d Diagonalen", last_n()))
    parts
  })

  # --- Rechnen ---------------------------------------------------------------
  elr_val <- reactive({
    p <- premiums()
    if (is.null(p) || isTRUE(input$use_cc) || is.na(input$elr)) NULL else input$elr / 100
  })
  boot <- reactive({
    n <- as.integer(input$sims)
    if (n <= 0) return(NULL)
    tryCatch(withProgress(message = "Bootstrap l\u00e4uft \u2026",
                 odp_bootstrap(tri(), chain_ladder(tri(), tail_v(), weights()), n, if (is.na(input$seed)) 0 else input$seed)),
             error = function(e) NULL)   # Hinweis erzeugt compute_all()
  })
  res <- reactive({
    compute_all(tri(), premiums(), elr_val(), tail_v(), n_sims = as.integer(input$sims), seed = input$seed, bootstrap = boot(),
                incurred = loaded()$incurred,
                benchmark = benchmark()$werte, weights = weights())
  })
  checks <- reactive(plausibility_checks(tri(), res()$cl, loaded()$premiums, loaded()$benchmark, loaded()$incurred))
  diag_res <- reactive(residual_diagnostics(tri(), res()$cl))
  sens_method <- reactive({
    m <- setdiff(names(res()$reserves), c(BENCHMARK, "ODP-Bootstrap (Mittel)", "Munich Chain Ladder"))
    if (!is.null(input$sens_method) && input$sens_method %in% m) input$sens_method else m[1]
  })
  sens <- reactive(sensitivity(tri(), premiums(), elr_val(), tail_v(), weights(), sens_method()))
  ave <- reactive({
    prev <- prev_p <- NULL
    if (identical(input$ave_mode, "file")) {
      req(input$ave_file)
      d <- read_excel_input(input$ave_file$datapath, input$kind == "inc", input$period, isTRUE(input$to_years),
                            name = input$ave_file$name)
      prev <- d$tri; prev_p <- d$premiums
    }
    actual_vs_expected(tri(), premiums(), elr_val(), prev, prev_p, last_n = last_n(),
                       exclude = if (is.null(prev)) excl())
  })

  # --- Uebersicht ---------------------------------------------------------------
  output$kpis <- renderUI({
    r <- res(); tots <- r$comp$Reserve[rownames(r$comp) != BENCHMARK]; cl_total <- tots[1]
    boxes <- list(
      value_box("Reserve Chain Ladder", de(cl_total), p(class = "small", "Diagonale ", de(sum(r$tri$latest)))),
      value_box("Mack-Standardfehler", de(r$mk$se_total), p(class = "small", "VK ", de_pct(r$mk$se_total / cl_total))),
      value_box("Spanne der Verfahren", paste(de(min(tots)), "\u2013", de(max(tots))),
                p(class = "small", "Median ", de(stats::median(tots)))))
    if (!is.null(r$bs)) boxes[[4]] <- value_box("Bootstrap 99,5 %-Quantil",
      de(stats::quantile(r$bs$res_total, 0.995)), p(class = "small", "Mittel ", de(mean(r$bs$res_total))))
    if (!is.null(r$bench)) {
      bt <- r$bench["Summe", ]
      boot_ok <- !is.na(bt$`Niveau Bootstrap`)
      lvl <- if (boot_ok) bt$`Niveau Bootstrap` else bt$`Niveau Mack`
      boxes[[length(boxes) + 1]] <- value_box("Manuelle Reserve", de(bt$`Manuelle Reserve`),
        p(class = "small", de_pct(bt$`Abw. zu CL`, sign = TRUE), " zu CL \u00b7 Niveau ",
          de_pct(lvl, 0), if (boot_ok) " (Bootstrap)" else " (Mack)"))
    }
    do.call(layout_column_wrap, c(boxes, list(width = 1 / length(boxes), fill = FALSE)))
  })

  output$fig_total <- renderPlotly(fig_bars(res()$comp, "Reserve"))
  output$tbl_comp <- renderUI(html_table(fmt_frame(res()$comp[, intersect(c("Reserve", "Ultimate", "Abw. zu CL", "Abw. zu Benchmark", "SE", "VK"), names(res()$comp))],
                                                   pct = "VK", spct = c("Abw. zu CL", "Abw. zu Benchmark"), index_name = "Verfahren"),
                                         first_strong = TRUE))
  output$tbl_q <- renderUI({
    cmp <- res()$comp; q <- cmp[!is.na(cmp$SE), c("Reserve", "SE", "Q 75%", "Q 95%", "Q 99.5%")]
    names(q) <- c("Erwartungswert", "SE", "75 %", "95 %", "99,5 %")
    html_table(fmt_frame(q, index_name = "Verfahren"), first_strong = TRUE)
  })
  output$check_summary <- renderUI({
    tab <- checks()$table
    if (!nrow(tab)) return(div(class = "caption mt-2", "Plausibilit\u00e4tspr\u00fcfung: keine Auff\u00e4lligkeiten."))
    cnt <- table(factor(tab$Schwere, levels = SEVERITY)); cnt <- cnt[cnt > 0]
    div(class = paste("alert py-2 small mt-2", if ("Warnung" %in% names(cnt)) "alert-warning" else "alert-light"),
        tags$b("Plausibilit\u00e4tspr\u00fcfung: "), paste(paste0(cnt, " \u00d7 ", names(cnt)), collapse = " \u00b7 "),
        " \u2013 Details auf der Seite \u201eDaten\u201c.")
  })
  output$elr_info <- renderUI({
    r <- res()
    tagList(
      if (length(selection_text())) div(class = "alert alert-info py-2 small",
        tags$b("Faktorauswahl aktiv: "), paste(selection_text(), collapse = " \u00b7 "), " (Seite \u201eChain Ladder & Mack\u201c)."),
      if (!is.null(r$elr_used)) div(class = "caption", sprintf("A-priori-Schadenquote: %s \u00b7 Cape-Cod-Quote: %s",
                                                               de_pct(r$elr_used, 2), de_pct(r$cc_elr, 2))),
      lapply(r$notes, function(n) div(class = "alert alert-info py-2 small", n)))
  })
  output$bench_card <- renderUI({
    r <- res()
    if (is.null(r$bench))
      return(div(class = "caption mt-2", "Hinweis: Die eigene, manuelle Reserve je Anfallperiode l\u00e4sst sich auf Blatt 3 der Excel-Datei mitliefern. Sie wird dann als Benchmark mit allen Verfahren verglichen."))
    card(class = "mt-3", card_header("Einordnung der manuellen Reserve"),
         tags$ul(lapply(benchmark_summary(r), tags$li)),
         html_table(fmt_frame(r$bench, pct = c("Niveau Mack", "Niveau Bootstrap"), spct = "Abw. zu CL",
                              index_name = tri()$labels$origin), bold_rows = "Summe"),
         div(class = "caption", "Niveau = Anteil der Szenarien, den die manuelle Reserve abdeckt: nach Mack \u00fcber eine Lognormal-N\u00e4herung, nach Bootstrap aus der simulierten Verteilung. 50% entspricht dem Median; weil Reserveverteilungen rechtsschief sind, liegt der Erwartungswert meist etwas dar\u00fcber. H\u00f6here Werte bedeuten eine vorsichtigere Reservierung."))
  })

  output$dl_xlsx <- downloadHandler(
    filename = "reserving_ergebnisse.xlsx",
    content = function(file) {
      r <- res()
      safe <- function(expr) tryCatch(expr, error = function(e) NULL)
      t <- tri(); w <- r$w
      fa <- as.data.frame(ifelse(t$obs[, -1, drop = FALSE], ifelse(w, "ja", "nein"), ""), check.names = FALSE)
      names(fa) <- paste0(head(t$devs, -1), "->", t$devs[-1])
      extra <- setNames(list(fa, safe(checks()$table), safe({ s <- sens(); cbind(Verfahren = sens_method(), s) }),
                             safe(ave()$table)),
                        c("Faktorauswahl", "Plausibilit\u00e4t", "Sensitivit\u00e4t", "Actual vs Expected"))
      if (!is.null(r$mcl)) extra[["Munich Chain Ladder"]] <- r$mcl$table
      b <- safe(rb())
      if (!is.null(b) && !inherits(b, "rb_error")) {
        extra[["R\u00fcckblick"]] <- b$summary
        dd <- b$detail; rownames(dd) <- NULL; extra[["R\u00fcckblick Detail"]] <- dd
        extra[["R\u00fcckblick Endschaden"]] <- b$ultimates
      }
      write_excel(r, file, list(`A-priori-Schadenquote` = r$elr_used, `Cape-Cod-Quote` = r$cc_elr,
                                `Tail-Faktor` = tail_v(), `Bootstrap-Simulationen` = as.integer(input$sims), Seed = input$seed,
                                Faktorauswahl = if (length(selection_text())) paste(selection_text(), collapse = "; ") else "alle Faktoren"),
                  extra = extra)
    })

  # --- Je Anfallperiode -----------------------------------------------------------
  output$one_pick <- renderUI({
    o <- tri()$origins
    prev <- isolate(input$one_year)
    selectInput("one_year", tri()$labels$origin, o, selected = if (!is.null(prev) && prev %in% o) prev else tail(o, 1),
                width = "200px")
  })
  one_idx <- reactive({ req(input$one_year %in% tri()$origins); match(input$one_year, tri()$origins) })
  output$one_facts <- renderUI({
    r <- res(); i <- one_idx()
    f <- c(Diagonale = de(r$tri$latest[i]), `Entwicklungsgrad (CL)` = de_pct(1 / r$cdf_d[i]),
           x = as.character(r$tri$latest_idx[i]))
    names(f)[3] <- paste("Beobachtete", r$tri$labels$dev_pl)
    if (!is.null(r$premiums)) f["Pr\u00e4mie"] <- de(r$premiums[i])
    if (!is.null(r$bench)) {
      b <- r$bench[i, ]; lvl <- if (!is.na(b$`Niveau Bootstrap`)) b$`Niveau Bootstrap` else b$`Niveau Mack`
      f["Manuelle Reserve"] <- paste0(de(b$`Manuelle Reserve`), if (!is.na(lvl)) paste0(" (Niveau ", de_pct(lvl, 0), ")"))
    }
    div(class = "small mb-2", HTML(paste0(names(f), ": <b>", f, "</b>", collapse = " &nbsp;\u00b7&nbsp; ")))
  })
  output$fig_one <- renderPlotly({
    i <- one_idx(); fig_bars(origin_detail(res(), i), paste("Reserve", tri()$labels$origin, tri()$origins[i]))
  })
  output$tbl_one <- renderUI({
    det <- origin_detail(res(), one_idx())
    html_table(fmt_frame(det, pct = intersect("Schadenquote", names(det)), spct = c("Abw. zu CL", "Abw. zu Benchmark"),
                         index_name = "Verfahren"), first_strong = TRUE)
  })

  output$sel_ui <- renderUI({
    m <- names(res()$reserves)
    prev <- isolate(input$sel)
    keep <- intersect(prev, m)
    if (!length(keep)) keep <- m[startsWith(m, "Chain Ladder") | startsWith(m, "Bornhuetter") | startsWith(m, "Cape Cod") | startsWith(m, "ELR") | startsWith(m, "Additiv") | startsWith(m, "ODP")  |  m == BENCHMARK]
    selectizeInput("sel", "Verfahren im Diagramm", m, selected = keep, multiple = TRUE, width = "100%")
  })
  output$fig_origin <- renderPlotly({ req(input$sel); fig_lines(res(), intersect(input$sel, names(res()$reserves)), tri()$labels$origin) })
  output$tbl_res <- renderUI(html_table(fmt_frame(res()$res_df, pct = "Entw.-Grad CL", index_name = tri()$labels$origin),
                                        bold_rows = "Summe"))
  output$tbl_ult <- renderUI(html_table(fmt_frame(res()$ult_df, index_name = tri()$labels$origin), bold_rows = "Summe"))

  # --- Chain Ladder & Mack -----------------------------------------------------
  output$tbl_fac <- renderUI(html_table(fmt_frame(res()$fac, dec = setNames(list(4, 4, 2, 0), names(res()$fac)),
                                                  index_name = "\u00dcbergang")))
  output$excl_status <- renderUI({
    txt <- selection_text()
    div(class = "small mb-2", if (length(txt)) tagList(tags$b("Aktiv: "), paste(txt, collapse = " \u00b7 ")) else "Alle Einzelfaktoren werden verwendet.")
  })
  output$tbl_ata <- renderUI({
    m <- excl(); t <- tri(); req(!is.null(m), identical(dim(m), c(t$I, t$J - 1L)))
    ata_table(t, res()$cl, m, checks()$odd)
  })
  output$tbl_proj <- renderUI({
    t <- tri(); full <- res()$cl$full
    sh <- !t$obs
    if (ncol(full) > ncol(sh))                       # Tail-Spalte: ebenfalls als projiziert markieren
      sh <- cbind(sh, matrix(TRUE, nrow(sh), ncol(full) - ncol(sh)))
    html_table(fmt_frame(as.data.frame(full, check.names = FALSE), index_name = t$labels$origin),
               shade = sh)
  })

  # --- Bootstrap -------------------------------------------------------------
  output$bs_off <- renderUI(if (as.integer(input$sims) == 0)
    div(class = "alert alert-info mt-3", "Der ODP-Bootstrap ist abgeschaltet (Simulationen = 0).")
    else if (is.null(res()$bs))
    div(class = "alert alert-info mt-3", "Der ODP-Bootstrap konnte f\u00fcr dieses Dreieck nicht gerechnet werden."))
  output$fig_hist <- renderPlotly({ r <- res(); req(r$bs)
    fig_hist(r$bs, if (!is.null(r$benchmark)) sum(r$benchmark)) })
  # Anfallperioden mit simulierter Reserve (vollstaendig abgewickelte fehlen)
  bs_origins <- reactive({ bs <- res()$bs; req(bs)
    tri()$origins[apply(bs$res_origin, 2, stats::sd) > 0] })
  output$bs_pick <- renderUI({
    o <- bs_origins(); req(length(o) > 0)
    prev <- isolate(input$bs_origin)
    selectInput("bs_origin", tri()$labels$origin, rev(o),
                selected = if (!is.null(prev) && prev %in% o) prev else tail(o, 1), width = "200px")
  })
  bs_idx <- reactive({ req(input$bs_origin %in% bs_origins()); match(input$bs_origin, tri()$origins) })
  output$bs_one_title <- renderUI({ req(input$bs_origin)
    paste0("Verteilung der Reserve ", tri()$labels$origin, " ", input$bs_origin) })
  output$fig_hist_one <- renderPlotly({
    r <- res(); req(r$bs); i <- bs_idx()
    fig_hist(list(res_total = r$bs$res_origin[, i]), if (!is.null(r$benchmark)) r$benchmark[i],
             xtitle = paste("Reserve", tri()$labels$origin, tri()$origins[i]))
  })
  output$tbl_bs_one <- renderUI({
    r <- res(); bs <- r$bs; req(bs); i <- bs_idx(); x <- bs$res_origin[, i]
    k <- data.frame(Reserve = c(mean(x), stats::median(x), stats::quantile(x, c(0.75, 0.95, 0.995)), stats::sd(x),
                                r$res_df[i, "Chain Ladder"]),
                    row.names = c("Mittel", "Median", "75 %", "95 %", "99,5 %", "Standardfehler", "Chain Ladder"))
    if (!is.null(r$benchmark)) k["Manuelle Reserve", "Reserve"] <- r$benchmark[i]
    tab <- fmt_frame(k, index_name = "Kennzahl")
    tagList(html_table(tab),
            if (!is.null(r$benchmark)) div(class = "caption", sprintf("Die manuelle Reserve deckt %s der Simulationen ab.",
                                                                      de_pct(mean(x <= r$benchmark[i])))))
  })
  output$tbl_bs_k <- renderUI({
    bs <- res()$bs; req(bs); tot <- bs$res_total
    k <- data.frame(Gesamtreserve = c(mean(tot), stats::median(tot), stats::quantile(tot, c(0.75, 0.95, 0.995)),
                                      stats::sd(tot)),
                    row.names = c("Mittel", "Median", "75 %", "95 %", "99,5 %", "Standardfehler"))
    html_table(fmt_frame(k, index_name = "Kennzahl"))
  })
  output$bs_n <- renderUI({ bs <- res()$bs; req(bs)
    div(class = "caption", formatC(length(bs$res_total), big.mark = ".", decimal.mark = ",", format = "d"), " Simulationen") })
  output$tbl_bs_o <- renderUI({
    bs <- res()$bs; req(bs); ro <- bs$res_origin
    qo <- data.frame(Mittel = colMeans(ro), SE = apply(ro, 2, stats::sd),
                     `75 %` = apply(ro, 2, stats::quantile, 0.75), `95 %` = apply(ro, 2, stats::quantile, 0.95),
                     `99,5 %` = apply(ro, 2, stats::quantile, 0.995), check.names = FALSE, row.names = tri()$origins)
    html_table(fmt_frame(qo, index_name = tri()$labels$origin))
  })

  output$diag_trend <- renderUI({
    d <- diag_res()
    if (is.null(d)) return(div(class = "alert alert-info py-2 small", "F\u00fcr die Residuen-Diagnose ist das Dreieck zu klein."))
    if (is.null(d$trend)) return(NULL)
    tr <- d$trend
    msg <- if (tr[["p"]] < 0.05) sprintf("Hinweis auf einen Trend \u00fcber die Kalenderperioden: Steigung %s Standardabweichungen je Periode (p = %s). Faktoren aus \u00e4lteren Diagonalen passen dann wom\u00f6glich nicht mehr \u2013 z. B. nur die letzten n Diagonalen verwenden.", de(tr[["slope"]], 3), de(tr[["p"]], 3))
           else sprintf("Kein deutlicher Trend \u00fcber die Kalenderperioden (Steigung %s Standardabweichungen je Periode, p = %s).", de(tr[["slope"]], 3), de(tr[["p"]], 2))
    div(class = paste("alert py-2 small", if (tr[["p"]] < 0.05) "alert-warning" else "alert-light"), msg)
  })
  output$fig_res_o <- renderPlotly({ d <- diag_res(); req(d); fig_resid(d$data, "origin", tri()$origins, tri()$labels$origin) })
  output$fig_res_d <- renderPlotly({ d <- diag_res(); req(d); fig_resid(d$data, "dev", tri()$devs, tri()$labels$dev) })
  output$fig_res_c <- renderPlotly({ d <- diag_res(); req(d); fig_resid(d$data, "cal_label", d$cal_labels, "Kalenderperiode") })
  output$fig_res_q <- renderPlotly({ d <- diag_res(); req(d); fig_qq(d$data) })

  # --- Sensitivitaet --------------------------------------------------------------
  output$sens_pick <- renderUI({
    m <- setdiff(names(res()$reserves), c(BENCHMARK, "ODP-Bootstrap (Mittel)", "Munich Chain Ladder"))
    selectInput("sens_method", "Verfahren", m, selected = isolate(sens_method()), width = "280px")
  })
  output$sens_base <- renderUI({
    s <- sens()
    div(class = "small mt-4", HTML(sprintf("Basis-Reserve <b>%s</b>: <b>%s</b>%s", sens_method(), de(attr(s, "base")),
        if (length(selection_text())) paste0(" (mit Faktorauswahl: ", paste(selection_text(), collapse = "; "), ")") else "")))
  })
  sens_eff <- reactive({ s <- sens(); s[!is.na(s$Abweichung) & abs(s$Abweichung) > 0.5, , drop = FALSE] })
  output$fig_tornado <- renderPlotly({ s <- sens_eff(); req(nrow(s) > 0); fig_tornado(s) })
  output$tbl_sens <- renderUI(html_table(fmt_frame(sens(), spct = "Abweichung %", index_name = "Szenario"), first_strong = TRUE))
  output$sens_none <- renderUI({
    s <- sens(); none <- rownames(s)[is.na(s$Abweichung) | abs(s$Abweichung) <= 0.5]
    tagList(
      if (length(none)) div(class = "caption", paste0("Ohne Wirkung auf dieses Verfahren: ", paste(none, collapse = "; "), ".")),
      if (is.null(elr_val())) div(class = "caption", "Die Szenarien zur A-priori-Schadenquote entfallen, weil keine eigene Quote gesetzt ist (keine Pr\u00e4mien oder Cape-Cod-Quote gew\u00e4hlt)."))
  })

  # --- Actual vs Expected ---------------------------------------------------------
  ave_safe <- reactive(tryCatch(ave(), error = function(e) structure(list(msg = conditionMessage(e)), class = "ave_error")))
  output$ave_msg <- renderUI({
    a <- ave_safe()
    if (inherits(a, "ave_error")) return(div(class = "alert alert-danger py-2 small", "Actual vs. Expected nicht m\u00f6glich: ", a$msg))
    tagList(
      div(class = "caption", if (a$backtest) "Vorstand: aktuelles Dreieck ohne die j\u00fcngste Diagonale. Faktorauswahl wie eingestellt." else "Vorstand: hochgeladene Vorjahresdatei (eigene Faktoren, ohne manuelle Ausschl\u00fcsse)."),
      if (length(a$new_origins)) div(class = "caption", paste0("Ohne Vorstand und daher nicht bewertet: ", paste(a$new_origins, collapse = ", "), ".")),
      if (length(a$no_factor)) div(class = "caption", paste0("Ohne CL-Erwartung (im Vorstand kein Faktor f\u00fcr diesen \u00dcbergang): ", paste(a$no_factor, collapse = ", "), ".")))
  })
  output$ave_kpis <- renderUI({
    a <- ave_safe(); req(!inherits(a, "ave_error")); s <- a$table["Summe", ]
    boxes <- list(value_box("Tats\u00e4chlicher Zuwachs", de(s[["Tats\u00e4chlich"]])),
                  value_box("Erwartet nach CL", de(s$`Erwartet CL`), p(class = "small", "A/E ", de(s$`A/E CL`, 2))))
    if (!is.na(s$`Erwartet BF`)) boxes[[3]] <- value_box("Erwartet nach BF", de(s$`Erwartet BF`), p(class = "small", "A/E ", de(s$`A/E BF`, 2)))
    do.call(layout_column_wrap, c(boxes, list(width = 1 / length(boxes), fill = FALSE, class = "mt-2 mb-2")))
  })
  output$fig_ave <- renderPlotly({ a <- ave_safe(); req(!inherits(a, "ave_error")); fig_ave(a$table) })
  output$tbl_ave <- renderUI({
    a <- ave_safe(); req(!inherits(a, "ave_error")); tab <- a$table
    ft <- fmt_frame(tab[, setdiff(names(tab), "\u00dcbergang")], dec = list(`A/E CL` = 2, `A/E BF` = 2), index_name = tri()$labels$origin)
    ft <- cbind(ft[1], step = tab[["\u00dcbergang"]], ft[-1]); names(ft)[2] <- "\u00dcbergang"
    html_table(ft, bold_rows = "Summe")
  })

  # --- Rueckblick: Modell vor n Jahren, dann rollierend AvE und CDR -------------------
  rb <- reactive({
    y <- input$rb_years; req(!is.null(y), !is.na(y))
    tryCatch(rolling_backtest(tri(), premiums(), elr_val(), years = y, refit = !identical(input$rb_mode, "fixed"),
                              exclude = excl(), last_n = last_n(), tail = tail_v(), manual = loaded()$manual_ave),
             error = function(e) structure(list(msg = conditionMessage(e)), class = "rb_error"))
  })
  output$rb_body <- renderUI({
    b <- rb()
    if (inherits(b, "rb_error")) return(div(class = "alert alert-info mt-2", b$msg))
    sm <- b$summary; tot <- sm["Summe", ]; yrs <- setdiff(rownames(sm), "Summe")
    u <- b$ultimates; u0 <- u["Summe", 1]; u1 <- u["Summe", ncol(u)]
    has_bf <- "CDR BF" %in% names(sm); has_man <- "Erwartet manuell" %in% names(sm)
    boxes <- list(
      value_box(paste0("Modell gew\u00e4hlt: Stand ", b$start), de(b$start_reserve),
                p(class = "small", paste0("Chain-Ladder-Reserve damals \u00b7 ", length(b$start_origins), " ", tri()$labels$origin_pl))),
      value_box(paste0("A/E ", yrs[1], "\u2013", tail(yrs, 1)),
                paste0(de(tot$`A/E CL`, 2), if (has_bf) paste0(" / ", de(tot$`A/E BF`, 2)), if (has_man) paste0(" / ", de(tot$`A/E manuell`, 2))),
                p(class = "small", paste0("Chain Ladder", if (has_bf) " / BF", if (has_man) " / manuell", " \u00b7 tats\u00e4chlich ", de(tot[["Tats\u00e4chlich"]]),
                  ", erwartet CL ", de(tot$`Erwartet CL`)))),
      value_box("Kumuliertes CDR (Chain Ladder)", de(tot$`CDR CL`),
                p(class = "small", paste0(if (b$refit) paste0("Merz-W\u00fcthrich-SE ", de(tot$`SE CDR (MW)`), " \u00b7 ", de(tot$`CDR / SE`, 2), " SE")
                                   else "negativ = Abwicklungsverlust", if (has_bf) paste0(" \u00b7 BF ", de(tot$`CDR BF`)),
                                   if (has_man && !is.na(tot$`CDR manuell`)) paste0(" \u00b7 manuell ", de(tot$`CDR manuell`))))),
      value_box(paste0("Endschaden ", tri()$labels$origin_pl, " bis ", b$start), paste(de(u0), "\u2192", de(u1)),
                p(class = "small", paste0("Sch\u00e4tzung ", b$start, " \u2192 ", b$today, " (", de_pct(u1 / u0 - 1, sign = TRUE), ")"))))
    nf <- tot$`ohne Faktor`
    if (!b$refit) sm <- sm[, setdiff(names(sm), c("SE CDR (MW)", "CDR / SE")), drop = FALSE]
    part <- function(cols) {
      x <- sm[, intersect(cols, names(sm)), drop = FALSE]
      f <- fmt_frame(x, dec = list(`A/E CL` = 2, `A/E BF` = 2, `A/E manuell` = 2, `CDR / SE` = 2), index_name = "Kalenderjahr")
      f[f == "NA" | f == "\u2013"] <- ""; f
    }
    fmt_ave <- part(c("Tats\u00e4chlich", "Erwartet CL", "A/E CL", "Erwartet BF", "A/E BF", "Erwartet manuell", "A/E manuell", "ohne Faktor"))
    fmt_cdr <- part(c("Reserve Anfang CL", "Reserve Ende CL", "CDR CL", "SE CDR (MW)", "CDR / SE", "Kum. CDR CL",
                      "CDR BF", "Kum. CDR BF", "CDR manuell", "Kum. CDR manuell"))
    tagList(
      div(class = "caption", paste0("Das Dreieck wird auf den Stand ", b$start, " zur\u00fcckgeschnitten; dort wird das Modell gew\u00e4hlt (Faktorauswahl, Tail-Faktor",
        if (!is.null(b$elr_bf)) paste0(", A-priori-Schadenquote ", de_pct(b$elr_bf)) else "", "). ",
        if (b$refit) "In jedem Folgejahr werden die Faktoren mit diesen Einstellungen neu gesch\u00e4tzt \u2013 so, wie es j\u00e4hrlich gemacht worden w\u00e4re. "
        else "Die Faktoren von damals werden unver\u00e4ndert fortgeschrieben. ",
        "Ausgewertet wird je Kalenderjahr \u00fcber die Anfallperioden, die zu Jahresbeginn schon bekannt waren.")),
      if (nf > 0) div(class = "alert alert-warning py-2 small", paste0(nf, " Mal gab es f\u00fcr den \u00dcbergang eines Anfalljahres zu Jahresbeginn noch keinen gesch\u00e4tzten Faktor (Erwartung 0, Reserve ohne diesen Schritt). ",
        "Diese F\u00e4lle gehen nicht in A/E ein, wohl aber ins CDR.")),
      if (has_man) div(class = "caption", paste0("Manuelle Werte aus Blatt 5 f\u00fcr ", paste(b$manual_used, collapse = ", "),
        if (length(b$manual_unused)) paste0("; au\u00dferhalb des R\u00fcckblicks und nicht verwendet: ", paste(b$manual_unused, collapse = ", ")) else "",
        ". Die tats\u00e4chlichen Zahlungen stammen f\u00fcr alle Vergleiche aus dem Dreieck.")),
      if (!has_man && length(b$manual_unused)) div(class = "alert alert-info py-2 small", paste0("Die manuellen Werte aus Blatt 5 (",
        paste(b$manual_unused, collapse = ", "), ") liegen au\u00dferhalb des R\u00fcckblicks.")),
      do.call(layout_column_wrap, c(list(width = 1 / 4, fill = FALSE), boxes)),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Actual vs Expected je Kalenderjahr"), plotlyOutput("fig_rb_ave", height = "auto")),
        card(card_header(if (b$refit) "Beobachtetes CDR mit 95 %-Band nach Merz-W\u00fcthrich" else "Beobachtetes CDR je Kalenderjahr"),
             plotlyOutput("fig_rb_cdr", height = "auto"))),
      div(class = "caption", paste0("Links: erwartete Zahlungen nach Chain Ladder",
        if (has_bf) ", Bornhuetter-Ferguson" else "", if (has_man) " und manuell (Blatt 5)" else "", " gegen\u00fcber den tats\u00e4chlichen Zahlungen des Kalenderjahres. Rechts: CDR = Reserve zu Jahresbeginn \u2212 (Zahlungen + Reserve zum Jahresende); negativ = Abwicklungsverlust.",
        if (b$refit) " Die Fehlerbalken zeigen \u00b11,96 Standardfehler nach Merz-W\u00fcthrich, berechnet mit dem Stand zu Jahresbeginn \u2013 ein Jahr au\u00dferhalb des Bandes ist ein Warnsignal." else "")),
      h5("Actual vs Expected je Kalenderjahr", class = "mt-3"),
      html_table(fmt_ave, bold_rows = "Summe"),
      div(class = "caption", paste0("Tats\u00e4chlich: Zahlungen im Kalenderjahr f\u00fcr die Anfallperioden, die zu Jahresbeginn bekannt waren. A/E CL ohne F\u00e4lle ohne Faktor; \u201eohne Faktor\u201c: Anzahl Anfallperioden ohne gesch\u00e4tzten Faktor f\u00fcr ihren \u00dcbergang.",
        if (has_man) " A/E manuell = Tats\u00e4chlich / manuell erwartete Zahlung (Blatt 5)." else "")),
      h5("Claims Development Result je Kalenderjahr", class = "mt-3"),
      html_table(fmt_cdr, bold_rows = "Summe"),
      div(class = "caption", paste0("Reserve Anfang/Ende: Chain-Ladder-Reserve der zu Jahresbeginn bekannten Anfallperioden. ",
        if (b$refit) "CDR / SE: beobachtetes CDR in Standardfehlern nach Merz-W\u00fcthrich; die Summe verwendet \u221a(\u03a3 SE\u00b2), weil die CDRs verschiedener Jahre unkorreliert sind. " else "",
        if (has_man) "CDR manuell: Wert aus Blatt 5, unver\u00e4ndert \u00fcbernommen." else "")),
      layout_columns(col_widths = c(7, 5),
        card(card_header(paste0("Endschadensch\u00e4tzung je Bewertungsjahr (", tri()$labels$origin_pl, " bis ", b$start, ")")),
             plotlyOutput("fig_rb_ult", height = "auto")),
        card(card_header("Faktoren: damals und heute"),
             html_table(fmt_frame(b$factors, dec = list(`Faktor Start` = 3, `Faktor heute` = 3), spct = "Abw.", index_name = "\u00dcbergang")),
             div(class = "caption", "\u201eAnzahl\u201c: Einzelfaktoren, auf denen die Sch\u00e4tzung beruht; 0 = zum Start keine Daten (Faktor 1)."))),
      h5("Endschadensch\u00e4tzung je Bewertungsjahr", class = "mt-3"),
      html_table(fmt_frame(u, index_name = tri()$labels$origin), bold_rows = "Summe"),
      div(class = "caption", "Chain-Ladder-Endschaden, wie er zum jeweiligen Stichtag gesch\u00e4tzt worden w\u00e4re. Steigende Zeilen bedeuten Nachreservierung, fallende Abwicklungsgewinne; die \u00c4nderung von Spalte zu Spalte ist das CDR des Anfalljahres."),
      h5("Detail je Kalenderjahr", class = "mt-3"),
      selectInput("rb_year", NULL, rev(yrs), selected = if (!is.null(isolate(input$rb_year)) && isolate(input$rb_year) %in% yrs) isolate(input$rb_year) else tail(yrs, 1), width = "160px"),
      uiOutput("rb_detail"))
  })
  output$rb_detail <- renderUI({
    b <- rb(); req(!inherits(b, "rb_error"), input$rb_year)
    d <- b$detail[b$detail$Jahr == input$rb_year, , drop = FALSE]; req(nrow(d) > 0)
    keep <- setdiff(names(d), c("Jahr", "Anfall", "\u00dcbergang", "Faktor vorhanden", "Reserve Anfang BF", "Reserve Ende BF"))
    x <- d[, keep, drop = FALSE]; rownames(x) <- d$Anfall
    x$`A/E CL` <- ifelse(d$`Faktor vorhanden` & d$`Erwartet CL` != 0, d[["Tats\u00e4chlich"]] / d$`Erwartet CL`, NA)
    if ("Erwartet BF" %in% names(x)) x$`A/E BF` <- ifelse(d$`Erwartet BF` != 0, d[["Tats\u00e4chlich"]] / d$`Erwartet BF`, NA)
    ord <- intersect(c("Reserve Anfang CL", "Erwartet CL", "Erwartet BF", "Tats\u00e4chlich", "A/E CL", "A/E BF", "Reserve Ende CL",
                       "CDR CL", if (b$refit) "SE CDR (MW)", "CDR BF"), names(x))
    x <- x[, ord, drop = FALSE]
    ft <- fmt_frame(x, dec = list(`A/E CL` = 2, `A/E BF` = 2), index_name = tri()$labels$origin)
    ft <- cbind(ft[1], step = ifelse(d$`Faktor vorhanden`, d[["\u00dcbergang"]], paste0(d[["\u00dcbergang"]], " (kein Faktor)")), ft[-1])
    names(ft)[2] <- "\u00dcbergang"; ft[ft == "NA"] <- ""
    tagList(html_table(ft), div(class = "caption", paste0("Kalenderjahr ", input$rb_year, ": je Anfallperiode, die zu Jahresbeginn bekannt war.")))
  })
  output$fig_rb_ave <- renderPlotly({
    b <- rb(); req(!inherits(b, "rb_error")); t <- b$summary[rownames(b$summary) != "Summe", ]; x <- rownames(t)
    p <- plot_ly(height = 360)
    for (nm in c("Erwartet CL", "Erwartet BF", "Erwartet manuell", "Tats\u00e4chlich")) {
      if (!nm %in% names(t)) next
      col <- switch(nm, `Erwartet CL` = unname(color_for("Chain Ladder")), `Erwartet BF` = unname(color_for("Bornhuetter-Ferguson")),
                    `Erwartet manuell` = MANUAL_COLOR, INK_2)
      ae <- switch(nm, `Erwartet CL` = t$`A/E CL`, `Erwartet BF` = t$`A/E BF`, `Erwartet manuell` = t$`A/E manuell`, NULL)
      p <- p %>% add_trace(x = x, y = t[[nm]], type = "bar", name = nm, marker = list(color = col),
                           hovertext = paste0(nm, ": ", de(t[[nm]]), if (!is.null(ae)) paste0(" \u00b7 A/E ", de(ae, 2)) else ""),
                           hoverinfo = "text")
    }
    p %>% base_layout(barmode = "group", xaxis = list(title = "", type = "category"),
                      yaxis = list(title = "Zahlungen im Kalenderjahr", tickformat = ",.0f", gridcolor = GRID),
                      legend = list(orientation = "h", y = 1.1, x = 0))
  })
  output$fig_rb_cdr <- renderPlotly({
    b <- rb(); req(!inherits(b, "rb_error")); t <- b$summary[rownames(b$summary) != "Summe", ]; x <- rownames(t)
    se <- t$`SE CDR (MW)`
    p <- plot_ly(height = 360) %>%
      add_trace(x = x, y = t$`CDR CL`, type = "bar", name = "CDR Chain Ladder",
                marker = list(color = ifelse(t$`CDR CL` < 0, "#c0392b", unname(color_for("Chain Ladder")))),
                error_y = if (b$refit) list(type = "data", symmetric = TRUE, array = 1.96 * se, color = INK_2, thickness = 1.2, width = 6) else NULL,
                hovertext = paste0("CDR CL: ", de(t$`CDR CL`), if (b$refit) paste0("<br>SE (MW): ", de(se), " \u00b7 ", de(t$`CDR / SE`, 2), " SE") else ""),
                hoverinfo = "text")
    if ("CDR BF" %in% names(t))
      p <- p %>% add_trace(x = x, y = t$`CDR BF`, type = "scatter", mode = "markers", name = "CDR BF",
                           marker = list(color = unname(color_for("Bornhuetter-Ferguson")), size = 10, symbol = "diamond"),
                           hovertext = paste0("CDR BF: ", de(t$`CDR BF`)), hoverinfo = "text")
    if ("CDR manuell" %in% names(t) && any(!is.na(t$`CDR manuell`)))
      p <- p %>% add_trace(x = x, y = t$`CDR manuell`, type = "scatter", mode = "markers", name = "CDR manuell",
                           marker = list(color = MANUAL_COLOR, size = 11, symbol = "square", line = list(color = "white", width = 1)),
                           hovertext = paste0("CDR manuell: ", de(t$`CDR manuell`)), hoverinfo = "text")
    p <- p %>% add_trace(x = x, y = t$`Kum. CDR CL`, type = "scatter", mode = "lines+markers", name = "kumuliert CL",
                         line = list(color = MUTED, dash = "dot", width = 2), marker = list(color = MUTED, size = 6),
                         hovertext = paste0("kumuliert CL: ", de(t$`Kum. CDR CL`)), hoverinfo = "text")
    p %>% base_layout(xaxis = list(title = "", type = "category"),
                      yaxis = list(title = "CDR (negativ = Verlust)", tickformat = ",.0f", gridcolor = GRID, zeroline = TRUE, zerolinecolor = INK_2),
                      legend = list(orientation = "h", y = 1.1, x = 0))
  })
  output$fig_rb_ult <- renderPlotly({
    b <- rb(); req(!inherits(b, "rb_error")); u <- b$ultimates; u <- u[rownames(u) != "Summe", , drop = FALSE]; x <- colnames(u)
    pal <- grDevices::colorRampPalette(c("#9cc3ee", "#2a78d6", "#0b3d7a"))(nrow(u))
    p <- plot_ly(height = 360)
    for (i in seq_len(nrow(u))) p <- p %>% add_trace(x = x, y = unlist(u[i, ]), type = "scatter", mode = "lines+markers", name = rownames(u)[i],
                                                     line = list(color = pal[i], width = 2), marker = list(color = pal[i], size = 6),
                                                     hovertext = paste0(rownames(u)[i], " \u00b7 Stand ", x, ": ", de(unlist(u[i, ]))), hoverinfo = "text")
    p %>% base_layout(xaxis = list(title = "Bewertungsjahr", type = "category"),
                      yaxis = list(title = "gesch\u00e4tzter Endschaden", tickformat = ",.0f", gridcolor = GRID),
                      legend = list(orientation = "h", y = 1.12, x = 0))
  })

  # --- Munich Chain Ladder --------------------------------------------------------
  # --- Ein-Jahres-Sicht (Merz-Wuethrich / CDR) -----------------------------------
  output$oy_body <- renderUI({
    r <- res(); oy <- r$oy
    if (is.null(oy) || !isTRUE(oy$available))
      return(div(class = "alert alert-info mt-3", oy$reason %||% "Die Ein-Jahres-Sicht ist f\u00fcr dieses Dreieck nicht verf\u00fcgbar."))
    sm <- oy$summary; has_bs <- !is.null(oy$cdr_total)
    g <- function(nm) if (nm %in% names(sm)) sm[[nm]] else NA
    boxes <- list(
      value_box("Standardfehler 1 Jahr (Merz-W\u00fcthrich)", de(g("SE 1 Jahr (MW)")),
                p(class = "small", "Ultimo (Mack) ", de(g("SE Ultimo (Mack)")), " \u00b7 Anteil ", de_pct(g("Anteil 1 Jahr / Ultimo")))),
      if (has_bs) value_box("Standardfehler 1 Jahr (Bootstrap)", de(g("SE 1 Jahr (Bootstrap)")),
                p(class = "small", "Ultimo (Bootstrap) ", de(stats::sd(r$bs$res_total)), " \u00b7 Anteil ",
                  de_pct(g("SE 1 Jahr (Bootstrap)") / stats::sd(r$bs$res_total)))),
      value_box("Verlust 99,5 % in einem Jahr", de(if (has_bs) g("Verlust 99,5 % 1 Jahr (Bootstrap)") else g("Verlust 99,5 % 1 Jahr (MW, lognormal)")),
                p(class = "small", if (has_bs) paste0("Bootstrap \u00b7 Merz-W\u00fcthrich (lognormal) ", de(g("Verlust 99,5 % 1 Jahr (MW, lognormal)")))
                                   else "Merz-W\u00fcthrich, Lognormal-N\u00e4herung")),
      value_box("Verlust 99,5 % bis Ultimo", de(if (has_bs) g("Verlust 99,5 % Ultimo (Bootstrap)") else g("Verlust 99,5 % Ultimo (Mack, lognormal)")),
                p(class = "small", if (has_bs) paste0("Bootstrap \u00b7 Mack (lognormal) ", de(g("Verlust 99,5 % Ultimo (Mack, lognormal)")))
                                   else "Mack, Lognormal-N\u00e4herung")))
    boxes <- Filter(Negate(is.null), boxes)
    tagList(
      div(class = "caption mt-3", "Ein-Jahres-Sicht (Solvency II): Wie stark kann sich die Chain-Ladder-Sch\u00e4tzung innerhalb des n\u00e4chsten Jahres \u00e4ndern? Ma\u00dfgeblich ist das Claims Development Result (CDR) = Reserve heute \u2212 (Zahlungen im n\u00e4chsten Jahr + neu gesch\u00e4tzte Reserve). Ein negatives CDR ist ein Abwicklungsverlust. Verluste sind gegen\u00fcber der heutigen Chain-Ladder-Reserve angegeben."),
      do.call(layout_column_wrap, c(list(width = 1 / length(boxes), fill = FALSE), boxes)),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Standardfehler je Anfallperiode"), plotlyOutput("fig_oy_se", height = "auto")),
        if (has_bs) card(card_header("Verteilung des CDR im n\u00e4chsten Jahr (Bootstrap)"), plotlyOutput("fig_oy_hist", height = "auto"))
        else card(card_header("Verteilung des CDR"), div(class = "alert alert-info", "Ohne Bootstrap keine simulierte Verteilung."))),
      div(class = "caption", "Links: Ultimo-Sicht (Mack) gegen\u00fcber Ein-Jahres-Sicht (Merz-W\u00fcthrich) je Anfallperiode; Punkte: Ein-Jahres-Standardfehler aus dem Bootstrap. Rechts: simuliertes CDR aller Anfallperioden; links der gestrichelten Linie liegen die 0,5 % schlechtesten Jahre."),
      h5("Ergebnisse je Anfallperiode", class = "mt-3"),
      html_table(fmt_frame(oy$table, pct = "Anteil 1 Jahr / Ultimo", index_name = tri()$labels$origin), bold_rows = "Summe"),
      div(class = "caption", paste0("SE = Standardfehler. \u201edavon Prozess\u201c: Zufall der Zahlungen im n\u00e4chsten Jahr; \u201edavon Parameter\u201c: \u00c4nderung der gesch\u00e4tzten Abwicklungsfaktoren durch die neue Diagonale (die Teile addieren sich quadratisch). ",
                                    "Die Ein-Jahres-Werte beziehen sich auf die Chain-Ladder-Reserve mit der aktuellen Faktorauswahl",
                                    if (tail_v() > 1) "; der Tail-Faktor wird wie bei Mack als feste Skalierung ber\u00fccksichtigt" else "", ".")))
  })
  output$fig_oy_se <- renderPlotly({
    oy <- res()$oy; req(isTRUE(oy$available))
    t <- oy$table[rownames(oy$table) != "Summe", ]; keep <- t[["Reserve CL"]] > 0 | t[["SE Ultimo (Mack)"]] > 0
    t <- t[keep, , drop = FALSE]; x <- rownames(t)
    p <- plot_ly(height = 380) %>%
      add_trace(x = x, y = t[["SE Ultimo (Mack)"]], type = "bar", name = "Ultimo (Mack)", marker = list(color = "#9cc3ee"),
                hovertext = paste0("Ultimo (Mack): ", de(t[["SE Ultimo (Mack)"]])), hoverinfo = "text") %>%
      add_trace(x = x, y = t[["SE 1 Jahr (MW)"]], type = "bar", name = "1 Jahr (Merz-W\u00fcthrich)", marker = list(color = ACCENT),
                hovertext = paste0("1 Jahr (Merz-W\u00fcthrich): ", de(t[["SE 1 Jahr (MW)"]]), " \u00b7 ", de_pct(t[["Anteil 1 Jahr / Ultimo"]]), " von Ultimo"),
                hoverinfo = "text")
    if (!is.null(t[["SE 1 Jahr (Bootstrap)"]]))
      p <- p %>% add_trace(x = x, y = t[["SE 1 Jahr (Bootstrap)"]], type = "scatter", mode = "markers", name = "1 Jahr (Bootstrap)",
                           marker = list(color = "#4a3aa7", size = 10, symbol = "diamond"),
                           hovertext = paste0("1 Jahr (Bootstrap): ", de(t[["SE 1 Jahr (Bootstrap)"]])), hoverinfo = "text")
    p %>% base_layout(barmode = "group", xaxis = list(title = "", type = "category"),
                      yaxis = list(title = "Standardfehler", tickformat = ",.0f", gridcolor = GRID),
                      legend = list(orientation = "h", y = 1.1, x = 0))
  })
  output$fig_oy_hist <- renderPlotly({
    oy <- res()$oy; req(isTRUE(oy$available), oy$cdr_total)
    x <- oy$cdr_total; q <- unname(stats::quantile(x, 0.005)); m <- mean(x)
    lab <- list(list(x = q, text = paste0("0,5 %-Quantil: ", de(q)), y = 1, dash = "dash", col = "#c0392b", anc = "left"),
                list(x = m, text = paste0("Mittel: ", de(m)), y = 0.9, dash = "dash", col = INK_2, anc = if (m <= 0) "right" else "left"),
                list(x = 0, text = "0 = Reserve best\u00e4tigt", y = 0.8, dash = "dot", col = BENCH_COLOR, anc = if (m <= 0) "left" else "right"))
    plot_ly(x = x, type = "histogram", nbinsx = 60, height = 380,
            marker = list(color = ACCENT, line = list(color = "white", width = 1)),
            hovertemplate = "CDR %{x:,.0f}<br>%{y} Simulationen<extra></extra>") %>%
      base_layout(xaxis = list(title = "CDR gesamt (negativ = Abwicklungsverlust)", tickformat = ",.0f", gridcolor = GRID),
                  yaxis = list(title = "Anzahl Simulationen", gridcolor = GRID), bargap = 0.05,
                  shapes = lapply(lab, function(l) vline(l$x, dash = l$dash, color = l$col)),
                  annotations = lapply(lab, function(l) list(x = l$x, y = l$y, yref = "paper", xanchor = l$anc, yanchor = "top",
                    showarrow = FALSE, bgcolor = "rgba(255,255,255,0.85)", text = l$text, font = list(size = 12, color = l$col))))
  })

  output$mcl_body <- renderUI({
    r <- res(); m <- r$mcl
    if (is.null(m)) return(card(class = "mt-3", card_header("Munich Chain Ladder"),
      p("Munich Chain Ladder rechnet bezahlte und gemeldete Sch\u00e4den gemeinsam hoch. Daf\u00fcr braucht die Excel-Datei auf ",
        tags$b("Blatt 4"), " das Dreieck der gemeldeten Sch\u00e4den \u2013 im gleichen Aufbau wie Blatt 1 (bezahlte Sch\u00e4den).")))
    t <- m$table; s <- t["Summe", ]; cl_res <- sum(r$cl$reserve)
    tagList(
      layout_column_wrap(width = 1 / 4, fill = FALSE, class = "mt-3",
        value_box("Reserve MCL (bezahlt)", de(sum(r$reserves[["Munich Chain Ladder"]])),
                  p(class = "small", paste0("CL bezahlt ", de(cl_res), " (", de_pct(sum(r$reserves[["Munich Chain Ladder"]]) / cl_res - 1, sign = TRUE), ")"))),
        value_box("Endschaden MCL", paste(de(s[["MCL bezahlt"]]), "/", de(s[["MCL gemeldet"]])),
                  p(class = "small", "bezahlt / gemeldet \u00b7 P/I ", de_pct(s[["P/I MCL"]]))),
        value_box("Endschaden CL getrennt", paste(de(s[["CL bezahlt"]]), "/", de(s[["CL gemeldet"]])),
                  p(class = "small", "bezahlt / gemeldet \u00b7 P/I ", de_pct(s[["P/I CL"]]))),
        value_box("Korrelation \u03bb", paste(de(m$lambda_p, 2), "/", de(m$lambda_i, 2)),
                  p(class = "small", "bezahlt / gemeldet"))),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Endschaden je Anfallperiode"), plotlyOutput("fig_mcl_ult", height = "auto")),
        card(card_header("Verh\u00e4ltnis bezahlt / gemeldet (P/I)"), plotlyOutput("fig_mcl_pi", height = "auto"))),
      div(class = "caption", "Getrennt hochgerechnet laufen bezahlt und gemeldet oft auseinander (P/I zum Endschaden deutlich unter oder \u00fcber 100 %). Munich Chain Ladder nutzt die Information des jeweils anderen Dreiecks und bringt beide n\u00e4her zusammen."),
      h5("Ergebnisse je Anfallperiode", class = "mt-3"),
      html_table(fmt_frame(t, pct = c("P/I aktuell", "P/I CL", "P/I MCL"), index_name = tri()$labels$origin), bold_rows = "Summe"),
      div(class = "caption", paste0("Reserve MCL = Endschaden MCL bezahlt \u2212 bezahlter Stand",
                                    if (tail_v() > 1) " (in \u00dcbersicht und Vergleich zus\u00e4tzlich mit dem Tail-Faktor multipliziert)" else "", ".")),
      h5("Residuen und Korrelation", class = "mt-3"),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Bezahlt: Faktor-Residuen gegen I/P-Residuen"), plotlyOutput("fig_mcl_rp", height = "auto")),
        card(card_header("Gemeldet: Faktor-Residuen gegen P/I-Residuen"), plotlyOutput("fig_mcl_ri", height = "auto"))),
      div(class = "caption", "Jeder Punkt ist ein Einzelfaktor. Die Steigung der Linie ist \u03bb: Je steiler, desto st\u00e4rker folgt die Abwicklung dem Verh\u00e4ltnis bezahlt/gemeldet. Ein positives \u03bb bedeutet: Ist bezahlt im Verh\u00e4ltnis zu gemeldet niedrig (I/P hoch), wachsen die Zahlungen danach \u00fcberdurchschnittlich."))
  })
  output$fig_mcl_ult <- renderPlotly({
    m <- res()$mcl; req(m); t <- m$table[rownames(m$table) != "Summe", ]; x <- rownames(t)
    cols <- c(`CL bezahlt` = "#9cc3ee", `CL gemeldet` = "#c9c7c0", `MCL bezahlt` = "#d0457c", `MCL gemeldet` = "#7d2248")
    p <- plot_ly(height = 380)
    for (nm in names(cols)) p <- p %>% add_trace(x = x, y = t[[nm]], type = "bar", name = nm, marker = list(color = cols[[nm]]),
                                                 hovertext = paste0(nm, ": ", de(t[[nm]])), hoverinfo = "text")
    p %>% base_layout(barmode = "group", xaxis = list(title = "", type = "category"),
                      yaxis = list(title = "Endschaden", tickformat = ",.0f", gridcolor = GRID),
                      legend = list(orientation = "h", y = 1.1, x = 0))
  })
  output$fig_mcl_pi <- renderPlotly({
    m <- res()$mcl; req(m); t <- m$table[rownames(m$table) != "Summe", ]; x <- rownames(t)
    ser <- list(`aktuell` = list(t$`P/I aktuell`, MUTED, "dot"), `CL getrennt` = list(t$`P/I CL`, ACCENT, "solid"),
                `Munich Chain Ladder` = list(t$`P/I MCL`, "#d0457c", "solid"))
    p <- plot_ly(height = 380)
    for (nm in names(ser)) p <- p %>% add_trace(x = x, y = 100 * ser[[nm]][[1]], type = "scatter", mode = "lines+markers", name = nm,
                                                line = list(color = ser[[nm]][[2]], dash = ser[[nm]][[3]], width = 2),
                                                marker = list(color = ser[[nm]][[2]], size = 7),
                                                hovertext = paste0(nm, ": ", de(100 * ser[[nm]][[1]], 1), " %"), hoverinfo = "text")
    p %>% base_layout(xaxis = list(title = "", type = "category"), yaxis = list(title = "P/I in %", gridcolor = GRID),
                      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 100, y1 = 100,
                                         line = list(color = INK_2, dash = "dash", width = 1))),
                      legend = list(orientation = "h", y = 1.1, x = 0))
  })
  mcl_resid_fig <- function(df, lam, xlab) {
    lim <- range(c(df$q, 0)) * 1.1
    plot_ly(height = 320) %>%
      add_trace(x = df$q, y = df$f, type = "scatter", mode = "markers", marker = list(color = ACCENT, opacity = 0.65, size = 8),
                hoverinfo = "x+y", name = "Residuen") %>%
      add_trace(x = lim, y = lam * lim, type = "scatter", mode = "lines", line = list(color = "#d0457c", width = 2),
                name = paste0("\u03bb = ", de(lam, 2)), hoverinfo = "name") %>%
      base_layout(xaxis = list(title = xlab, gridcolor = GRID, zeroline = TRUE, zerolinecolor = MUTED),
                  yaxis = list(title = "Residuum Einzelfaktor", gridcolor = GRID, zeroline = TRUE, zerolinecolor = MUTED),
                  legend = list(orientation = "h", y = 1.12, x = 0))
  }
  output$fig_mcl_rp <- renderPlotly({ m <- res()$mcl; req(m); mcl_resid_fig(m$resid$paid, m$lambda_p, "Residuum I/P") })
  output$fig_mcl_ri <- renderPlotly({ m <- res()$mcl; req(m); mcl_resid_fig(m$resid$incurred, m$lambda_i, "Residuum P/I") })

  # --- Daten -------------------------------------------------------------------
  output$tbl_inc <- renderUI({
    inc <- loaded()$incurred
    if (is.null(inc)) return(NULL)
    tagList(h5("Gemeldete Sch\u00e4den (Blatt 4)", class = "mt-3"),
            html_table(fmt_frame(as.data.frame(inc$cum, check.names = FALSE), index_name = inc$labels$origin)))
  })
  output$tbl_tri <- renderUI({
    t <- tri(); neg <- checks()$neg
    st <- ifelse(neg, "background:#fbe5d0", "")
    html_table(fmt_frame(as.data.frame(t$cum, check.names = FALSE), index_name = t$labels$origin), styles = st)
  })
  output$tbl_checks <- renderUI({
    tab <- checks()$table
    if (!nrow(tab)) return(div(class = "small", "Keine Auff\u00e4lligkeiten gefunden."))
    head <- tags$tr(lapply(names(tab), function(n) tags$th(n, style = "text-align:left")))
    rows <- lapply(seq_len(nrow(tab)), function(i) tags$tr(
      tags$td(span(class = paste0("sev sev-", tab$Schwere[i]), tab$Schwere[i])),
      lapply(names(tab)[-1], function(n) tags$td(tab[i, n], style = if (n == "Hinweis") "text-align:left;white-space:normal" else "text-align:left"))))
    div(class = "tbl-wrap", tags$table(class = "rt", tags$thead(head), tags$tbody(rows)))
  })
}

shinyApp(ui, server)
