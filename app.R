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
  "ELR" = "#e87ba4", "Additiv" = "#008300",
  "ODP-Bootstrap (Mittel)" = "#4a3aa7", "Manuelle Reserve" = "#3d3c38")
BENCH_COLOR <- "#3d3c38"
ACCENT <- "#2a78d6"; INK_2 <- "#52514e"; MUTED <- "#898781"; GRID <- "#e1e0d9"
SHADE <- "rgba(42,120,214,0.14)"
SIM_CHOICES <- c(0, 1000, 2000, 5000, 10000, 20000)

`%||%` <- function(a, b) if (is.null(a)) b else a

color_for <- function(nm) vapply(nm, function(n) {
  hit <- names(METHOD_COLORS)[startsWith(n, names(METHOD_COLORS))]
  if (length(hit)) METHOD_COLORS[[hit[1]]] else MUTED
}, "")

METHODS_INFO <- list(
  c("Chain Ladder", "Volumengewichtete Abwicklungsfaktoren, Hochrechnung der aktuellen Diagonale", "nein"),
  c("Mack", "Verteilungsfreies Modell hinter Chain Ladder; liefert den Standardfehler", "nein"),
  c("Expected Loss Ratio", "Endschaden = Pr\u00e4mie \u00d7 A-priori-Schadenquote; ignoriert die bisherige Abwicklung", "ja"),
  c("Bornhuetter-Ferguson", "Reserve = A-priori-Endschaden \u00d7 noch ausstehender Anteil (1 \u2212 1/CDF)", "ja"),
  c("Cape Cod", "Wie BF, aber die Schadenquote wird aus den Daten gesch\u00e4tzt (Diagonale / verbrauchte Pr\u00e4mie)", "ja"),
  c("Additiv", "Inkrementelle Schadenquoten je Abwicklungsjahr, auf die Pr\u00e4mie angewendet", "ja"),
  c("ODP-Bootstrap", "Resampling der Pearson-Residuen (England & Verrall)", "nein"))
METHOD_NOTES <- c(
  "Chain Ladder reagiert stark auf junge Anfalljahre mit kleinen Diagonalwerten; BF und Cape Cod stabilisieren dort \u00fcber die Pr\u00e4mie.",
  "Wird die Cape-Cod-Quote als A-priori-Quote verwendet, sind die Gesamtreserven von ELR, BF und Cape Cod rechnerisch identisch.",
  "Mit Tail-Faktor wird der Mack-Standardfehler nur proportional hochgerechnet (N\u00e4herung).",
  "Der Bootstrap setzt ein ODP-Modell voraus; viele negative Inkremente machen ihn unzuverl\u00e4ssig.",
  "Der Bootstrap ist zufallsbasiert; mit gleichem Zufallsstartwert und gleicher Anzahl Simulationen sind die Ergebnisse reproduzierbar.",
  "Manuelle Reserve (Benchmark): Das Niveau ist der Anteil der Szenarien, in denen die Reserve den eingetragenen Wert nicht \u00fcbersteigt (Mack: Lognormal-N\u00e4herung, Bootstrap: simulierte Verteilung). 50 % entspricht dem Median.")

CSS <- sprintf("
.caption { color: %1$s; font-size: 13px; margin: 4px 0 10px; }
table.rt { border-collapse: collapse; width: 100%%; font-size: 13px; font-variant-numeric: tabular-nums; }
table.rt th { font-weight: 500; color: %2$s; background: #f6f6f3; padding: 6px 10px; text-align: right;
              border-bottom: 1px solid %3$s; white-space: normal; vertical-align: bottom; }
table.rt td { padding: 6px 10px; text-align: right; border-bottom: 1px solid %3$s; white-space: nowrap; }
table.rt th:first-child, table.rt td:first-child { text-align: left; color: %1$s; }
table.rt td.first-strong { color: %2$s; }
.tbl-wrap { overflow-x: auto; }
.bslib-value-box .value-box-value { font-size: 1.6rem; }
", MUTED, INK_2, GRID)

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

html_table <- function(fdf, bold_rows = character(0), shade = NULL, first_strong = FALSE) {
  head <- tags$tr(lapply(names(fdf), tags$th))
  rows <- lapply(seq_len(nrow(fdf)), function(i) {
    bold <- fdf[i, 1] %in% bold_rows
    tags$tr(lapply(seq_along(fdf), function(j) {
      st <- c(if (bold) "font-weight:600",
              if (!is.null(shade) && j > 1 && shade[i, j - 1]) paste0("background:", SHADE))
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

fig_hist <- function(bs, benchmark_total = NULL) {
  tot <- bs$res_total
  q <- c(Mittel = mean(tot), `99,5 %` = unname(stats::quantile(tot, 0.995)))
  if (!is.null(benchmark_total))
    q[sprintf("Manuelle Reserve (%s%%)", formatC(100 * mean(tot <= benchmark_total), format = "f", digits = 0))] <- benchmark_total
  plot_ly(x = tot, type = "histogram", nbinsx = 60, height = 380,
          marker = list(color = ACCENT, line = list(color = "white", width = 1))) %>%
    base_layout(xaxis = list(title = "Gesamtreserve", tickformat = ",.0f", gridcolor = GRID),
                yaxis = list(title = "Anzahl Simulationen", gridcolor = GRID), bargap = 0.05,
                shapes = unname(lapply(seq_along(q), function(k) if (startsWith(names(q)[k], "Manuelle"))
                  vline(q[[k]], dash = "dot", color = BENCH_COLOR) else vline(q[[k]], dash = "dash", color = INK_2))),
                annotations = lapply(seq_along(q), function(k) list(
                  x = q[[k]], y = if (startsWith(names(q)[k], "Manuelle")) 0.9 else 1, yref = "paper",
                  xanchor = "left", yanchor = if (startsWith(names(q)[k], "Manuelle")) "top" else "bottom",
                  showarrow = FALSE, bgcolor = "rgba(255,255,255,0.85)",
                  text = paste0(names(q)[k], ": ", de(q[[k]])), font = list(size = 12, color = INK_2))))
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
    radioButtons("source", NULL, c("Demo Jahre" = "demo", "Demo Quartale" = "demo_q",
                                   "Excel-Datei hochladen" = "upload")),
    conditionalPanel("input.source == 'upload'",
                     fileInput("upload", NULL, accept = c(".xlsx", ".xls"),
                               buttonLabel = "Datei w\u00e4hlen", placeholder = "(.xlsx)"),
                     div(class = "caption", style = "margin-top:-8px",
                         "Blatt 1 Sch\u00e4den, Blatt 2 Beitr\u00e4ge, Blatt 3 manuelle Reserve. ",
                         downloadLink("dl_template", "Vorlage herunterladen"))),
    selectInput("period", "Periodizit\u00e4t", c("Jahre" = "year", "Quartale" = "quarter")), # "automatisch erkennen" = "auto", 
    #checkboxInput("to_years", "Quartale zu Jahren verdichten", FALSE),
    radioButtons("kind", "Werte im Dreieck", c("kumuliert" = "cum", "inkrementell" = "inc"), inline = TRUE),
    h6("Parameter", class = "mt-2"),
    input_switch("use_prem", "Pr\u00e4mienbasierte Verfahren", TRUE),
    checkboxInput("use_cc", "Cape-Cod-Quote als A-priori-Quote", FALSE),
    numericInput("elr", "A-priori-Schadenquote (%)", 70, min = 1, max = 300, step = 0.5),
    numericInput("tail", "Tail-Faktor", 1, min = 1, max = 3, step = 0.005),
    selectInput("sims", "Bootstrap-Simulationen",
                setNames(SIM_CHOICES, formatC(SIM_CHOICES, big.mark = ".", decimal.mark = ",", format = "d")), selected = 5000),
    numericInput("seed", "Zufallsstartwert", 42, min = 0, step = 1)
  ),
  tags$head(tags$style(HTML(CSS))),
  uiOutput("load_msg"),
  navset_underline(
    nav_panel("Daten",
              layout_columns(col_widths = c(8, 4),
                             div(h5("Kumuliertes Dreieck", class = "mt-3"), uiOutput("tbl_tri")),
                             div(h5("Beitr\u00e4ge und manuelle Reserve", class = "mt-3"), uiOutput("tbl_inputs"),
                                 div(class = "caption", "Die Werte stammen aus Blatt 2 (Beitr\u00e4ge) und Blatt 3 (manuelle Reserve) der Excel-Datei. Zum \u00c4ndern die Datei anpassen und neu hochladen. Ist die manuelle Reserve ausgef\u00fcllt, wird sie als Benchmark mit allen Verfahren verglichen."),
                                 uiOutput("prem_msg")))
    ),
    nav_panel("\u00dcbersicht",
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
      h5("Abwicklungsfaktoren", class = "mt-3"), uiOutput("tbl_fac"),
      h5("Individuelle Abwicklungsfaktoren", class = "mt-3"), uiOutput("tbl_ata"),
      div(class = "caption", "Auff\u00e4llige Einzelfaktoren (sehr gro\u00df oder < 1) sind Kandidaten f\u00fcr eine manuelle Pr\u00fcfung."),
      h5("Projiziertes Dreieck (Chain Ladder)", class = "mt-3"), uiOutput("tbl_proj"),
      div(class = "caption", "Blau hinterlegt: projizierte Werte (ohne Tail-Faktor).")
    ),
    nav_panel("Bootstrap",
      uiOutput("bs_off"),
      conditionalPanel("input.sims != '0'",
        layout_columns(col_widths = c(8, 4), class = "mt-3",
          card(card_header("Verteilung der Gesamtreserve"), plotlyOutput("fig_hist", height = "auto")),
          card(card_header("Kennzahlen"), uiOutput("tbl_bs_k"), uiOutput("bs_n"))),
        h5("Quantile je Anfallperiode", class = "mt-3"), uiOutput("tbl_bs_o"))
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
        return(list(tri = demo_triangle(), premiums = DEMO_PREMIUMS, benchmark = NULL, notes = character(0),
                    warnings = character(0), err = NULL))
      d <- if (input$source == "demo_q") demo_quarterly(isTRUE(input$to_years))
           else if (!is.null(input$upload))
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
      "Lade links eine Excel-Datei hoch: Blatt 1 Sch\u00e4den (erste Spalte Anfalljahr oder Anfallquartal wie 2024Q1, weitere Spalten Abwicklungsperioden, zuk\u00fcnftige Zellen leer), Blatt 2 Beitr\u00e4ge (Anfallperiode, Pr\u00e4mie), Blatt 3 manuelle Reserve (Anfallperiode, Wert). Blatt 2 und 3 sind optional. ", downloadLink("dl_template2", "Vorlage herunterladen")))
    cap <- c(demo = "Demo: Jahresdaten; Die Daten sind fiktiv und nur zur Veranschaulichung eingetragen.",
             demo_q = "Demo: Quartalsdaten; Die Daten sind fiktiv und nur zur Veranschaulichung eingetragen.")
    tagList(div(class = "caption", paste(c(if (input$source %in% names(cap)) cap[[input$source]],
                                           paste0("Periodizit\u00e4t: ", l$tri$labels$origin_pl, ".")), collapse = " ")),
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

  output$prem_msg <- renderUI(tagList(
    if (isTRUE(input$use_prem) && is.null(loaded()$premiums))
      div(class = "alert alert-warning py-2 small",
          "Keine Beitr\u00e4ge vorhanden (Blatt 2 fehlt, ist leer oder passt nicht) \u2013 pr\u00e4mienbasierte Verfahren werden \u00fcbersprungen.")
  ))

  # --- Rechnen ---------------------------------------------------------------
  tail_v <- reactive(if (is.na(input$tail)) 1 else input$tail)
  boot <- reactive({
    n <- as.integer(input$sims)
    if (n <= 0) return(NULL)
    withProgress(message = "Bootstrap l\u00e4uft \u2026",
                 odp_bootstrap(tri(), chain_ladder(tri(), tail_v()), n, if (is.na(input$seed)) 0 else input$seed))
  })
  res <- reactive({
    p <- premiums()
    elr <- if (is.null(p) || isTRUE(input$use_cc) || is.na(input$elr)) NULL else input$elr / 100
    compute_all(tri(), p, elr, tail_v(), n_sims = as.integer(input$sims), seed = input$seed, bootstrap = boot(),
                benchmark = benchmark()$werte)
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
  output$elr_info <- renderUI({
    r <- res()
    tagList(
      if (!is.null(r$elr_used)) div(class = "caption", sprintf("A-priori-Schadenquote: %s \u00b7 Cape-Cod-Quote: %s",
                                                               de_pct(r$elr_used, 2), de_pct(r$cc_elr, 2))),
      lapply(r$notes, function(n) div(class = "alert alert-info py-2 small", n)))
  })
  output$bench_card <- renderUI({
    r <- res()
    if (is.null(r$bench))
      return(div(class = "caption mt-2", "Tipp: Die eigene, manuelle Reserve je Anfallperiode l\u00e4sst sich auf Blatt 3 der Excel-Datei mitliefern. Sie wird dann als Benchmark mit allen Verfahren verglichen."))
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
      write_excel(r, file, list(`A-priori-Schadenquote` = r$elr_used, `Cape-Cod-Quote` = r$cc_elr,
                                `Tail-Faktor` = tail_v(),
                                `Bootstrap-Simulationen` = as.integer(input$sims), Seed = input$seed))
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
    if (!length(keep)) keep <- m[startsWith(m, "Chain Ladder") | startsWith(m, "Bornhuetter") | startsWith(m, "Cape Cod") | m == BENCHMARK]
    selectizeInput("sel", "Verfahren im Diagramm", m, selected = keep, multiple = TRUE, width = "100%")
  })
  output$fig_origin <- renderPlotly({ req(input$sel); fig_lines(res(), intersect(input$sel, names(res()$reserves)), tri()$labels$origin) })
  output$tbl_res <- renderUI(html_table(fmt_frame(res()$res_df, pct = "Entw.-Grad CL", index_name = tri()$labels$origin),
                                        bold_rows = "Summe"))
  output$tbl_ult <- renderUI(html_table(fmt_frame(res()$ult_df, index_name = tri()$labels$origin), bold_rows = "Summe"))

  # --- Chain Ladder & Mack -----------------------------------------------------
  output$tbl_fac <- renderUI(html_table(fmt_frame(res()$fac, dec = setNames(list(4, 4, 2), names(res()$fac)),
                                                  index_name = "\u00dcbergang")))
  output$tbl_ata <- renderUI({
    t <- tri(); C <- t$cum
    ata <- C[, -1, drop = FALSE] / C[, -t$J, drop = FALSE]
    df <- as.data.frame(ata, check.names = FALSE)
    names(df) <- paste0(head(t$devs, -1), "->", t$devs[-1])
    df["CL (gewichtet)", ] <- res()$cl$f
    df["einfacher Mittelwert", ] <- colMeans(ata, na.rm = TRUE)
    html_table(fmt_frame(df, digits = 3, index_name = tri()$labels$origin),
               bold_rows = c("CL (gewichtet)", "einfacher Mittelwert"))
  })
  output$tbl_proj <- renderUI({
    t <- tri()
    html_table(fmt_frame(as.data.frame(res()$cl$full, check.names = FALSE), index_name = tri()$labels$origin),
               shade = !t$obs)
  })

  # --- Bootstrap -------------------------------------------------------------
  output$bs_off <- renderUI(if (as.integer(input$sims) == 0)
    div(class = "alert alert-info mt-3", "Der ODP-Bootstrap ist abgeschaltet (Simulationen = 0)."))
  output$fig_hist <- renderPlotly({ r <- res(); req(r$bs)
    fig_hist(r$bs, if (!is.null(r$benchmark)) sum(r$benchmark)) })
  output$tbl_bs_k <- renderUI({
    bs <- res()$bs; req(bs); tot <- bs$res_total
    k <- data.frame(Gesamtreserve = c(mean(tot), stats::median(tot), stats::quantile(tot, c(0.75, 0.95, 0.995)),
                                      stats::sd(tot), bs$phi),
                    row.names = c("Mittel", "Median", "75 %", "95 %", "99,5 %", "Standardfehler", "Skalenparameter \u03c6"))
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

  # --- Daten -------------------------------------------------------------------
  output$tbl_tri <- renderUI(html_table(fmt_frame(as.data.frame(tri()$cum, check.names = FALSE),
                                                  index_name = tri()$labels$origin)))
}

shinyApp(ui, server)
