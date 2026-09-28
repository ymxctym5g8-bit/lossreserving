# Loss Reserving – R-Version

| Datei | Inhalt |
|---|---|
| `loss_reserving.R` | Rechenlogik (Chain Ladder, Mack, ELR, Bornhuetter-Ferguson, Cape Cod, Additiv, ODP-Bootstrap) – auch als Kommandozeilen-Skript nutzbar |
| `app.R` | R-Shiny-App |
| `install_packages.R` | installiert die benötigten Pakete |
| `beispiel_jahre.xlsx` | Beispiel und Vorlage: RAA-Dreieck, fiktive Beiträge, fiktive manuelle Reserve |
| `beispiel_quartale.xlsx` | fiktives Quartalsdreieck 2021Q1–2024Q4 mit Quartalsbeiträgen |
| `Loss_Reserving_Benutzerhandbuch_R.docx` | ausführliches Handbuch |

## Installation (einmalig, in R)

    source("install_packages.R")

Benötigt R ≥ 4.1 und die Pakete shiny, bslib, plotly, readxl, openxlsx.

## Starten

**Shiny-App** – in RStudio `app.R` öffnen und auf „Run App“ klicken, oder in R:

    shiny::runApp("pfad/zum/ordner")

**Kommandozeile:**

    Rscript loss_reserving.R                                   # Demo mit RAA-Dreieck
    Rscript loss_reserving.R --excel daten.xlsx --elr 0.65
    Rscript loss_reserving.R --excel quartale.xlsx --to-years

**Als Bibliothek in eigenen Skripten:**

    source("loss_reserving.R")
    d <- read_excel_input("daten.xlsx")
    r <- compute_all(d$tri, premiums = d$premiums, elr = 0.65, benchmark = d$benchmark)
    r$comp        # Vergleich aller Verfahren
    r$res_df      # Reserven je Anfallperiode
    r$bench       # Einordnung der manuellen Reserve (falls vorhanden)
    write_excel(r, "ergebnisse.xlsx")

## Daten: eine Excel-Datei mit drei Blättern

| Blatt | Inhalt | Pflicht |
|---|---|---|
| 1 – Schäden | Dreieck: erste Spalte Anfallperiode, weitere Spalten Abwicklungsperioden (1, 2, 3, …), zukünftige Zellen leer | ja |
| 2 – Beiträge | Anfallperiode, Prämie | nein (sonst keine prämienbasierten Verfahren) |
| 3 – Manuelle Reserve | Anfallperiode, eigene Reserve | nein (sonst kein Benchmark) |

- Maßgeblich ist die **Reihenfolge** der Blätter, nicht ihr Name. Erste Zeile jedes Blatts = Überschriften.
- Blatt 2 und 3 werden über die Beschriftung der Anfallperiode zugeordnet, sonst über die Reihenfolge.
- In der App: Quelle **„Excel-Datei hochladen“**; darunter der Link **„Vorlage herunterladen“** (= `beispiel_jahre.xlsx`).
- Die Seite **„Daten“** zeigt Dreieck, Beiträge und manuelle Reserve nur zur Kontrolle – geändert wird in der Excel-Datei.
- Kumuliert oder inkrementell wählbar (Seitenleiste „Werte im Dreieck“, Kommandozeile `--incremental`).

## Jahres- und Quartalsdaten

- **Erkennung:** Anfallperioden wie `2024Q1`, `2024-Q1`, `Q1 2024`, `2024/1` oder `1. Quartal 2024` werden
  automatisch als Quartale erkannt (Seitenleiste **Periodizität**; lässt sich auch fest auf Jahre oder Quartale stellen).
  Die Spalten sind dann Abwicklungsquartale (1 = Anfallquartal selbst, 2 = folgendes Quartal usw.).
- **Direkt als Quartale:** Alle Verfahren laufen auf dem Quartalsdreieck; Beiträge und manuelle Reserve stehen je Quartal auf Blatt 2 und 3.
- **Zu Jahren verdichten:** Häkchen **„Quartale zu Jahren verdichten“** (Kommandozeile `--to-years`). Der Wert eines
  Anfalljahres im Abwicklungsjahr k ist die Summe seiner vier Anfallquartale, jeweils zum Stand 31.12. (Anfalljahr + k − 1).
  Endet der Datenstand unterjährig, verwendet das Tool den Stand zum letzten Jahresende und weist darauf hin.
  Beiträge und manuelle Reserve je Quartal werden automatisch zu Jahren aufsummiert.
- **Beispiel:** `beispiel_quartale.xlsx`; in der App als Quelle **„Demo Quartale (fiktiv)“**.

## Eigene (manuelle) Reserve als Benchmark

Steht auf Blatt 3 eine Reserve je Anfallperiode (0 ist erlaubt), zeigt die App zusätzlich:
- eine Kachel **„Manuelle Reserve“** mit Abweichung zu Chain Ladder und Sicherheitsniveau,
- die Zeile **„Manuelle Reserve“** in allen Tabellen und Diagrammen sowie die Spalte **„Abw. zu Benchmark“**,
- eine gepunktete Linie in den Balkendiagrammen und im Bootstrap-Histogramm,
- die Karte **„Einordnung der manuellen Reserve“** (Übersicht) mit Kurzaussagen und einer Tabelle je Anfallperiode,
- im Excel-Export das Blatt **„Benchmark“**.

**Sicherheitsniveau:** Anteil der Szenarien, den die manuelle Reserve abdeckt – nach Mack über eine
Lognormal-Näherung, nach Bootstrap aus der simulierten Verteilung. 50 % entspricht dem Median; höhere
Werte bedeuten eine vorsichtigere Reservierung.

## Hinweis

Der ODP-Bootstrap ist zufallsbasiert. Mit gleichem Zufallsstartwert und gleicher Anzahl Simulationen
sind die Ergebnisse reproduzierbar. Ausführliche Anleitung: `Loss_Reserving_Benutzerhandbuch_R.docx`.
