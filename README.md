# Loss Reserving – R-Version (neue Basis mit Erweiterungen)

Grundlage ist die neue Basisversion (ELR, Tail als eigene Spalte, Anfalljahre 2016–2025, wählbare Bootstrap-Simulationen,
Seite „Daten“ vorn). Darauf aufgesetzt sind alle Erweiterungen der bisherigen vollen Version.

| Datei | Inhalt |
|---|---|
| `loss_reserving.R` | Rechenlogik – auch als Kommandozeilen-Skript nutzbar |
| `app.R` | R-Shiny-App |
| `install_packages.R` | installiert die benötigten Pakete (shiny, bslib, plotly, readxl, openxlsx) |
| `beispiel_jahre.xlsx` | Beispiel und Vorlage mit fünf Blättern (2016–2025, fiktive Beiträge, manuelle Reserve, manuelle AvE/CDR) |
| `beispiel_paid_incurred.xlsx` | bezahlte (Blatt 1) und gemeldete Schäden (Blatt 4) für Munich Chain Ladder |
| `beispiel_quartale.xlsx` | fiktives Quartalsdreieck |
| `Loss_Reserving_Benutzerhandbuch_R.docx` / `.pdf` | ausführliches Handbuch zu dieser Version |

## Starten

    source("install_packages.R")     # einmalig
    shiny::runApp("pfad/zum/ordner")

    Rscript loss_reserving.R --excel beispiel_jahre.xlsx --elr 0.5
    Rscript loss_reserving.R --rueckblick 5 --faktoren-fest

## Aus der Basis unverändert übernommen

- Verfahren: Chain Ladder, Mack, **ELR** (Vorjahre aus Chain Ladder, jüngstes Anfalljahr Prämie × Quote), Bornhuetter-Ferguson,
  Cape Cod, Additiv, ODP-Bootstrap. Die Ergebnisse stimmen mit der Basis exakt überein.
- Tail-Faktor als eigene Spalte „Tail“ in der Chain-Ladder-Projektion.
- Seitenleiste: Quelle „Demo“ / Upload, Perioden „automatisch erkennen“, A-priori-Schadenquote 50 %, Bootstrap-Simulationen 0 / 1.000 / 10.000.
- Reihenfolge der Seiten (Daten zuerst), Texte der Methodik-Seite, Bootstrap-Kennzahlen ohne φ, Excel-Download weiterhin auskommentiert.

## Erweiterungen

| Seite | Inhalt |
|---|---|
| Daten | Plausibilitätsprüfungen; Anzeige von Blatt 4 (gemeldete Schäden) und Blatt 5 (manuelle AvE/CDR) |
| Übersicht | Zusammenfassung der Plausibilitätsprüfung |
| Chain Ladder & Mack | Faktorauswahl: Einzelfaktoren per Klick ausschließen, nur die letzten n Diagonalen |
| Munich Chain Ladder | bezahlt und gemeldet gemeinsam (braucht Blatt 4) |
| Bootstrap | Verteilung je Anfallperiode mit Auswahlfeld, Residuen-Diagnose |
| Ein-Jahres-Sicht | Merz-Wüthrich (Standardfehler des CDR) und Ein-Jahres-Bootstrap (Re-Reserving) |
| Sensitivität | Tornado-Diagramm je Verfahren (auch ELR) |
| Actual vs Expected | Backtest der jüngsten Diagonale oder Vergleich mit einer Vorjahresdatei |
| Rückblick | Modell vor n Jahren, rollierend AvE und CDR, Vergleich mit manuellen Werten aus Blatt 5 |

## Excel-Datei (Reihenfolge der Blätter entscheidet)

| Blatt | Inhalt | Pflicht |
|---|---|---|
| 1 – Schäden | Dreieck: erste Spalte Anfalljahr, weitere Spalten Abwicklungsperioden | ja |
| 2 – Beiträge | Anfalljahr, Prämie | nein |
| 3 – Manuelle Reserve | Anfalljahr, eigene Reserve | nein |
| 4 – Gemeldete Schäden | Dreieck wie Blatt 1 (Blatt 1 dann = bezahlt) | nein (Munich Chain Ladder) |
| 5 – Manuelle AvE und CDR | Kalenderjahr, erwartete Zahlung, CDR (Summe über alle Anfalljahre) | nein (Rückblick; Blatt 4 darf dann leer sein) |

## Hinweise

- Ein-Jahres-Sicht und Rückblick setzen ein Jahresdreieck voraus. Bei Quartalsdaten zeigen beide Seiten einen Hinweis;
  einen Schalter zum Verdichten gibt es in dieser Basis nicht (Kommandozeile: `--to-years`).
- Mit „Bootstrap-Simulationen = 0“ entfallen die Bootstrap-Seite und die simulierte Verteilung der Ein-Jahres-Sicht;
  Merz-Wüthrich wird trotzdem gerechnet.
- Der Excel-Export (`dl_xlsx`) ist im Server vollständig vorhanden, inklusive der Blätter der Erweiterungen. Zum Einschalten
  die auskommentierte Zeile `downloadButton("dl_xlsx", ...)` auf der Seite „Übersicht“ wieder aktivieren.
