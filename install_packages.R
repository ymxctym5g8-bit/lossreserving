# Einmalig ausfuehren: installiert die benoetigten R-Pakete
pakete <- c("shiny", "bslib", "plotly", "readxl", "openxlsx")
fehlend <- pakete[!vapply(pakete, requireNamespace, logical(1), quietly = TRUE)]
if (length(fehlend)) install.packages(fehlend, repos = "https://cloud.r-project.org")
cat("Fertig. Installiert:", paste(pakete, collapse = ", "), "\n")
