# Installs the R packages listed in requirements.txt. Nothing else.
#
# Usage:  Rscript setup.R

cran <- "https://cloud.r-project.org"

lines    <- trimws(readLines("requirements.txt", warn = FALSE))
required <- lines[nzchar(lines) & !startsWith(lines, "#")]

missing <- setdiff(required, rownames(installed.packages()))

if (length(missing) > 0) {
  cat("Installing:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = cran)
} else {
  cat("Already installed:", paste(required, collapse = ", "), "\n")
}
