# Run the analysis and capture the complete output.
# The console shows everything live, output.txt keeps a copy for review.
#
# Usage:  Rscript run.R
# In RStudio, source b105_analysis.R directly instead.

con <- file("output.txt", open = "wt")
sink(con, split = TRUE)
sink(con, type = "message")

source("b105_analysis.R", echo = FALSE)

sink(type = "message")
sink()
close(con)

cat("\nOutput saved to output.txt (",
    length(readLines("output.txt")), "lines )\n")
