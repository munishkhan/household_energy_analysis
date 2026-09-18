# What drives a household's annual energy bill?

B105 Applied Statistical Modelling.
Analysis of the US Residential Energy Consumption Survey (RECS) 2020 in base R.

## Business Problem

What is the main reason for a household's high annual energy bill?

Four hypotheses are tested against the total annual energy cost of surveyed
households in the US.

| | Method | Question |
|---|---|---|
| H1 | One-way ANOVA | Does cost differ by census region? |
| H2 | Welch two-sample t-test | Do homes built before 2000 cost more than newer ones? |
| H3 | Simple linear regression | Does a larger floor area mean a higher bill? |
| H4 | Multiple linear regression | Does climate add anything once floor area is known? |

**Outcome:** floor area is the biggest driver of a household's energy cost. It
explains 24.4% of the variation, against 4.3% for region and a further 0.6% for
climate. All four hypotheses reject their null — with 18,495 households almost
anything does — so the finding is the ranking by effect size, not the p-values.

---

# Reproducing this analysis

## 1. Prerequisites

| | |
|---|---|
| R | **4.0 or newer** (developed on 4.6.1) |
| RStudio | optional, needed only to see the figures |
| Network | not required — the data ships with the repository |

Get R from https://cran.r-project.org/. Check your version with `R --version`.

## 2. Clone and enter

```bash
git clone https://github.com/munishkhan/householed_energy_analysis.git
cd householed_energy_analysis
```

## 3. Install dependencies

```bash
Rscript setup.R
```

Installs the packages listed in `requirements.txt` — `psych` for `describe()` and
`car` for `vif()`. Safe to re-run; anything already installed is left alone.

## 4. Run the analysis

```bash
Rscript run.R
```

Runs all seven sections, prints to the console and writes the same text to
`output.txt`. Takes about 30 seconds.

---

## Files

| File | Purpose |
|---|---|
| `b105_analysis.R` | The analysis, sections 1–7 |
| `setup.R` | Installs the packages in `requirements.txt` |
| `run.R` | Runs the analysis, copies the console to `output.txt` |
| `requirements.txt` | R package list |
| `data/` | Pinned microdata and codebook |

## Data

| | |
|---|---|
| Source | U.S. Energy Information Administration (EIA) |
| Survey | 2020 Residential Energy Consumption Survey (RECS) |
| File | Public Use Microdata, **version 7**, released January 2024 |
| Shape | 18,496 households × 799 variables |
| Licence | U.S. Government work, public domain — redistribution permitted |

- Landing page: https://www.eia.gov/consumption/residential/data/2020/index.php?view=microdata
- CSV (54 MB): https://www.eia.gov/consumption/residential/data/2020/csv/recs2020_public_v7.csv
- Codebook: https://www.eia.gov/consumption/residential/data/2020/xls/RECS%202020%20Codebook%20for%20Public%20File%20-%20v7.xlsx

---

## References

U.S. Energy Information Administration (2024) *2020 Residential Energy
Consumption Survey (RECS): Public Use Microdata, Version 7*. Washington, DC:
U.S. Energy Information Administration. Available at:
https://www.eia.gov/consumption/residential/data/2020/index.php?view=microdata
(Accessed: 17 September 2026).

R Core Team (2026) *R: A Language and Environment for Statistical Computing*.
Vienna: R Foundation for Statistical Computing. doi:10.32614/R.manuals. Available
at: https://www.R-project.org/ (Accessed: 17 September 2026).

Revelle, W. (2026) *psych: Procedures for Psychological, Psychometric, and
Personality Research*. R package version 2.6.5. Evanston, IL: Northwestern
University. Available at: https://CRAN.R-project.org/package=psych
(Accessed: 17 September 2026).

Fox, J. and Weisberg, S. (2019) *An R Companion to Applied Regression*. 3rd edn.
Thousand Oaks, CA: Sage. Available at: https://www.john-fox.ca/Companion/
(Accessed: 17 September 2026).
