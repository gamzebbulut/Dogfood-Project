# =============================================================================
# Table 1 (summary statistics) — R2 revision, version 2026-10-29
# PLOS ONE PONE-D-26-36808 (dog food AGE paper)
#
# WHY THIS SCRIPT EXISTS
#   The R1 Table 1 was assembled by hand from several R outputs
#   (table1_summary/submitted_R1/: "Merged_Summary_Table_2025-03-26.docx",
#   "combined table manual formatting.docx") into the manuscript table layout. Hand copying
#   let in errors (e.g. dry MG-H1 maximum from excluded food 98; median typo
#   "9,0874.96"). This script recomputes EVERY cell of Table 1 straight from the
#   data file, in the manuscript's row/column order, so the table can be checked
#   or re-typed cell by cell.
#
# WHAT IT DOES
#  1. Loads the same data file and the same analysis set as Figs 1-2, 7 and
#     Table 3: wet (Canned) and dry (Kibble) foods with all four AGE measures
#     -> n = 56 wet / 44 dry (IDs 98, 117, 120 have no CML and drop out).
#  2. Builds the 7 Table 1 measures on each of the 3 reporting bases:
#       CML, MG-H1, CML + MG-H1, hydrophilic fluorescence (SF),
#       hydrophobic fluorescence (LF), combined fluorescence (SF + LF),
#       Composite AGE Index (CAI = CML + MG-H1 + SF + LF)
#     Bases: dry matter (µg/g), as-fed (µg/g), caloric basis (µg/kcal ME).
#  3. For each measure x food format x basis: Mean, SEM (= SD / sqrt(n),
#     sample SD), Median, Min, Max.
#  4. Writes two CSVs to output/:
#       Table1_R2_<date>_values.csv     unrounded numbers (long format)
#       Table1_R2_<date>_formatted.csv  manuscript layout, 2 decimals,
#                                       thousands commas, "Min–Max" ranges
# =============================================================================

# ---- TUNING BLOCK ----
data_file <- "/Users/gbbulut/Desktop/making figures attempt 15/dogfood-AGE-figures/data/Final excel sheet for manuscript_02-20-25.xlsx"
out_dir   <- "/Users/gbbulut/Desktop/making figures attempt 15/dogfood-AGE-figures/table1_summary/output"
version   <- "R2_2026-10-29"
# ----------------------

library(readxl)
library(dplyr)
library(tidyr)

dir.create(out_dir, showWarnings = FALSE)

# ---- 1. Data: same analysis set as the other R2 figures/tables ----
# Columns per basis, in the order CML, MG-H1, hydrophilic (SF), hydrophobic (LF)
bases <- list(
  "Dry matter (µg/g)"          = c("CML_ug_per_g_food",          "MG_ug_per_g_food",
                                   "SF_ug_per_g_food",           "LF_AGE_ug_per_g_food"),
  "As-fed (µg/g)"              = c("CML_ug_per_g_food_moist",    "MG_ug_per_g_food_moist",
                                   "SF_ug_per_g_food_moist",     "LF_AGE_ug_per_g_food_moist"),
  "Caloric basis (µg/kcal ME)" = c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist",
                                   "SF_ug_per_kcal_food_moist",  "LF_ug_per_kcal_food_moist")
)
all_cols <- unlist(bases, use.names = FALSE)

# CML_ug_per_g_food stores the excluded foods (IDs 98, 117, 120) as the text "NA",
# so read "NA" as missing; those 3 foods then drop out below.
data <- read_excel(data_file, na = c("", "NA")) %>%
  filter(Type %in% c("Canned", "Kibble")) %>%
  mutate(across(all_of(all_cols), as.numeric)) %>%
  drop_na(all_of(all_cols)) %>%                       # foods with all four AGE measures
  mutate(Format = recode(Type, "Canned" = "Wet", "Kibble" = "Dry (kibble)"))

print(table(data$Format))                             # expect Dry (kibble) 44, Wet 56
stopifnot(sum(data$Format == "Wet") == 56, sum(data$Format == "Dry (kibble)") == 44)

# ---- 2-3. Seven measures x 3 bases x 2 formats ----
measure_names <- c("CML", "MG-H1", "CML + MG-H1",
                   "Hydrophilic fluorescence", "Hydrophobic fluorescence",
                   "Combined fluorescence", "Composite AGE Index (CAI)")

summarise_values <- function(x) {
  data.frame(n = length(x), Mean = mean(x), SEM = sd(x) / sqrt(length(x)),
             Median = median(x), Min = min(x), Max = max(x))
}

long <- bind_rows(lapply(c("Wet", "Dry (kibble)"), function(fmt) {
  d <- filter(data, Format == fmt)
  bind_rows(lapply(names(bases), function(b) {
    cols <- bases[[b]]
    cml <- d[[cols[1]]]; mg <- d[[cols[2]]]; sf <- d[[cols[3]]]; lf <- d[[cols[4]]]
    vals <- list(cml, mg, cml + mg, sf, lf, sf + lf, cml + mg + sf + lf)
    bind_rows(lapply(seq_along(vals), function(i) {
      cbind(data.frame(Measure = measure_names[i], Format = fmt, Basis = b),
            summarise_values(vals[[i]]))
    }))
  }))
}))

write.csv(long, file.path(out_dir, paste0("Table1_", version, "_values.csv")),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---- 4. Manuscript layout: one row per measure x format, 4 columns per basis ----
f2 <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

formatted <- bind_rows(lapply(measure_names, function(m) {
  bind_rows(lapply(c("Wet", "Dry (kibble)"), function(fmt) {
    row <- data.frame(Measure = m, Format = fmt, check.names = FALSE)
    for (b in names(bases)) {                    # Dry matter, As-fed, Caloric basis
      s <- filter(long, Measure == m, Format == fmt, Basis == b)
      row[[paste(b, "Mean")]]    <- f2(s$Mean)
      row[[paste(b, "SEM")]]     <- f2(s$SEM)
      row[[paste(b, "Median")]]  <- f2(s$Median)
      row[[paste(b, "Min–Max")]] <- paste0(f2(s$Min), "–", f2(s$Max))
    }
    row
  }))
}))

write.csv(formatted, file.path(out_dir, paste0("Table1_", version, "_formatted.csv")),
          row.names = FALSE, fileEncoding = "UTF-8")

print(as.data.frame(formatted))
