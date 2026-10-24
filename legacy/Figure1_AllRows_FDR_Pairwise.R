
library(dplyr)
library(rstatix)
library(readxl)
library(tidyr)

# Load data
data <- read_excel("Final excel sheet for manuscript_02-20-25.xlsx")

# Preprocessing
data <- data %>%
  mutate(CML_plus_MG_g = CML_ug_per_g_food + MG_ug_per_g_food,
         CML_plus_MG_g_moist = CML_ug_per_g_food_moist + MG_ug_per_g_food_moist,
         CML_plus_MG_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist)

# Define group order
group_levels <- c("CML - Canned", "CML - Kibble",
                  "MG - Canned", "MG - Kibble",
                  "CML+MG - Canned", "CML+MG - Kibble")

# Helper function
prep_long <- function(df, cml, mg, cmlmg, label) {
  df %>%
    select(Type, !!sym(cml), !!sym(mg), !!sym(cmlmg)) %>%
    pivot_longer(cols = everything(), names_to = "Compound", values_to = "Value") %>%
    mutate(
      Compound = recode(Compound,
                        !!cml := "CML",
                        !!mg := "MG",
                        !!cmlmg := "CML+MG"),
      Group = factor(paste(Compound, "-", Type), levels = group_levels),
      Measurement = label
    )
}

# Prepare all data frames
dry_data <- prep_long(data, "CML_ug_per_g_food", "MG_ug_per_g_food", "CML_plus_MG_g", "Dry Matter (µg/g)")
moist_data <- prep_long(data, "CML_ug_per_g_food_moist", "MG_ug_per_g_food_moist", "CML_plus_MG_g_moist", "As Fed (µg/g)")
kcal_data <- prep_long(data, "CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist", "CML_plus_MG_kcal_moist", "As Fed (µg/kcal)")

# Combine all into one
all_data <- bind_rows(dry_data, moist_data, kcal_data)

# Pairwise tests
all_pvals <- all_data %>%
  group_by(Measurement) %>%
  wilcox_test(Value ~ Group, p.adjust.method = "none") %>%
  ungroup() %>%
  mutate(
    p.adj = p.adjust(p, method = "fdr"),
    Significant = ifelse(p.adj < 0.05, "Yes", "No")
  )

# Save results
write.csv(all_pvals, "Figure1_AllRows_FDR_Pvalues.csv", row.names = FALSE)
