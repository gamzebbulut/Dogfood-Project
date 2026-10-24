# =============================================================================
# Figure 7 — R2 revision (PLOS ONE PONE-D-26-36808), version 2026-10-24 v7 (final)
# v2: food labels only under panels A and E (as in the R1 Illustrator layout), larger
# v3: font sizes raised to R1 Illustrator sizes (titles, legend, letters) + larger ticks/axis titles
# v4: legend moved outside panel A on the right (no bars hidden); tags "A)"-"E)";
#     y-axis title on two lines; extra space above each panel
# v5: legend above panel A bars in 2 rows (outside-right squeezed all panels)
# v6: pieces for Illustrator: full figure WITHOUT legend (PDF + TIFF), each panel as PDF,
#     legend alone as PDF (one column, R1 size). Gamze places the legend by hand.
# v7: only the outputs used for the final figure are kept: Figure7_CAI_noLegend (PDF + TIFF)
#     and Figure7_legend_only (PDF), assembled in Illustrator. Per-panel PDFs removed.
# Based on R1 script: figure_6_combined_AGE_plots_final_Aug6_2025_local.R
#   (file name says figure 6 but it makes manuscript Fig 7; plot part only.
#    The CV block in that script makes Table 3 and is not changed here.)
#
# CHANGE LOG (every edit is tagged "# R2 CHANGE n" below)
#  1. Standalone: loads data with the same type casting and NA filtering as
#     R1 "pre pocessing Jan 29 25.R" (R1 needed it run first). n = 56 wet / 44 dry.
#  2. "Total AGE Score" -> Composite AGE Index (CAI)              (Reviewer 3 / corresponding author, Fig 7)
#  3. Panel titles: CML, MG-H1, Hydrophilic AGE-associated fluorescence,
#     Hydrophobic AGE-associated fluorescence; same names in the legend
#  4. Y-axis on every panel: Caloric basis (µg/kcal ME)          (Reviewer 3, Lines 350-352)
#  5. X labels "Wet <ID>" / "Dry <ID>" coloured by food type (R1: ID only in R,
#     Canned/Kibble names and colours added in Illustrator); x-axis title
#     "Dog food sample"
#  6. Panel letters A-E (patchwork tags)                         (corresponding author, Fig 7)
#  7. Saved to TIFF + PNG from code
#  Plot order, colours, bar values and food order are unchanged from R1.
# =============================================================================

# ---- TUNING BLOCK ----------------------------------------------------- R2 CHANGE 1
data_file <- "/Users/gbbulut/Desktop/making figures attempt 14/dogfood-AGE-figures/data/Final excel sheet for manuscript_02-20-25.xlsx"
out_dir   <- "/Users/gbbulut/Desktop/making figures attempt 14/dogfood-AGE-figures/07_fig7_CAI/output"
version   <- "R2_2026-10-24"
wet_label_colour <- "#D55E00"   # R1 figure: canned labels orange-red
dry_label_colour <- "black"
fig_w_in <- 16; fig_h_in <- 20  # same as R1 ggsave
# ---------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)  # For combining plots

# ====== R2 CHANGE 1: data, exactly as R1 preprocessing ======
data_filtered_13 <- read_excel(data_file) %>%
  mutate(label = as.character(ID),
         Type.f = as.factor(Type),
         CML_ug_per_g_food = as.numeric(CML_ug_per_g_food),
         `kcal/kg` = as.numeric(`kcal/kg`)) %>%
  rename(kcal_per_kg = `kcal/kg`) %>%
  mutate(Science.f = as.factor(Science),
         CML_plus_MG_kcal_moist = CML_ug_per_kcal_food_moist + MG_ug_per_kcal_food_moist) %>%
  drop_na(any_of(c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist",
                   "CML_plus_MG_kcal_moist", "SF_ug_per_kcal_food_moist"))) %>%
  filter(Type %in% c("Canned", "Kibble"))
print(table(data_filtered_13$Type))   # expect Canned 56, Kibble 44

# ====== Compute Total AGE Score per kcal ======
data <- data_filtered_13 %>%
  mutate(
    FoodType = recode(Type, "Canned" = "Wet", "Kibble" = "Dry"),        # R2 CHANGE 5
    DogFoodLabel = paste(FoodType, ID),                                  # R2 CHANGE 5 (R1: ID only)
    Total_AGE_Score_kcal = CML_ug_per_kcal_food_moist +
      MG_ug_per_kcal_food_moist +
      LF_ug_per_kcal_food_moist +
      SF_ug_per_kcal_food_moist
  ) %>%
  arrange(Total_AGE_Score_kcal) %>%
  mutate(DogFoodLabel = factor(DogFoodLabel, levels = DogFoodLabel))

# R2 CHANGE 5: label colours in plotting order
label_cols <- ifelse(data$FoodType == "Wet", wet_label_colour, dry_label_colour)

# ====== Define Custom Colors ======
age_colors <- c(
  "CML_ug_per_kcal_food_moist" = "red",
  "MG_ug_per_kcal_food_moist" = "gray",
  "LF_ug_per_kcal_food_moist" = "darkgreen",
  "SF_ug_per_kcal_food_moist" = "blue"
)
# R2 CHANGE 3: legend names
age_names <- c(
  "CML_ug_per_kcal_food_moist" = "CML",
  "MG_ug_per_kcal_food_moist" = "MG-H1",
  "LF_ug_per_kcal_food_moist" = "Hydrophobic AGE-associated fluorescence",
  "SF_ug_per_kcal_food_moist" = "Hydrophilic AGE-associated fluorescence"
)
y_title <- "Caloric basis\n(µg/kcal ME)"   # R2 CHANGE 4; v4: two lines

# R2 shared theme (R1 used theme_minimal() + 90-degree x text)
fig7_theme <- theme_minimal(base_size = 18) +                  # v3: 14 -> 18
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10,   # v2: 7 -> 9; v3: 10
                                   colour = label_cols),      # R2 CHANGE 5 (vectorised colour)
        axis.text.y = element_text(size = 16),                # v3
        axis.title.y = element_text(size = 18),               # v3
        plot.title = element_text(size = 30),                 # v3: 16 bold -> 30 plain (R1 size)
        plot.margin = margin(t = 20, r = 5, b = 5, l = 5),    # v4: space above each panel
        panel.grid.minor = element_blank())

# ====== Step 1: Stacked Bar Plot in Total AGE Score Order ======
data_long <- data %>%
  select(DogFoodLabel,
         CML_ug_per_kcal_food_moist,
         MG_ug_per_kcal_food_moist,
         LF_ug_per_kcal_food_moist,
         SF_ug_per_kcal_food_moist) %>%
  pivot_longer(cols = -DogFoodLabel, names_to = "AGE_Measure", values_to = "Value")

stacked_plot <- ggplot(data_long, aes(x = DogFoodLabel, y = Value, fill = AGE_Measure)) +
  geom_bar(stat = "identity") +
  labs(title = "Composite AGE Index (CAI)", x = NULL, y = y_title, fill = "AGE measure") +   # R2 CHANGE 2, 4
  fig7_theme +
  theme(legend.position = "top", legend.justification = "left",          # v5: above the bars, full width kept
        legend.background = element_rect(fill = "white", colour = NA),
        legend.title = element_text(size = 24), legend.text = element_text(size = 22),   # v3
        legend.key.size = unit(1.2, "cm")) +
  scale_fill_manual(values = age_colors, labels = age_names,                               # R2 CHANGE 3
                    breaks = c("CML_ug_per_kcal_food_moist", "MG_ug_per_kcal_food_moist",
                               "SF_ug_per_kcal_food_moist", "LF_ug_per_kcal_food_moist")) +
  guides(fill = guide_legend(ncol = 1))                                                    # v6: one column (legend file)

# ====== Step 2: Individual AGE Plots in Same Order ======
plot_single_age <- function(var, color, label, show_x_title = FALSE) {
  p <- ggplot(data, aes(x = DogFoodLabel, y = .data[[var]])) +
    geom_bar(stat = "identity", fill = color) +
    labs(title = label, x = if (show_x_title) "Dog food sample" else NULL, y = y_title) +   # R2 CHANGE 3, 4, 5
    fig7_theme
  if (!show_x_title) p <- p + theme(axis.text.x = element_blank())   # v2: labels only on A and E
  p
}

cml_plot <- plot_single_age("CML_ug_per_kcal_food_moist", age_colors["CML_ug_per_kcal_food_moist"], "CML")
mg_plot  <- plot_single_age("MG_ug_per_kcal_food_moist", age_colors["MG_ug_per_kcal_food_moist"], "MG-H1")
sf_plot  <- plot_single_age("SF_ug_per_kcal_food_moist", age_colors["SF_ug_per_kcal_food_moist"],
                            "Hydrophilic AGE-associated fluorescence")
lf_plot  <- plot_single_age("LF_ug_per_kcal_food_moist", age_colors["LF_ug_per_kcal_food_moist"],
                            "Hydrophobic AGE-associated fluorescence", show_x_title = TRUE)

# ====== Step 3: Combine All Plots Vertically ======
# v6: legend removed from the assembled figure; saved separately below
combined_plot <- (stacked_plot + theme(legend.position = "none")) / cml_plot / mg_plot / sf_plot / lf_plot +
  plot_layout(heights = c(2, 1, 1, 1, 1)) +
  plot_annotation(tag_levels = "A", tag_suffix = ")") &                  # R2 CHANGE 6; v4: "A)" as in R1
  theme(plot.tag = element_text(size = 32))   # v3: R1 letter size

# ====== Save ====== R2 CHANGE 7; v6: PDF (vector, editable in Illustrator) + TIFF
ggsave(file.path(out_dir, paste0("Figure7_CAI_noLegend_", version, ".pdf")), combined_plot,
       width = fig_w_in, height = fig_h_in, bg = "white")
ggsave(file.path(out_dir, paste0("Figure7_CAI_noLegend_", version, ".tiff")), combined_plot,
       width = fig_w_in, height = fig_h_in, dpi = 600, bg = "white")

# v6: legend alone (crop in Illustrator)
legend_plot <- stacked_plot + theme(legend.position = "right")
ggsave(file.path(out_dir, paste0("Figure7_legend_only_", version, ".pdf")), legend_plot,
       width = fig_w_in + 8, height = 7, bg = "white")

# Food order table for checking against the R1 figure
write.csv(data %>% select(ID, Type, FoodType, DogFoodLabel, Total_AGE_Score_kcal,
                          CML_ug_per_kcal_food_moist, MG_ug_per_kcal_food_moist,
                          SF_ug_per_kcal_food_moist, LF_ug_per_kcal_food_moist),
          file.path(out_dir, paste0("Figure7_CAI_values_", version, ".csv")), row.names = FALSE)
