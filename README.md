# Dog food AGE paper — figure and table code

Code for the figures and tables of:

> Turner DP, Ding B, Bulut G, He J, *et al.* **Assessment of Advanced Glycation End Products in Commercially Available Dog Foods Using a Multi-Metric Analytical Approach.** PLOS ONE (under revision, PONE-D-26-36808).

Figure code: Gamze Bulut (Figs 1–4, 7, Tables). t-SNE / k-means code for Figs 5–6: Jian He (to be added).

## Repository layout

One folder per figure; each `code/` folder holds the script that makes the revised (R2) figure.

| Manuscript | Folder | Script | What it makes |
|---|---|---|---|
| Figure 1 — CML, MG-H1, CML + MG-H1 | `01_fig1_CML_MGH1/code/` | `Fig1_R2_2026-10-24.R` | Violin/box plots (rows A–C: dry matter, as-fed, caloric basis); Wilcoxon wet vs dry, BH over the 9 comparisons shown |
| Figure 2 — AGE-associated fluorescence | `02_fig2_fluorescence/code/` | `Fig2_R2_2026-10-24.R` | Same layout; BH over the 6 wet-vs-dry comparisons; paired signed-rank test for hydrophilic vs hydrophobic within dry foods |
| Figure 3 — correlations among AGE measures | `03_fig3_AGE_correlations/code/` | `Fig3_R2_2026-10-24_v2.R` | Spearman matrices, dry (A) and wet (B), BH-adjusted stars |
| Figure 4 — macronutrients × AGE measures | `04_fig4_nutrient_correlations/code/` | `Fig4_R2_2026-10-24_v3.R` | Spearman matrix, wet foods (dry foods checked; none significant) |
| Figures 5–6 — t-SNE / k-means | `05_fig5_tSNE_AGE/`, `06_fig6_tSNE_nutrients/` | to be added | — |
| Figure 7 — Composite AGE Index | `07_fig7_CAI/code/` | `Fig7_R2_2026-10-24_v7.R` | Stacked CAI bars + component panels (caloric basis) |
| Table 3 — coefficient of variation | `table3_CV/code/` | `Table3_R2_2026-10-24_v2.R`, then `make_Table3_docx_R2_2026-10-24_v2.py` | CV table (CSV), then the Word table |

Each script starts with a change log (every edit from the R1 version is tagged `# R2 CHANGE n`) and a tuning block with the input file and output folder.

Final figure layout (panel letters, column headers, legend placement) was assembled in Adobe Illustrator from the R exports; all plotted values, statistics and stars come from the scripts.

## Data

The scripts read the study data file (`Final excel sheet for manuscript_02-20-25.xlsx`, sheets "Combined data" and "macro normalization"). The data are not included in this repository; they are available from the corresponding author. The analysis set is the 100 foods with all four AGE measures (56 wet, 44 dry).

To run a script, set `data_file` and `out_dir` in its tuning block.

## Software

R 4.4 with readxl, dplyr, tidyr, ggplot2, rstatix, corrplot, patchwork. Python 3 with python-docx (Table 3 Word file only).

## History

- Tag **`R1-submission`** — the code as it was when the R1 manuscript was submitted.
- `legacy/` — those R1-era scripts, kept for reference (file names do not always match manuscript figure numbers: e.g. `Fig 4 making among ages corr matrix and tables.R` made Figure 3 and `figure_6_combined_AGE_plots_final*.R` made Figure 7).
- R2 changes (Oct 2026): reviewer-requested relabelling (wet/dry; dry matter, as-fed, caloric basis µg/kcal ME; Composite AGE Index), correlation coefficients shown in every cell, multiple-testing families limited to the comparisons shown, and Figure 2 restricted to the same 56 + 44 foods as the other figures.
- Public-release edits: co-author names in code comments replaced by roles; the internal note in the Table 3 Word output disabled.
