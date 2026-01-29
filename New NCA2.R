############################################################
# NCA outputs (clean): uses Revenue Upper directly
############################################################

# =========================
# 0) PACKAGES
# =========================
required_pkgs <- c("readxl", "dplyr", "stringr", "NCA", "ggplot2", "readr")

installed <- rownames(installed.packages())
for (p in required_pkgs) {
  if (!p %in% installed) install.packages(p)
}
invisible(lapply(required_pkgs, library, character.only = TRUE))

# =========================
# 1) LOAD DATA
# =========================
DATA_FILE <- "C:/Users/madivar/Documents/R Codes/Working own bus owoner or founder and not missing or NA task mastery full revenue3.xlsx"
df <- read_excel(DATA_FILE)

cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")

# =========================
# 2) TASK MASTERY ITEMS (21)
# =========================
ese_items <- df %>%
  select(contains(
    "Please indicate your degree of certainty in performing each of the following roles/tasks related to business ownership"
  ))
stopifnot(ncol(ese_items) == 21)

# =========================
# 3) ENSURE NUMERIC + BUILD ESE
# =========================
ese_items_num <- ese_items %>%
  mutate(across(everything(), ~ {
    v <- as.character(.x) %>% str_trim()
    suppressWarnings(nv <- as.numeric(v))
    if (any(is.na(nv) & v != "")) {
      case_when(
        tolower(v) == "sure" ~ 3,
        tolower(v) == "neither sure nor unsure" ~ 2,
        tolower(v) == "unsure" ~ 1,
        TRUE ~ NA_real_
      )
    } else nv
  }))

if (any(is.na(ese_items_num))) stop("Unexpected NA(s) in task mastery items after conversion.")
df$ESE <- rowSums(ese_items_num)

# =========================
# 4) REVENUE UPPER → LOG10 (NO RECODING, NO +1)
# =========================
stopifnot("Revenue Upper" %in% names(df))

df <- df %>%
  mutate(
    revenue_upper = as.numeric(`Revenue Upper`),
    log10_revenue = log10(revenue_upper)
  )
# =========================
# 5) NCA DATASET (INCLUDES ORIGINAL REVENUE + TASK MASTERY)
# =========================
ese_items_num_named <- ese_items_num
colnames(ese_items_num_named) <- paste0("ESE_item_", seq_len(ncol(ese_items_num_named)))

# Include original Revenue (interval text) IF it exists; otherwise omit cleanly
base_cols <- c("ESE", "revenue_upper", "log10_revenue")
if ("Revenue" %in% names(df)) base_cols <- c("ESE", "Revenue", "revenue_upper", "log10_revenue")

nca_df <- df %>%
  select(all_of(base_cols)) %>%
  bind_cols(ese_items_num_named) %>%
  na.omit()

cat("NCA dataset rows:", nrow(nca_df), "\n")
cat("NCA dataset columns:", ncol(nca_df), "\n")

readr::write_csv(nca_df, "nca_dataset_ESE_log10revenue_with_task_mastery.csv")

# =========================
# 6) SCATTER PLOT
# =========================
scatter_ESE_vs_log10_revenue <-
  ggplot(nca_df, aes(ESE, log10_revenue)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "ESE vs Log10(Revenue Upper)",
    subtitle = "Upper-left quadrant should be empty if ESE is necessary",
    x = "ESE (sum of 21 task mastery items)",
    y = "Log10(Revenue Upper)"
  ) +
  theme_minimal()

print(scatter_ESE_vs_log10_revenue)
ggsave("scatter_ESE_vs_log10_revenue.png", scatter_ESE_vs_log10_revenue,
       width = 8, height = 5, dpi = 300)

# =========================
# 7) NCA (cr_fdh + ce_fdh)
# =========================
model <- NCA::nca_analysis(
  data     = nca_df,
  x        = "ESE",
  y        = "log10_revenue",
  ceilings = c("cr_fdh", "ce_fdh")
)

NCA::nca_output(model)
capture.output(NCA::nca_output(model), file = "nca_output_summary.txt")

# =========================
# 8) EXPORT CEILING PLOTS
# =========================
png("nca_ceiling_plot_cr_fdh.png", width = 1200, height = 800, res = 150)
plot(model, ceiling = "cr_fdh",
     xlab = "ESE", ylab = "Log10(Revenue Upper)",
     main = "NCA Ceiling Plot (CR-FDH)")
dev.off()

png("nca_ceiling_plot_ce_fdh.png", width = 1200, height = 800, res = 150)
plot(model, ceiling = "ce_fdh",
     xlab = "ESE", ylab = "Log10(Revenue Upper)",
     main = "NCA Ceiling Plot (CE-FDH)")
dev.off()

cat("\nSaved outputs:\n",
    "- nca_dataset_ESE_log10revenue_with_task_mastery.csv\n",
    "- scatter_ESE_vs_log10_revenue.png\n",
    "- nca_output_summary.txt\n",
    "- nca_ceiling_plot_cr_fdh.png\n",
    "- nca_ceiling_plot_ce_fdh.png\n")
# =========================
# 9) THRESHOLD / BOTTLENECK DATASET (EXPORT)
# =========================
threshold_cr_fdh <- model$bottlenecks$cr_fdh
threshold_ce_fdh <- model$bottlenecks$ce_fdh

threshold_cr_fdh$ceiling <- "cr_fdh"
threshold_ce_fdh$ceiling <- "ce_fdh"

threshold_summary <- dplyr::bind_rows(threshold_cr_fdh, threshold_ce_fdh)

# Clean for CSV (ESE column sometimes comes as non-atomic due to "NN")
threshold_summary <- threshold_summary %>%
  mutate(
    ESE_required = as.character(ESE),
    ESE_required_num = suppressWarnings(as.numeric(ESE_required))
  ) %>%
  select(log10_revenue, ESE_required, ESE_required_num, ceiling)

readr::write_csv(threshold_summary, "threshold_summary_bottleneck.csv")

cat("- threshold_summary_bottleneck.csv\n")
