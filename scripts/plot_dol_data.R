# Top 25 job categories
lca_dedup <- readRDS(here("ref", "lca_dedup.rds"))

job_cat_tab <- lca_dedup |> 
  tbl_summary(
    include = "SOC_TITLE_lumped",
    label = list(SOC_TITLE_lumped ~ "Job Category (Using Gov't Codes (SOC))"),
    statistic = list(all_categorical() ~ "{n} ({p}%)")
    # no sort argument — factor level order drives row order now
  ) |>
  bold_labels() |>
  modify_header(label = "")

# Chart of top job categories
top5_chart_data <- readRDS(here("ref","lca_top5_chart_data.rds"))

job_cat_by_year <- ggplot(top5_chart_data, aes(x = pct, y = factor(year), fill = chart_category)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d(name = "Job Category") +
  labs(
    title = "Top Five Job Categories for H1-B Applicants by Year",
    subtitle = "Each applicant counted only once, irrespective of number of applications or application status",
    x = "Percentage of H-1B Applications",
    y = "Fiscal Year",
    caption = "Source: US Department of Labor."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot"
  )
