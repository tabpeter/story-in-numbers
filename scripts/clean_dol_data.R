lca_files <- dir_ls(here("ref"), regexp = "LCA_Disclosure_Data_FY202.*\\.xlsx$")

# lca_combined <- lca_files |> 
#   set_names() |> 
#   map_dfr(read_excel, .id = "source_file", col_types = "text")

# glimpse(lca_combined) # look at results
# saveRDS(lca_combined, file = here("ref", "lca_combined.rds")) # this takes a while to read in, save for convenience

lca_combined <- readRDS(here("ref", "lca_combined.rds"))

# check for duplicates
# lca_combined |>
#   filter(VISA_CLASS == "H-1B") |> 
#   count(CASE_NUMBER) |> 
#   filter(n > 1) |> 
#   arrange(desc(n))

# lca_combined |> 
#   filter(CASE_NUMBER == c("I-200-20268-843604")) |> 
#   View()

# For those with multiple dates, choose the first one (all we need for descriptive stats)
lca_dedup <- lca_combined |> 
  # look only at H1-B
  filter(VISA_CLASS == "H-1B") |> 
  # Sort by case number and date, then eliminate duplicates. 
  arrange(CASE_NUMBER, DECISION_DATE) |> 
  distinct(CASE_NUMBER, .keep_all = TRUE) |>
  mutate(
    SOC_TITLE_lumped = fct_lump_n(
      SOC_TITLE,
      n = 25,
      other_level = "All Other Categories"
    ) |> 
      fct_infreq() |> 
      fct_relevel("All Other Categories", after = Inf)  # force to last position
) |> 
# To examine change over time, I need year/quarter variable
  mutate(
    year = str_extract(source_file, "FY(\\d{4})", group = 1) |> as.integer(),
    quarter = str_extract(source_file, "Q(\\d)", group = 1) |> as.integer()
  )

# checks
# glimpse(lca_dedup)
# duplicated(lca_dedup$CASE_NUMBER) |> any() # check to make sure no dups remain
# table(lca_dedup$year, useNA = "always")
# table(lca_dedup$quarter, useNA = "always")

# save data for creating tables/figures
saveRDS(lca_dedup, here("ref", "lca_dedup.rds"))

# top 5 categories overall, based on the already-lumped variable
top5 <- lca_dedup |>
  filter(SOC_TITLE_lumped != "All Other Categories") |>
  count(SOC_TITLE_lumped, sort = TRUE) |>
  slice_head(n = 5) |>
  pull(SOC_TITLE_lumped) |> 
  as.character()

# collapse everything else into "Other" so percentages sum to 100%
top5_chart_data <- lca_dedup |>
  mutate(
    chart_category = fct_other(
      SOC_TITLE_lumped,
      keep = top5,
      other_level = "Other"
    )
  ) |>
  count(year, chart_category) |>
  group_by(year) |>
  mutate(pct = n / sum(n)) |>
  ungroup()

saveRDS(top5_chart_data, here("ref","lca_top5_chart_data.rds"))
