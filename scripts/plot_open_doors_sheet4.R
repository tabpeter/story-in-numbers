########################################################################
## Horizontal stacked bar chart: STEM field breakdown by academic year
## Input: t4_categories_tidy (with is_stem), as constructed above
##
## Same approach as the country-level chart: collapse into stem_fields
## (each STEM category kept individually, everything else -> "Other"),
## normalize to % of that year's total so each bar sums to 100%, keep
## "Other" pinned last/rightmost, viridis "D" fill.
##
## One assumption, since there's no natural "sort by size" here with
## only 2 bars: academic_year is ordered chronologically (2023/24 on
## top, 2024/25 below) rather than by magnitude -- flag if you want
## it flipped.
########################################################################

t4_categories_tidy <- read.csv(here("ref","table4_categories_tidy.csv"))
t4_subfields_tidy <- read.csv(here("ref","table4_subfields_tidy.csv"))

## ---- 1. Collapse category into stem_fields, using year totals --------
## (year totals, not the overall grand total) so each bar's segments
## sum to that year's own 100%, since 2023/24 and 2024/25 have
## slightly different total enrollment.
year_totals <- t4_categories_tidy %>%
  group_by(academic_year) %>%
  summarise(year_total = sum(n_students), .groups = "drop")

t4_stem <- t4_categories_tidy %>%
  mutate(stem_fields = if_else(is_stem == "Yes", category, "Other")) %>%
  group_by(academic_year, stem_fields) %>%
  summarise(n_students = sum(n_students), .groups = "drop") %>%
  left_join(year_totals, by = "academic_year") %>%
  mutate(pct_of_students = 100 * n_students / year_total)

## Sanity check: each year's segments should sum to ~100%
stopifnot(
  t4_stem %>%
    group_by(academic_year) %>%
    summarise(total_pct = sum(pct_of_students), .groups = "drop") %>%
    pull(total_pct) %>%
    { all(abs(. - 100) < 0.01) }   # exact here since these are raw counts, not pre-rounded %s
)

## ---- 2. Order the axis and fill --------------------------------------
year_order <- c("2023/24", "2024/25")   # chronological -- see note above

stem_field_names <- t4_stem %>%
  filter(stem_fields != "Other") %>%
  distinct(stem_fields) %>%
  arrange(stem_fields) %>%
  pull(stem_fields)

fill_order <- c(stem_field_names, "Other")

t4_stem <- t4_stem %>%
  mutate(
    academic_year = factor(academic_year, levels = year_order),
    stem_fields   = factor(stem_fields, levels = fill_order)
  )

## ---- 3. Plot -----------------------------------------------------------
## Same position_stack(reverse = TRUE) trick as before: ggplot's
## default stacking puts the FIRST fill level farthest from zero, so
## with "Other" listed last in fill_order, reverse = TRUE is needed
## to actually push it to the far end (rightmost) as intended.
stem_fields_by_year <- ggplot(t4_stem, aes(x = pct_of_students, y = academic_year, fill = stem_fields)) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.5) +
  scale_fill_viridis(discrete = TRUE, option = "D", name = "Field") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "STEM Field Concentration Among International Students by Academic Year",
    subtitle = "All places of origin, based on Table 4 field-of-study counts",
    x = "% of students",
    y = NULL,
    caption = "Source: IIE Open Doors 2025. 'Other' includes all non-STEM categories and Undeclared."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title.position = "plot"
  )
# ggsave("stem_fields_by_year.png", stem_fields_by_year, width = 9, height = 4, dpi = 300)

# Also count how many total STEM students per year 
total_stem_students <- t4_categories_tidy |> 
  filter(is_stem == "Yes") |> 
  group_by(academic_year) |> 
  summarize(N = sum(n_students), .groups = "drop")
