########################################################################
## Table 4: Number of International Students by Field of Study,
## 2023/24 & 2024/25
## Sheet name in workbook: "4"
########################################################################

od_path <- here("ref", "OD25_Intl-Student-Census_Tables.xlsx")

## ---- 1. Read raw ------------------------------------------------------
t4_raw <- read_excel(od_path, sheet = "4", skip = 5, col_names = TRUE) |>
  clean_names()
# -> field_of_study, x2023_24, x2024_25, percent_change

t4_raw <- t4_raw |>
  rename(
    field        = field_of_study,
    year_2023_24 = x2023_24,
    year_2024_25 = x2024_25,
    pct_change   = percent_change
  ) |>
  mutate(field = str_squish(field))   # header had a trailing space; be safe on every row too

## ---- 2. Pull out the grand total row before block-grouping -----------
## Doing this first so it doesn't get treated as a 16th "parent
## category" block below.
t4_grand_total <- t4_raw |>
  filter(str_detect(coalesce(field, ""), regex("^TOTAL INTERNATIONAL", ignore_case = TRUE)))

t4_raw <- t4_raw |>
  filter(!str_detect(coalesce(field, ""), regex("^TOTAL INTERNATIONAL", ignore_case = TRUE)))

## ---- 3. Assign block IDs from blank-row separators --------------------
## Every blank row starts a new block boundary; cumsum over "is this
## row blank" gives each block (including the blank rows themselves)
## a shared id, then we drop the blanks.
t4_blocks <- t4_raw |>
  mutate(
    is_blank = is.na(field),
    block_id = cumsum(is_blank)
  ) |>
  filter(!is_blank) |>
  select(-is_blank)

## ---- 4. Split each block into its parent (first row) and any ----------
## children (remaining rows)
t4_blocks <- t4_blocks |>
  group_by(block_id) |>
  mutate(row_in_block = row_number()) |>
  ungroup()

t4_categories <- t4_blocks |>
  filter(row_in_block == 1) |>
  select(field, year_2023_24, year_2024_25, pct_change) |>
  rename(category = field)

## Lookup: block_id -> its parent category name, so each child row can
## look up which category it belongs to via a plain join (much safer
## than trying to attach it inline).
parent_lookup <- t4_blocks |>
  filter(row_in_block == 1) |>
  select(block_id, category = field)

t4_subfields <- t4_blocks |>
  filter(row_in_block > 1) |>
  left_join(parent_lookup, by = "block_id") |>
  select(category, subfield = field, year_2023_24, year_2024_25, pct_change)

## ---- 5. Sanity checks --------------------------------------------------
## Every category with children should have its children sum exactly
## to its own total (verified by hand above -- this just automates
## catching it if a future year's release breaks the pattern).
check_sums <- t4_subfields |>
  group_by(category) |>
  summarise(children_sum_2024 = sum(year_2024_25), .groups = "drop") |>
  left_join(t4_categories |> select(category, year_2024_25), by = "category") |>
  mutate(diff = children_sum_2024 - year_2024_25)

if (any(abs(check_sums$diff) > 0)) {
  message("Category/child mismatch -- inspect before trusting the breakdown:")
  print(check_sums |> filter(abs(diff) > 0))
}

## Categories should sum to the grand total.
stopifnot(
  abs(sum(t4_categories$year_2024_25) - t4_grand_total$year_2024_25) < 1,
  abs(sum(t4_categories$year_2023_24) - t4_grand_total$year_2023_24) < 1
)

## Flag (don't silently fix) the same-label parent/child collisions,
## so anyone joining this later knows to join on category+subfield,
## never on field name alone.
name_collisions <- t4_subfields |>
  filter(category == subfield) |>
  distinct(category)

message("Categories where a sub-field shares the parent's exact name -- ",
        "always disambiguate via `category` + `subfield` together, never `subfield` alone: ",
        paste(name_collisions$category, collapse = ", "))

## ---- 6. Tidy long format ------------------------------------------------
stem_cats <- c("Physical and life sciences", "Math and computer science", "Engineering", "Health professions")
t4_categories_tidy <- t4_categories |>
  select(category, year_2023_24, year_2024_25) |>
  pivot_longer(starts_with("year_"), names_to = "academic_year",
               names_prefix = "year_", values_to = "n_students") |>
  mutate(academic_year = str_replace(academic_year, "_", "/"),
          is_stem = if_else(category %in% stem_cats, "Yes", "No"))

t4_subfields_tidy <- t4_subfields |>
  select(category, subfield, year_2023_24, year_2024_25) |>
  pivot_longer(starts_with("year_"), names_to = "academic_year",
               names_prefix = "year_", values_to = "n_students") |>
  mutate(academic_year = str_replace(academic_year, "_", "/"),
          is_stem = if_else(category %in% stem_cats, "Yes", "No"))

## ---- 7. Export ------------------------------------------------------------
write.csv(t4_categories_tidy, here("ref","table4_categories_tidy.csv"), row.names = FALSE)
write.csv(t4_subfields_tidy,  here("ref","table4_subfields_tidy.csv"),  row.names = FALSE)