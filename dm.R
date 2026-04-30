#dm.R
library(tidyverse)

df0 <- read_sav("Data/export.sav")

# Item label lookups from Excel codebook (named vector: name = varname, value = label)
item_labels <- read_excel("Data/enkat_export.xlsx") |>
  filter(str_detect(`Variable Name`, "^VAR0[567]_")) |>
  mutate(item = str_extract(Label, "(?<= - ).+$")) |>
  select(var = `Variable Name`, item)

var05_labels <- item_labels |> filter(startsWith(var, "VAR05")) |>
  mutate(var = str_replace(var, "^VAR05_", "Resurs_Likert_")) |> deframe()
var06_labels <- item_labels |> filter(startsWith(var, "VAR06")) |>
  mutate(var = str_replace(var, "^VAR06_", "Resurs_Rank_")) |> deframe()
var07_labels <- item_labels |> filter(startsWith(var, "VAR07")) |>
  mutate(var = str_replace(var, "^VAR07_", "Påstående_")) |> deframe()

likert_05 <- c(Aldrig = 1, `Med--` = 2, `Med-` = 3, `Med+` = 4, `Med++` = 5, Alltid = 6)
likert_07 <- c(`Inte alls` = 1, `Med--` = 2, `Med-` = 3, `Med+` = 4, `Med++` = 5, Fullständigt = 6)

df <- df0 |>
  #Alltid was coded 0, should be 6 (to be more than 5)
  mutate(across(starts_with("VAR05_"), \(x)
    haven::labelled(ifelse(x == 0, 6L, as.integer(x)), likert_05)
  )) |>
  mutate(across(starts_with("VAR07_"), \(x)
    haven::labelled(as.integer(x), likert_07)
  )) |>
  mutate(across(where(haven::is.labelled), as_factor),
         program = as.character(VAR00)) |>
  mutate(course = as.character(VAR01)) |>
  separate_wider_regex(
    course,
    patterns = c(course_code = "[A-Z]+\\d+(?:\\s*/\\s*[A-Z]+\\d+)*",
                 "\\s+",
                 course_name = ".+"),
    too_few = "align_start") |> 
  separate_wider_regex(
    program,
    patterns = c(prog = ".+", "\\s+\\(", 
                 fakultet = "[^)]+", "\\)"),
    too_few = "align_start"
  ) |>
  # Two persons (ID 87 and 268) had NA for fakultet 
  # ID 87 - Master i beräkningsfysik, ID 268 Teacher...
  # From freetext I see that they are both Nfak and thus assign them to Nfak
  mutate(prog     = as_factor(prog),
         fakultet = as_factor(if_else(is.na(fakultet), "Nfak", fakultet))) |>
  mutate(
    course_name = str_remove(course_name, "\\s*\\([^)]*\\)$"),
    course_code = if_else(
      course_code == "FMSF50 / MASB13 / MASL01",
      if_else(fakultet == "LTH", "FMSF50", "MASB13"),
      course_code
    )
  ) |>
  select(ID, prog, fakultet, course_code, course_name, everything()) |>
  rename(
    Tid_totalt_timmar = VAR02,
    Tid_schema_proc   = VAR03
  ) |>
  rename_with(~ str_replace(.x, "^VAR04_", "Tid_uppd_timmar_"), starts_with("VAR04_")) |>
  rename_with(~ str_replace(.x, "^VAR05_", "Resurs_Likert_"),   starts_with("VAR05_")) |>
  rename_with(~ str_replace(.x, "^VAR06_", "Resurs_Rank_"),     starts_with("VAR06_")) |>
  rename_with(~ str_replace(.x, "^VAR07_", "Påstående_"),       starts_with("VAR07_"))

saveRDS(df,'Data/enkat.rds')
