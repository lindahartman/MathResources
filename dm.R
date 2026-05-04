#dm.R
library(tidyverse)
library(haven)
library(readxl)

df0 <- read_sav("Data/export.sav")

# Item label lookups from Excel codebook (named vector: name = varname, value = label)
item_labels <- read_excel("Data/enkat_export.xlsx") |>
  filter(str_detect(`Variable Name`, "^VAR0[567]_")) |>
  mutate(item = str_extract(Label, "(?<= - ).+$")) |>
  select(var0 = `Variable Name`, item)

var05_labels <- item_labels |> filter(startsWith(var0, "VAR05")) |>
  mutate(var = str_replace(var0, "^VAR05_", "Resurs_Likert_")) |> select(var, item) |> deframe()
var06_labels <- item_labels |> filter(startsWith(var0, "VAR06")) |>
  mutate(var = str_replace(var0, "^VAR06_", "Resurs_Rank_")) |> select(var, item) |> deframe()
var07_labels <- item_labels |> filter(startsWith(var0, "VAR07")) |>
  mutate(var = str_replace(var0, "^VAR07_", "Påstående_")) |> select(var, item) |> deframe()

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
  rename_with(~ str_replace(.x, "^VAR07_", "Påstående_"),       starts_with("VAR07_")) |> 
  select(ID:course_name,starts_with('Tid'),starts_with('Resurs_'),starts_with('Påstående'),
         everything())



item_labels <- item_labels |>
  mutate(var = var0 |>
    str_replace("^VAR04_", "Tid_uppd_timmar_") |>
    str_replace("^VAR05_", "Resurs_Likert_")   |>
    str_replace("^VAR06_", "Resurs_Rank_")     |>
    str_replace("^VAR07_", "Påstående_")
  ) |> 
  select(var,item,var0)

# Add factor versions
df <- df |>
  mutate(Tid_schema_grp5 = case_when(
    Tid_schema_proc == 0   ~ "0%",
    Tid_schema_proc <= 30  ~ "1–30%",
    Tid_schema_proc <= 50  ~ "31–50%",
    Tid_schema_proc <= 70  ~ "51–70%",
    Tid_schema_proc <= 100 ~ "71–100%"
  ) |> factor(levels = c("0%", "1–30%", "31–50%", "51–70%", "71–100%")),
  .after = Tid_schema_proc) |>
  mutate(subject = case_when(
    str_starts(course_code, "FMA") ~ "Matte LTH",
    str_starts(course_code, "FMS") ~ "Matstat LTH",
    str_starts(course_code, "MAT") ~ "Matte NF",
    str_starts(course_code, "MAS") ~ "Matstat NF"
  ) |> factor(),
  .after = course_code) |>
  mutate(course_gr = case_when(
    course_code == "FMAB70" &
      prog %in% c("Teknisk fysik", "Teknisk matematik", "Teknisk nanovetenskap") ~ 1L,
    course_code == "FMAB70"                                                       ~ 2L,
    course_code == "MATA32"                                                       ~ 3L,
    course_code %in% c("FMSF50", "FMSF20")                                       ~ 4L,
    course_code == "MASA03"                                                       ~ 5L
    ) |>
    factor(levels = c(1L, 2L, 3L, 4L, 5L),
           labels = c("B2 (F,pi,Nano)", "B2 (övriga)",
                      "MATA32", "FMSF50/20", "MASA03")),
  .after = subject)

## Add question formulations
item_labels <- item_labels |> 
  mutate(question = case_when(
  str_starts(var, "Resurs_Likert") ~ "Hur ofta använder du var och en av resurserna nedan vid icke-schemalagda självstudier i kursen för att lära dig matematik? Gör dina bedömningar på en skala från 'Aldrig' till 'Alltid' där 'Alltid' betyder att resursen använts någon gång vid varje icke-schemalagt självstudietillfälle i denna kurs den senaste veckan.",
  str_starts(var, "Resurs_Rank") ~ "Vänligen välj de fem resurser för ditt matematiklärande som du använt mest den senaste veckan när du studerat i denna kurs genom att markera i listan. Här avses alla typer av resurser, både sådana som tillhandahålls av universitetet och övriga.",
  str_starts(var, "Resurs_Påstående") ~ "Markera i vilken grad du håller med om följande påståenden angående dina studier i denna kurs")
)


saveRDS(df,'Data/enkat.rds')
saveRDS(item_labels,'Data/item_labels.rds')

rm(list=c("df","df0","item_labels","likert_05","likert_07","var05_labels","var06_labels","var07_labels"))
