#dm.R
library(tidyverse)
library(haven)
library(readxl)
#library(forcats)

collapse_top2 <- function(x) {
  fct_collapse(x,
               Ofta = c("Med++", "Alltid"),
               other_level    = "Övriga"
  )
}

df0 <- read_sav("Data/export.sav")
df0_eng <- read_sav("Data/export_eng.sav")

# Item label lookups from Excel codebook (named vector: name = varname, value = label)
item_labels <- read_excel("Data/enkat_export.xlsx") |>
  filter(str_detect(`Variable Name`, "^VAR0[567]_")) |>
  mutate(item = str_extract(Label, "(?<= - ).+$")) |>
  select(var0 = `Variable Name`, item)

item_labels_eng <- read_excel("Data/enkat_export_eng.xlsx") |>
  filter(str_detect(`Variable Name`, "^VAR0[567]_")) |>
  mutate(item_eng = str_extract(Label, "(?<= - ).+$")) |>
  select(var0 = `Variable Name`, item_eng)
item_labels <- item_labels |> left_join(item_labels_eng)

var05_labels <- item_labels |> filter(startsWith(var0, "VAR05")) |>
  mutate(var = str_replace(var0, "^VAR05_", "Resurs_Likert_")) |> select(var, item) |> deframe()
var06_labels <- item_labels |> filter(startsWith(var0, "VAR06")) |>
  mutate(var = str_replace(var0, "^VAR06_", "Resurs_Rank_")) |> select(var, item) |> deframe()
var07_labels <- item_labels |> filter(startsWith(var0, "VAR07")) |>
  mutate(var = str_replace(var0, "^VAR07_", "Påstående_")) |> select(var, item) |> deframe()

likert_05 <- c(Aldrig = 1, `Med--` = 2, `Med-` = 3, `Med+` = 4, `Med++` = 5, Alltid = 6)
likert_07 <- c(`Inte alls` = 1, `Med--` = 2, `Med-` = 3, `Med+` = 4, `Med++` = 5, Fullständigt = 6)
likert_05_eng <- c(Never = 1, `Med--` = 2, `Med-` = 3, `Med+` = 4, `Med++` = 5, Always = 6)
likert_07_eng <- c(`Not at all` = 1, `Med--` = 2, `Med-` = 3, `Med+` = 4, `Med++` = 5, Completely = 6)


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
  select(var,item,item_eng,var0)

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


df <- df |>
  mutate(across(
    starts_with("Resurs_Likert"),
    collapse_top2,
    .names = "{.col}_bin"
  ))

df |> select(Resurs_Likert_1, Resurs_Likert_1_bin) |> 
  count(Resurs_Likert_1, Resurs_Likert_1_bin)


## Add question formulations
item_labels <- item_labels |> 
  mutate(fraga = case_when(
  str_starts(var, "Resurs_Likert") ~ "Hur ofta använder du var och en av resurserna nedan vid icke-schemalagda självstudier i kursen för att lära dig matematik? Gör dina bedömningar på en skala från 'Aldrig' till 'Alltid' där 'Alltid' betyder att resursen använts någon gång vid varje icke-schemalagt självstudietillfälle i denna kurs den senaste veckan.",
  str_starts(var, "Resurs_Rank") ~ "Vänligen välj de fem resurser för ditt matematiklärande som du använt mest den senaste veckan när du studerat i denna kurs genom att markera i listan. Här avses alla typer av resurser, både sådana som tillhandahålls av universitetet och övriga.",
  str_starts(var, "Resurs_Påstående") ~ "Markera i vilken grad du håller med om följande påståenden angående dina studier i denna kurs"
  ),
  question = case_when(
    str_starts(var, "Resurs_Likert") ~ "How often do you use each of the resources below in your unscheduled sessions of self-study for learning mathematics for this course? Make a judgement on a scale from 'Never' to 'Always', where 'Always' means that the resource has been used at least once during every self study session for this course the last week.",
    str_starts(var, "Resurs_Rank") ~ "	Please choose the five resources for your learning in mathematics that you have used the most the last week when you have studied for this course, by marking them in the list. Here we mean all types of resources, the ones provided by the university as well as other resources.",
    str_starts(var, "Resurs_Påstående") ~ "How often do you use each of the resources below in your unscheduled sessions of self-study for learning mathematics for this course? Make a judgement on a scale from 'Never' to 'Always', where 'Always' means that the resource has been used at least once during every self study session for this course the last week."
  )
  
)


## Descriptive course table -------------------------------------------------
course_n <- enkat |>
  filter(!is.na(course_code)) |>
  count(course_code, name = "n_students")

course_table <- tribble(
  ~course_code, ~course_name,                                                                ~course_name_abbr,    ~faculty,      ~subject,                   ~subject_abbr, ~year,  ~comments,
  "FMAB50",     "Calculus in One Variable A2",                                               "Calculus 1A2",       "Engineering", "Mathematics",              "Math",        "1",    "",
  "FMAB70",     "Calculus in One Variable B2",                                               "Calculus 1B2",       "Engineering", "Mathematics",              "Math",        "1",    "",
  "FMSF20",     "Mathematical Statistics, Basic Course (for E and D)",                       "MathStat (E,D)",     "Engineering", "Mathematical Statistics",  "MathStat",    "3",    "",
  "FMSF50",     "Mathematical Statistics, Basic Course (for L, V, Risk, Physics, Teachers)", "MathStat (L,V,…)",   "Engineering", "Mathematical Statistics",  "MathStat",    "3",    "",
  "FMSF80",     "Mathematical Statistics, Basic Course (for F, I, Pi)",                      "MathStat (F,I,Pi)",  "Engineering", "Mathematical Statistics",  "MathStat",    "2",    "",
  "MASA03",     "Mathematical Statistics, Basic Course (Faculty of Science)",                "MathStat (Sci)",     "Science",     "Mathematical Statistics",  "MathStat",    "1–3",  "",
  "MASB13",     "Mathematical Statistics, Basic Course (for L, V, Risk, Physics, Teachers)", "MathStat (L,V,…) S", "Science",     "Mathematical Statistics",  "MathStat",    "1–3",  "",
  "MATA31",     "Analysis in One Variable (Faculty of Science)",                             "Analysis 1V (Sci)",  "Science",     "Mathematics",              "Math",        "1",    "",
  "MATA32",     "Algebra and Vector Geometry (Faculty of Science)",                          "Algebra & VG (Sci)", "Science",     "Mathematics",              "Math",        "1",    "",
) |>
  left_join(course_n, by = "course_code") |>
  select(course_code, course_name, course_name_abbr, faculty, subject, subject_abbr,
         year, n_students, comments)

## English version of df ---------------------------------------------------
dfe <- df |>
  select(-course_name) |>
  left_join(course_table |> select(course_code, course_name), by = "course_code") |>
  relocate(course_name, .after = course_code) |>
  mutate(
    fakultet  = fct_recode(fakultet,
                           Engineering = "LTH",
                           Science     = "Nfak"),
    subject   = fct_recode(subject,
                           Mathematics              = "Matte LTH",
                           Mathematics              = "Matte NF",
                           `Mathematical Statistics` = "Matstat LTH",
                           `Mathematical Statistics` = "Matstat NF"),
    course_gr = fct_recode(course_gr, `B2 (others)` = "B2 (övriga)"),
    across(starts_with("Resurs_Likert") & !ends_with("_bin"),
           \(x) fct_recode(x, Never = "Aldrig", Always = "Alltid")),
    across(ends_with("_bin"),
           \(x) fct_recode(x, Often = "Ofta", Other = "Övriga")),
    across(starts_with("Påstående_"),
           \(x) fct_recode(x, `Not at all` = "Inte alls", Completely = "Fullständigt")),
    prog_eng = fct_recode(prog,
      # LTH Civil Engineering programmes
      "Engineering Physics"                       = "Teknisk fysik",
      "Mechanical Engineering"                    = "Maskinteknik",
      "Computer Science and Engineering"          = "Datateknik",
      "Industrial Engineering and Management"     = "Industriell ekonomi",
      "Electrical Engineering"                    = "Elektroteknik",
      "Civil Engineering"                         = "Väg- och vattenbyggnad",
      "Surveying and Land Management"             = "Lantmäteri",
      "Biotechnology"                             = "Bioteknik",
      "Environmental Engineering"                  = "Ekosystemteknik",
      "Chemical Engineering"                      = "Kemiteknik",
      "Engineering Mathematics"                   = "Teknisk matematik",
      "Engineering Nanoscience"                   = "Teknisk nanovetenskap",
      "Risk Management and Safety Engineering"    = "Risk, säkerhet och krishantering",
      "Fire Protection Engineering"               = "Brandteknik",
      "Biomedical Engineering"                    = "Medicin och teknik",
      "Information and Communication Technology"  = "Informations- och kommunikationsteknik",
      "Mechanical Engineering with Engineering Design" = "Maskinteknik med teknisk design",
      # Faculty of Science programmes
      "Bachelor's Programme in Mathematics"       = "Kandidatprogram i matematik",
      "Bachelor's Programme in Physics"           = "Kandidatprogram i fysik",
      # Other
      "Stand-alone Course"                        = "Fristående kurs",
      "Other Education"                           = "Annan utbildning:"
    ),
    .after = prog
  )

saveRDS(df,'Data/enkat.rds')
saveRDS(dfe, 'Data/enkat_eng.rds')
saveRDS(item_labels,'Data/item_labels.rds')
saveRDS(course_table, 'Data/course_table.rds')

rm(list=c("df","df0","item_labels","likert_05","likert_07","var05_labels","var06_labels","var07_labels"))
