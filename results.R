library(tidyverse)

enkat <- readRDS('Data/enkat.rds')
item_labels <- readRDS('Data/item_labels.rds')




# ── Schemalagd tid per Fakultet ──────────────────────────────────────────────────────────────────
enkat |>
  count(fakultet, Tid_schema_grp5) |>
  group_by(fakultet) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = Tid_schema_grp5, y = pct, fill = Tid_schema_grp5)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, .12))) +
  scale_fill_brewer(palette = "Blues", direction = 1) +
  facet_wrap(~ fakultet) +
  labs(x = NULL, y = NULL, title = "Schemalagd tid per fakultet") +
  theme_minimal() +
  theme(legend.position = "none")

# ── Schemalagd tid per ämne ───────────────────────────────────────────────────
# one student has missing course code and thus missing 
enkat |>
  filter(!is.na(subject)) |>
  mutate(subject = factor(subject, levels = c("Matte LTH", "Matte NF", "Matstat LTH", "Matstat NF"))) |>
  count(subject, Tid_schema_grp5) |>
  group_by(subject) |>
  mutate(pct = n / sum(n), n_total = sum(n)) |>
  ungroup() |>
  mutate(subject_lbl = fct_inorder(paste0(subject, " (n = ", n_total, ")"))) |>
  ggplot(aes(x = Tid_schema_grp5, y = pct, fill = Tid_schema_grp5)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.4, size = 3) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, .12))) +
  scale_fill_brewer(palette = "Greens", direction = 1) +
  facet_wrap(~ subject_lbl, nrow = 2) +
  labs(x = NULL, y = NULL, title = "Schemalagd tid per ämne") +
  theme_minimal() +
  theme(legend.position = "none")

enkat |>
  select(all_of(names(var05_labels))) |>
  pivot_longer(everything(), names_to = "var", values_to = "rating") |>
  drop_na(rating) |>
  summarise(prop_alltid = mean(rating == "Alltid"), .by = var) |>
  mutate(var = fct_reorder(var, prop_alltid)) |>
  ggplot(aes(x = prop_alltid, y = var)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  summarise(prop_alltid = mean(rating == "Alltid"), .by = var) |>
  mutate(var = fct_reorder(var, prop_alltid)) |>
  ggplot(aes(x = prop_alltid, y = var)) +
  geom_col() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_discrete(labels = var05_labels) +
  labs(x = "Andel 'Alltid'", y = NULL,
       title = "Hur ofta används varje resurs?",
       subtitle = "Andel studenter som svarat 'Alltid'")