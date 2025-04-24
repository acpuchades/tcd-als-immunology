library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(janitor)
library(missForest)

impute_data <- function(df) {
  miss <- df |>
    select(where(is.numeric), where(is.factor)) |>
    as.data.frame()
  df |>
    select(-c(where(is.numeric), where(is.factor))) |>
    bind_cols(missForest(miss)$ximp) |>
    select(all_of(colnames(df)))
}

as_subject_group <- function(x) {
  x |>
    case_match(
      "Control" ~ "Control",
      "Patient" ~ "ALS",
      "ALS" ~ "ALS",
    ) |>
    factor(levels = c("Control", "ALS"))
}

as_cognitive_status <- function(x) {
  factor(x, levels = c("Normal", "Abnormal"))
}

as_sex <- function(x) {
  factor(x, levels = c("M", "F"))
}

as_site_of_onset <- function(x) {
  x |>
    case_match(
      "Bulbar" ~ "Bulbar",
      "Spinal" ~ "Spinal",
      "Bulbar and Cognitive/Behavioural" ~ "Bulbar",
      "Thoracic/Respiratory" ~ "Thoracic/Respiratory",
    ) |>
    factor(levels = c("Spinal", "Bulbar", "Thoracic/Respiratory"))
}

as_progression_category <- function(x) {
  factor(x, levels = c("NP", "SP", "FP"))
}

as_timepoint <- function(x) {
  factor(x, levels = c("T0", "T1"))
}

clinical <- read_excel("data/Clinical Data_ImmunologyCohort_2024 07 23 Barry.xlsx", sheet = "Clinical_Demographics") |>
  clean_names() |>
  select(-immunology_code_2, -score_t0, -t1) |>
  rename(sample_id = immunology_code_3) |>
  rename_with(~ str_replace(.x, "_patient_only", "")) |>
  mutate(
    across(patient_control, as_subject_group),
    across(site_of_onset, as_site_of_onset),
    across(cognitive_status, as_cognitive_status),
    disease_duration = pmax(
      years_from_onset_present,
      survival_yr_death_yr_onset,
      na.rm = TRUE
    ),
    vital_status = case_when(
      !is.na(survival_yr_death_yr_onset) ~ 1,
      is.na(years_from_onset_present) & !is.na(survival_yr_death_yr_onset) ~ 0
    )
  )

alsfrs <- read_excel("data/Clinical Data_ImmunologyCohort_2024 07 23 Barry.xlsx", sheet = "ALSFRS") |>
  clean_names() |>
  rename(sample_id = immunology_code) |>
  group_by(register_code) |>
  fill(sample_id) |>
  ungroup()

progression_info <- alsfrs |>
  left_join(clinical |> select(sample_id, date_of_onset), by = "sample_id") |>
  mutate(
    months_from_onset = (clinical_visit_date - date_of_onset) / dmonths(1),
    delta_fs = (48 - total) / months_from_onset
  ) |>
  select(sample_id, months_from_onset, delta_fs) |>
  drop_na() |>
  slice_min(months_from_onset, n = 1, by = sample_id) |>
  mutate(
    progression_category = as_progression_category(case_when(
      delta_fs < 0.5 ~ "SP",
      delta_fs |> between(0.5, 1) ~ "NP",
      delta_fs > 1 ~ "FP"
    ))
  )

clinical <- clinical |>
  left_join(progression_info |> select(-months_from_onset), by = "sample_id")

imm_pheno <- bind_rows(
  read_excel("data/2025 03 09 ALS Immune phenotyping panel EXCL DEAD.xlsx", sheet = "Controls") |> mutate(group = "Control", timepoint = as_timepoint("T0")),
  read_excel("data/2025 03 09 ALS Immune phenotyping panel EXCL DEAD.xlsx", sheet = "ALS T0") |> mutate(group = "ALS", timepoint = as_timepoint("T0")),
  read_excel("data/2025 03 09 ALS Immune phenotyping panel EXCL DEAD.xlsx", sheet = "ALS T1") |> mutate(group = "ALS", timepoint = as_timepoint("T1")),
) |>
  clean_names(replace = c(`\\-` = "neg_"), parsing_option = 2) |>
  select(-c(sex, als_type, comments)) |>
  mutate(
    sample_id = str_replace_all(sample_id, "_T[0-1]", ""),
    group = as_subject_group(group),
  ) |>
  drop_na(sample_id) |>
  pivot_longer(
    cols = -c(sample_id, group, timepoint),
    names_to = "marker",
    values_to = "proportion"
  ) |>
  pivot_wider(
    names_from = c(marker, timepoint),
    values_from = proportion,
    names_sep = "_"
  )

imm_cytokines_l <- bind_rows(
  read_excel("data/2025 03 09 Cytokine panel EXCL DEAD.xlsx", sheet = "CD4 Controls") |> mutate(celltype = "CD4", group = "Control", timepoint = as_timepoint("T0")),
  read_excel("data/2025 03 09 Cytokine panel EXCL DEAD.xlsx", sheet = "CD4 ALS T0") |> mutate(celltype = "CD4", group = "ALS", timepoint = as_timepoint("T0")),
  read_excel("data/2025 03 09 Cytokine panel EXCL DEAD.xlsx", sheet = "CD4 ALS T1") |> mutate(celltype = "CD4", group = "ALS", timepoint = as_timepoint("T1")),
  read_excel("data/2025 03 09 Cytokine panel EXCL DEAD.xlsx", sheet = "CD8 Controls") |> mutate(celltype = "CD8", group = "Control", timepoint = as_timepoint("T0")),
  read_excel("data/2025 03 09 Cytokine panel EXCL DEAD.xlsx", sheet = "CD8 ALS T0") |> mutate(celltype = "CD8", group = "ALS", timepoint = as_timepoint("T0")),
  read_excel("data/2025 03 09 Cytokine panel EXCL DEAD.xlsx", sheet = "CD8 ALS T1") |> mutate(celltype = "CD8", group = "ALS", timepoint = as_timepoint("T1")),
) |>
  clean_names() |>
  select(-c(sex, als_type)) |>
  rename(tnf_alpha = tn_fa, cd161 = x161) |>
  rename_with(columns = starts_with("if_ny"), ~ str_replace(.x, "if_ny", "ifn_gamma")) |>
  mutate(
    sample_id = str_replace_all(sample_id, "_T[0-1]", ""),
    group = as_subject_group(group),
  ) |>
  drop_na(sample_id) |>
  select(-starts_with("x"), -comments)

imm_cytokines <- imm_cytokines_l |>
  pivot_longer(
    cols = -c(sample_id, group, celltype, timepoint),
    names_to = "cytokine",
    values_to = "levels"
  ) |>
  pivot_wider(
    names_from = c(celltype, cytokine, timepoint),
    values_from = levels,
    names_sep = "_"
  ) |>
  select(-c(starts_with("CD8_cd161_tregs"), starts_with("CD8_il17_tregs"), starts_with("CD8_ifn_gamma_tregs")))

imm_cytokines_t0 <- imm_cytokines |>
  select(sample_id, group, ends_with("T0")) |>
  mutate(timepoint = as_timepoint("T0"), .after = group) |>
  rename_with(~ str_replace(.x, "_T0", ""))

imm_cytokines_t1 <- imm_cytokines |>
  select(sample_id, group, ends_with("T1")) |>
  mutate(timepoint = as_timepoint("T1"), .after = group) |>
  rename_with(~ str_replace(.x, "_T1", ""))

imm_treg_tmem <- bind_rows(
  read_excel("data/2025 03 09 Treg_Tmem panel EXCL DEAD.xlsx", sheet = "Controls") |> mutate(group = "Control", timepoint = as_timepoint("T0")),
  read_excel("data/2025 03 09 Treg_Tmem panel EXCL DEAD.xlsx", sheet = "ALS T0") |> mutate(group = "ALS", timepoint = as_timepoint("T0")),
  read_excel("data/2025 03 09 Treg_Tmem panel EXCL DEAD.xlsx", sheet = "ALS T1") |> mutate(group = "ALS", timepoint = as_timepoint("T1")),
) |>
  clean_names() |>
  select(-c(sex, als_type)) |>
  mutate(
    sample_id = str_replace_all(sample_id, "_T[0-1]", ""),
    group = as_subject_group(group)
  ) |>
  drop_na(sample_id) |>
  select(-x18) |>
  pivot_longer(
    cols = -c(sample_id, group, timepoint),
    names_to = "marker",
    values_to = "proportion"
  ) |>
  pivot_wider(
    names_from = c(marker, timepoint),
    values_from = proportion,
    names_sep = "_"
  )

imm_features <- imm_pheno |>
  left_join(imm_cytokines |> select(-group), by = "sample_id") |>
  left_join(imm_treg_tmem |> select(-group), by = "sample_id")

imm_features.imputed <- impute_data(imm_features)
imm_pheno.imputed <- imm_features.imputed |> select(all_of(names(imm_pheno)))
imm_cytokines.imputed <- imm_features.imputed |> select(all_of(names(imm_cytokines)))
imm_treg_tmem.imputed <- imm_features.imputed |> select(all_of(names(imm_treg_tmem)))

imm_cytokines_l.imputed <- imm_cytokines.imputed |>
  pivot_longer(-c(sample_id, group)) |>
  mutate(
    celltype = str_extract(name, "^(CD[48])_", group = 1),
    timepoint = str_extract(name, "_(T[01])$", group = 1) |> as_timepoint(),
    name = name |> str_replace("^CD[48]_", "") |> str_replace("_T[01]$", "")
  ) |>
  pivot_wider(names_from = name, values_from = value)

imm_cytokines_t0.imputed <- imm_cytokines.imputed |>
  select(sample_id, group, ends_with("T0")) |>
  mutate(timepoint = as_timepoint("T0"), .after = group) |>
  rename_with(~ str_replace(.x, "_T0", ""))

imm_cytokines_t1.imputed <- imm_cytokines.imputed |>
  select(sample_id, group, ends_with("T1")) |>
  mutate(timepoint = as_timepoint("T1"), .after = group) |>
  rename_with(~ str_replace(.x, "_T1", ""))
