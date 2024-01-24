library(tidyverse)
library(NHANES)

nhanes_small <- select(
  NHANES, Age, Gender, BMI, Diabetes,
  PhysActive, BPSysAve, BPDiaAve, Education
)

nhanes_small <- rename_with(nhanes_small, snakecase::to_snake_case)

nhanes_small <- rename(nhanes_small, sex = gender)

# Did we have to select here before renaming?
nhanes_small %>%
  select(phys_active) %>%
  rename(physically_active = phys_active)

nhanes_small %>%
  select(bp_sys_ave, education)

nhanes_small %>%
  select(bp_sys_ave, bp_dia_ave) %>%
  rename(bp_sys = bp_sys_ave, bp_dia = bp_dia_ave)

nhanes_small %>%
  select(bmi, contains("age"))

nhanes_small %>%
  select(starts_with("bp")) %>%
  rename(bp_systolic = bp_sys_ave)

nhanes_small %>%
  filter(phys_active == "No")

nhanes_small %>%
  filter(phys_active != "No")

nhanes_small %>%
  filter(bmi == 25)

nhanes_small %>%
  filter(bmi >= 25)

nhanes_small %>%
  filter(bmi == 25 & phys_active == "No")

nhanes_small %>%
  filter(bmi == 25 | phys_active == "No")

nhanes_small %>%
  arrange(education, age)

nhanes_small <- nhanes_small %>%
  mutate(age = age * 12)

nhanes_small <- nhanes_small %>%
  mutate(
    age = age * 12,
    logged_bmi = log(bmi)
  )

nhanes_small <- nhanes_small %>%
  mutate(age = age / 12)

nhanes_small <- nhanes_small %>%
  mutate(old = if_else(age >= 30 * 12, "Yes", "No"))


nhanes_small <- nhanes_small %>%
  filter(bmi >= 20 & bmi <= 40 & diabetes == "Yes") %>%
  mutate(mean_arterial_pressure = ((bp_dia_ave * 2) + bp_sys_ave) / 3) %>%
  mutate(young_child = if_else(age < 6 * 12, "Yes", "No"))

nhanes_small %>%
  summarise(max_bmi = max(bmi))

nhanes_small %>%
  summarise(max_bmi = max(bmi, na.rm = TRUE), min_bmi = min(bmi, na.rm = TRUE))

nhanes_small %>%
  filter(!is.na(diabetes)) %>%
  group_by(diabetes) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_bmi = mean(bmi, na.rm = TRUE)
  )

nhanes_small %>%
  filter(!is.na(diabetes)) %>%
  group_by(diabetes, phys_active) %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    mean_bmi = mean(bmi, na.rm = TRUE)
  ) %>%
  ungroup()

readr::write_csv(
    nhanes_small,
    here::here("data/nhanes_small.csv")
)

nhanes_small <- readr::read_csv(here::here("data/nhanes_small.csv"))
