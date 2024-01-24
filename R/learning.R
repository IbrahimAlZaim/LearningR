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
