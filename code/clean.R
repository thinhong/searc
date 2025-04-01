library(readxl)
library(janitor)
library(tidyverse)

df <- read_excel("data/data_202519036.xlsx")

# Clean column names
df <- clean_names(df)

# Remove first line (question details)
df <- df[-1,]

# Drop NA and no consent (based on Q9)
df <- df |> 
  filter(!is.na(q9_1))

# Clean variables
df <- df |> 
  mutate(
    # Gender
    q1 = if_else(q1 == "Prefer to self-describe, please specify:", q1_5_text, q1),
    q1 = factor(q1, levels = c("Female", "Male", "Non-binary / third gender", "Prefer not to say", "Bayot")),
    # Country based in
    q2 = if_else(q2 == "Other, please specify:", "Other", q2),
    q2 = factor(q2),
    # Country of experience
    
    # Institution categorized into public, private, UK-funded
    q5_category = case_when(
      str_detect(q5, ("UK-funded institution")) ~ "UK-funded",
      str_detect(q5, fixed("Public research institution or centre (government funded)")) |
        str_detect(q5, fixed("Public university (government funded)")) |
        str_detect(q5, fixed("Public hospital or health-related facility (government funded)")) ~ "Government funded",
      str_detect(q5, fixed("Private research institution or centre")) |
        str_detect(q5, fixed("Private university")) |
        str_detect(q5, fixed("Private hospital or health-related facility")) |
        str_detect(q5, fixed("Externally-funded institution (through private, external government, other sources)")) |
        str_detect(q5, fixed("Industry or business")) |
        str_detect(q5, fixed("Funder or philanthropic organization")) ~ "Private and industry",
      TRUE ~ "Other"  # in case nothing matches
    ),
    q5_category = factor(q5_category, levels = c("Government funded", "Private and industry", "UK-funded", "Other")),
    # Years of experience
    q4 = factor(q4, levels = c("1-5 years", "5-10 years", "10-15 years", "20 years or more")),
    # Job
    q7 = factor(q7, levels = c("Student (BSc, BA, MSc, MA, MPH, MD, PhD)", "Research assistant", "Research fellow, post-doctoral fellow, or other research positions", "Lab technician", "Research administrator or manager", "Research doctor or nurse", "Lecturer (main job is teaching, no research)", "Assistant Professor", "Associate Professor", "Full Professor", "Dean or Head of Department", "Other, please specify:")),
    # Main questions
    q9_1 = factor(q9_1, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "I don't know how to answer")),
    q9_2 = factor(q9_2, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "I don't know how to answer")),
    q9_3 = factor(q9_3, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "I don't know how to answer")),
    q9_4 = factor(q9_4, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "I don't know how to answer")),
    q9_5 = factor(q9_5, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree", "I don't know how to answer")),
    q10 = factor(q10, levels = c("No", "Yes"))
  )

saveRDS(df, "data/data_cleaned.rds")