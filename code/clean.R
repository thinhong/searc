library(readxl)
library(janitor)
library(tidyverse)

df <- read_excel("data/data_20250416.xlsx")

# Clean column names
df <- clean_names(df)

# Remove first line (question details)
df <- df[-1, ]

# Drop NA and no consent (based on Q9)
df <- df |>
  filter(!is.na(q9_1))

sea_countries <- unique(df$q2)
sea_countries <- sea_countries[sea_countries != "Other, please specify:"]

# Clean variables
df <- df |>
  mutate(
    # Gender
    q1 = if_else(q1 == "Prefer to self-describe, please specify:", q1_5_text, q1),
    q1 = if_else(q1 == "ผู้ชายออกสาวนิดหน่อย", "Non-binary / third gender", q1),
    q1 = factor(
      q1,
      levels = c(
        "Female",
        "Male",
        "Non-binary / third gender",
        "Prefer not to say",
        "Bayot"
      )
    ),
    # Country based in
    q2 = case_when(
      q2 == "Other, please specify:" & q3 %in% sea_countries ~ q3,
      response_id == "R_4HYLhQebtrD4xdE" ~ "Thailand",
      q2_12_text == "Hàn Quốc" ~ "Vietnam",
      response_id == "R_8PsCEAmNwKRPf6o" ~ "Thailand",
      q2 == "Other, please specify:" ~ "Others",
      TRUE ~ q2
    ),
    q2 = factor(q2, levels = sea_countries),
    country = q2,
    country = if_else(
      country %in% c("Brunei Darussalam", "Myanmar", "Timor Leste"),
      "Others",
      country
    ),
    country = factor(
      country,
      levels = c(
        "Cambodia",
        "Indonesia",
        "Lao PDR",
        "Malaysia",
        "Philippines",
        "Singapore",
        "Thailand",
        "Vietnam",
        "Others"
      )
    ),
    # Country of experience
    
    # Institution categorized into public, private, UK-funded
    q5_category = case_when(
      str_detect(q5, ("UK-funded institution")) ~ "UK-funded",
      str_detect(
        q5,
        fixed("Public research institution or centre (government funded)")
      ) |
        str_detect(q5, fixed("Public university (government funded)")) |
        str_detect(
          q5,
          fixed("Public hospital or health-related facility (government funded)")
        ) ~ "Government funded",
      str_detect(q5, fixed("Private research institution or centre")) |
        str_detect(q5, fixed("Private university")) |
        str_detect(q5, fixed(
          "Private hospital or health-related facility"
        )) |
        str_detect(
          q5,
          fixed(
            "Externally-funded institution (through private, external government, other sources)"
          )
        ) |
        str_detect(q5, fixed("Industry or business")) |
        str_detect(q5, fixed("Funder or philanthropic organization")) ~ "Private and industry",
      TRUE ~ "Others"
    ),
    q5_category = factor(
      q5_category,
      levels = c(
        "Government funded",
        "Private and industry",
        "UK-funded",
        "Others"
      )
    ),
    # Years of experience
    q4 = factor(
      q4,
      levels = c("1-5 years", "5-10 years", "10-15 years", "20 years or more")
    ),
    # Job
    q7 = if_else(
      grepl("head of|director|lead", q7_12_text, ignore.case = T),
      "Dean or Head of Department",
      q7
    ),
    q7 = if_else(
      grepl("coordinator", q7_12_text, ignore.case = T),
      "Research administrator or manager",
      q7
    ),
    q7 = if_else(
      grepl("doctor", q7_12_text, ignore.case = T),
      "Research doctor or nurse",
      q7
    ),
    q7 = if_else(
      grepl("researcher", q7_12_text, ignore.case = T),
      "Research fellow, post-doctoral fellow, or other research positions",
      q7
    ),
    q7 = if_else(
      grepl("technical", q7_12_text, ignore.case = T),
      "Lab technician",
      q7
    ),
    q7 = if_else(
      grepl("manag|officer", q7_12_text, ignore.case = T),
      "Research administrator or manager",
      q7
    ),
    q7 = if_else(
      q7_12_text %in% c(
        "lecturer with research",
        "Lecturer, 60% teaching and 40% Research",
        "Lecturer (teaching & research)"
      ),
      "Research fellow, post-doctoral fellow, or other research positions",
      q7
    ),
    q7 = if_else(
      q7_12_text %in% c("Lecturer, main job is teaching, with some research"),
      "Lecturer (main job is teaching, no research)",
      q7
    ),
    q7 = factor(
      q7,
      levels = c(
        "Student (BSc, BA, MSc, MA, MPH, MD, PhD)",
        "Research assistant",
        "Research fellow, post-doctoral fellow, or other research positions",
        "Lab technician",
        "Research administrator or manager",
        "Research doctor or nurse",
        "Lecturer (main job is teaching, no research)",
        "Assistant Professor",
        "Associate Professor",
        "Full Professor",
        "Dean or Head of Department",
        "Other, please specify:"
      )
    ),
    # Area of research
    q8 = str_replace(q8, ",Other, please specify:", ""),
    q8 = str_replace_all(q8, ",([A-Z])", ";\\1"),
    q8 = case_when(
      q8 == "Other, please specify:" & q8_7_text %in% c("Immunization", "Pathology", "Pharmacist", "Pharmacy", "General surgery", "Medical education", "Surgery", "Pharmacological and chemical sciences", "One Health (Focusing on animal and human health)", "population genetics") ~ "Biomedicine, medicine, nursing, clinical",
      q8 == "Other, please specify:" & grepl("Chronic disease management", q8_7_text) ~ "Biomedicine, medicine, nursing, clinical",
      q8 == "Other, please specify:" & q8_7_text %in% c("AQUACULTURE NUTRITION", "Teknologi Industri Pertanian", "Fiisiologi Reproduksi Ternak", "Biochemistry", "Natural Science", "marine biology", "Finance", "Material science lab") ~ "STEM (science, technology, engineering, maths)",
      q8 == "Other, please specify:" & q8_7_text %in% c("Molecular", "Biology", "công nghệ sinh học", "ชีวโมเลกุล") ~ "Microbiology, virology, parasitology",
      q8 == "Other, please specify:" & q8_7_text %in% c("Market research on a variety of industries", "nhân sự", "Document control", "Bảo tàng", "Administration", "Khoa học giáo dục (giáo dục mầm non)", "Media", "Asian Studies", "Law", "bioethics", "Education", "support team") ~ "Social sciences, humanities",
      TRUE ~ q8
    ),
    # Main questions
    across(
      c(q9_1, q9_2, q9_3, q9_4, q9_5),
      ~ factor(
        .x,
        levels = c(
          "Strongly disagree",
          "Somewhat disagree",
          "Neither agree nor disagree",
          "Somewhat agree",
          "Strongly agree",
          "I don't know how to answer"
        ),
        ordered = T
      )
    ),
    across(
      .cols = c(q9_1, q9_2, q9_3, q9_4, q9_5),
      .fns = as.integer,
      .names = "{.col}_int"
    ),
    q10 = factor(q10, levels = c("No", "Yes")),
    q11_text_cleaned = case_when(
      q11_8_text %in% c("Non-clinicians", "Non-MD holders") |
        str_detect(
          q11_8_text,
          "As a non-clinical specialist researcher, opportunities for career progression is limited"
        ) ~ "Non-MD holders",
      str_detect(q11_8_text, regex("local?", ignore_case = T)) ~ "Local staff",
      q11_8_text %in% c(
        "Foriegners doesn't seem appropriate - as they are the nationals of the country i'm based in",
        "it looks easier for foreigners",
        "Non-foreigners"
      ) ~ "Local staff",
      q11_8_text %in% c("non postgraduate degree") ~ "Non-PhD holders",
      is.na(q11_8_text) ~ NA_character_,
      TRUE ~ "Others"
    ),
    q11 = if_else(
      grepl("Others, please specify:", q11),
      str_replace(q11, fixed("Others, please specify:"), q11_text_cleaned),
      q11
    )
  ) |>
  filter(q2 != "Others")

saveRDS(df, "data/data_cleaned.rds")
