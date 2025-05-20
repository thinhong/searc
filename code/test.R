df |> 
  filter(str_detect(q7_12_text, regex("researcher?", ignore_case = T))) |> pull(q7_12_text)

df |> 
  filter(grepl("manag|officer", q7_12_text, ignore.case = T)) |> pull(q7_12_text)

tmp <- df_long |> 
  filter(q11 == "Local staff")

table(tmp$q5_category)

table(df$q11_8_text[df$q11_text_cleaned == "Others"])
