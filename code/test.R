df |> 
  filter(str_detect(q7_12_text, regex("researcher?", ignore_case = T))) |> pull(q7_12_text)

df |> 
  filter(grepl("manag|officer", q7_12_text, ignore.case = T)) |> pull(q7_12_text)
