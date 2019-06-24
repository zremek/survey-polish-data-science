library(tidyverse)
# kaggle 2017
kaggle_17_pl$DataScienceIdentitySelect %>% factor() %>% summary()
length(kaggle_17_pl$DataScienceIdentitySelect)
length(kaggle_17_pl$DataScienceIdentitySelect[kaggle_17_pl$DataScienceIdentitySelect != "No"])

bind_kag_17 <- kaggle_17_pl %>%
  filter(DataScienceIdentitySelect != "No" | is.na(DataScienceIdentitySelect)) %>% 
  select(GenderSelect, Age, MajorSelect, FormalEducation) %>% 
  rename(płeć = GenderSelect, wiek = Age, poziom_wykształcenia = FormalEducation,
         kierunek_wykształcenia = MajorSelect) %>% 
  mutate(Źródło = "Kaggle 2017 [n = 130]", wiek = as.character(wiek))
# kaggle 2018
kaggle_18_pl$Q26 %>% table() # filter out Definitely not Probably not 301-(16+33=49)=252
kaggle_18_pl$Q26 %>% factor() %>% summary()

bind_kag_18 <- kaggle_18_pl %>% 
  filter(Q26 != "Definitely not" & Q26 != "Probably not" | is.na(Q26)) %>% 
  select(Q1, Q2, Q4, Q5) %>% 
  rename(płeć = Q1, wiek = Q2, poziom_wykształcenia = Q4, kierunek_wykształcenia = Q5) %>% 
  mutate(Źródło = "Kaggle 2018 [n = 252]", wiek = as.character(wiek))
# Q4 What is the highest level of formal education 
# that you have attained **or plan to attain** within the next 2 years?

# so 2018
bind_so_18 <- so_18_pl %>% 
  filter(is_ds_or_ml == TRUE) %>% 
  select(Student, FormalEducation, UndergradMajor, Gender, Age) %>% 
  rename(płeć = Gender, wiek = Age, poziom_wykształcenia = FormalEducation,
         kierunek_wykształcenia = UndergradMajor) %>% 
  mutate(Źródło = "Stack Overflow 2018 [n = 121]", wiek = as.character(wiek))

# so 2019
bind_so_19 <- so_19_pl %>%
  filter(is_ds_or_ml == TRUE) %>% 
  select(Student, EdLevel, UndergradMajor, Gender, Age, Trans) %>% 
  rename(płeć = Gender, wiek = Age, poziom_wykształcenia = EdLevel,
         kierunek_wykształcenia = UndergradMajor) %>% 
  mutate(Źródło = "Stack Overflow 2019 [n = 111]", wiek = as.character(wiek))

# whyR 2017
bind_whyr_17 <- whyr_17_pl %>%
  select(`Stopień naukowy do umieszczenia na certyfikacie uczestnictwa w konferencji (o ile dotyczy)`,
         Płeć, wiek) %>%
  rename(poziom_wykształcenia = `Stopień naukowy do umieszczenia na certyfikacie uczestnictwa w konferencji (o ile dotyczy)`,
         płeć = Płeć) %>% 
  mutate(Źródło = "WhyR? 2017 [n = 202]", wiek = as.character(wiek))

# pydata 2018
bind_py_18 <- pydata_18_pl %>% select(Gender) %>% rename(płeć = Gender) %>% 
  mutate(Źródło = "PyData 2018 [n = 284]")

# bind and write file

write_csv(bind_rows(bind_kag_17, bind_kag_18, bind_so_18, bind_so_19, 
                    bind_whyr_17, bind_py_18),
          "demographics.csv")
