library(tidyverse)

### Stack Overflow Annual Developer Survey 2019
### https://drive.google.com/file/d/1QOmVDpd8hcVYqqUXDXf68UMDWQZP0wQV/

so_19_pl <- read_csv("data/stack_overflow_developer_survey_2019/survey_results_public.csv")
so_19_pl <- filter(so_19_pl, Country == "Poland")
so_19_schema <- read_csv("data/stack_overflow_developer_survey_2019/survey_results_schema.csv")

length(levels(factor(so_19_pl$DevType)))

so_19_pl <- so_19_pl %>%
  mutate(is_ds_or_ml = grepl(pattern = "Data scientist or machine learning specialist",
                                              x = DevType)) 
so_19_pl %>% 
  filter(is_ds_or_ml == TRUE) %>% 
  dim() # 111 data scientists / ml specialists

prop.table(table(so_19_pl$is_ds_or_ml)) # almost 6% of all surveyed are ds

### Stack Overflow Annual Developer Survey 2018
### https://drive.google.com/uc?export=download&id=1_9On2-nsBQIw3JiY43sWbrF8EjrqrR4U

so_18_pl <- read_csv("data/stack_overflow_developer_survey_2018/survey_results_public.csv")
so_18_pl <- filter(so_18_pl, Country == "Poland")
so_18_schema <- read_csv("data/stack_overflow_developer_survey_2018/survey_results_schema.csv")

length(levels(factor(so_18_pl$DevType)))

so_18_pl <- so_18_pl %>%
  mutate(is_ds_or_ml = grepl(pattern = "Data scientist or machine learning specialist",
                             x = DevType)) 
so_18_pl %>% 
  filter(is_ds_or_ml == TRUE) %>% 
  dim() # 121 data scientists / ml specialists

prop.table(table(so_18_pl$is_ds_or_ml)) # again almost 6% of all surveyed are ds

### Kaggle Machine Learning & Data Science Survey 2017
### https://www.kaggle.com/kaggle/kaggle-survey-2017
# manual sign in
kaggle_17_pl <- read_csv("data/kaggle-survey-2017/multipleChoiceResponses.csv",
                        col_types = cols(
                          CompensationAmount = col_character(),
                          WorkToolsFrequencyKNIMECommercial = col_character()))
kaggle_17_pl <- kaggle_17_pl %>% filter(Country == "Poland")
kaggle_17_schema <- read_csv("data/kaggle-survey-2017/schema.csv")

### 2018 Kaggle ML & DS Survey
### https://www.kaggle.com/kaggle/kaggle-survey-2018
kaggle_18_pl <- read_csv("data/kaggle-survey-2018/multipleChoiceResponses.csv")
kaggle_18_pl <- kaggle_18_pl %>% filter(Q3 == "Poland")
kaggle_18_schema <- read_csv("data/kaggle-survey-2018/SurveySchema.csv")

### PyData Warsaw 2018
### https://github.com/stared/random_data_explorations/tree/master/201811_pydatawaw2018
pydata_18_pl <- read_csv("https://raw.githubusercontent.com/stared/random_data_explorations/master/201811_pydatawaw2018/pdwc2018_anonym.csv")
pydata_18_pl <- pydata_18_pl %>% filter(Country == "Poland")

### WhyR? Warsaw 2017
### https://github.com/WhyR2017/
whyr_17_pl <- read_csv("https://raw.githubusercontent.com/WhyR2017/konkursy/master/dane_z_formularza_rejestracyjnego.csv")
whyr_17_pl <- whyr_17_pl %>% filter(Kraj == "Polska")

