library(tidyverse)
library(scales)
# we want at least min-max monthly salary in PLN for junior, regular, senior 

## what vars?
# kaggle 2018 Q8 experience Q9 compensation 
# k 2017 CompensationAmount CompensationCurrency Tenure as experience
# so 18 ConvertedSalary yearly in USD, YearsCodingProf
# so 19 YearsCodingProf, ConvertedSalary

kaggle_17_pl$CompensationCurrency %>% table(useNA = "always")
kaggle_17_pl$CompensationAmount %>% head(10)
parse_number(kaggle_17_pl$CompensationAmount) %>% summary()

k_17_salary <- kaggle_17_pl %>% 
  filter(DataScienceIdentitySelect != "No" | is.na(DataScienceIdentitySelect)) %>% 
  select(CompensationAmount, CompensationCurrency, Tenure)
  
k_18_salary <- kaggle_18_pl %>% 
  filter(Q26 != "Definitely not" & Q26 != "Probably not" | is.na(Q26)) %>% 
  select(Q8, Q9) %>% 
  rename(experience = "Q8", compensation = "Q9")

so_18_salary <- so_18_pl %>% 
  filter(is_ds_or_ml == TRUE) %>% 
  select(ConvertedSalary, YearsCodingProf)

so_19_salary <- so_19_pl %>% 
  filter(is_ds_or_ml == TRUE) %>% 
  select(ConvertedComp, YearsCodePro)

### write stage above to csv? YES!

# exchange rate to PLN for August 25th 2017 - for k_17
# https://www.nbp.pl/home.aspx?navid=archa&c=/ascx/tabarch.ascx&n=a164z170825
k_17_salary$CompensationCurrency %>% table(useNA = "always")

k_17_USD <- 3.6112
k_17_AFN <- 0.05
k_17_EUR <- 4.26

k_17_salary <- k_17_salary %>% 
  mutate(salary_dirty_year_pln = case_when(
    CompensationCurrency == "PLN" ~ parse_number(CompensationAmount),
    CompensationCurrency == "EUR" ~ parse_number(CompensationAmount) * k_17_EUR,
    CompensationCurrency == "USD" ~ parse_number(CompensationAmount) * k_17_USD,
    CompensationCurrency == "AFN" ~ parse_number(CompensationAmount) * k_17_AFN),
    salary_dirty_month_pln = salary_dirty_year_pln / 12,
    salary_clean_month_pln = case_when(
      salary_dirty_year_pln <= 23000 ~ salary_dirty_year_pln,
      salary_dirty_year_pln > 23000 ~ salary_dirty_year_pln / 12), 
    doswiadczenie = fct_recode(.f = Tenure, 
                               `mniej niż 1 rok` = "Less than a year",
                               `1 - 2 lata` = "1 to 2 years",
                               `3 - 5 lat` = "3 to 5 years",
                               `6 - 10 lat` = "6 to 10 years",
                               `więcej niż 10 lat` = "More than 10 years"),
    doswiadczenie = fct_relevel(.f = doswiadczenie, "mniej niż 1 rok", after = 0))

# fix 96 PLN yearly as it should be 96K
k_17_salary$salary_clean_month_pln[5] <- 96 * 1000 / 12

boxplot(k_17_salary$salary_clean_month_pln ~ k_17_salary$Tenure)
by(k_17_salary$salary_clean_month_pln, k_17_salary$doswiadczenie, summary)

boxpl_k_17 <- ggplot(k_17_salary %>%
         filter(!is.na(salary_clean_month_pln))) +
  geom_boxplot(aes(x = doswiadczenie, y = salary_clean_month_pln)) +
  geom_dotplot(aes(x = doswiadczenie, y = salary_clean_month_pln), 
             alpha = 1/4, binaxis = 'y', stackdir = 'center',
             dotsize = 0.7) +
  coord_flip() +
  theme_minimal(base_family = "serif", base_size = 10) +
  labs(title = "Zarobki rosną wraz z długością doświadczenia zawodowego\ndla trzech środkowych kategorii doświadczenia.\nDla kategorii skrajnych zależność jest niejednoznaczna",
       caption = "Dane z ankiety Kaggle 2017 [n ważnych = 46]",
       y = "Miesięczne zarobki brutto lub netto [PLN]",
       x = "Doświadczenie zawodowe w analizie danych")

k_17_salary %>% 
  group_by(doswiadczenie) %>% 
  summarise(n = n(), 
            n_zar_T = mean(!is.na(salary_clean_month_pln)) * sum(n), 
            n_zar_F = mean(is.na(salary_clean_month_pln)) * sum(n))

# png("boxpl_k_17.png", width = 160, height = 75, units = "mm", res = 300)
# plot(boxpl_k_17) # Rys. 15. in chapter 5.1.3.
# dev.off()

# exchange rate to PLN for October 29th 2018
# https://www.nbp.pl/home.aspx?navid=archa&c=/ascx/tabarch.ascx&n=a210z181029
k_18_USD <- 3.7930

ggplot(k_18_salary) + geom_bar(aes(x = compensation, fill = experience), position = "dodge") +
  coord_flip()

ggplot(k_18_salary) + geom_bar(aes(x = experience, fill = compensation), position = "fill")

k_18_salary %>% group_by(compensation) %>% 
  summarise(n = n()) # zwinąć do powyżej 70 tys. usd rocznie

k_18_salary %>% group_by(experience) %>% 
  summarise(n = n()) # zwinąc do powyżej 10 lat doświadczenia

tibble(breaks_usd_year = c(10000, 20000, 30000, 40000, 50000, 60000, 70000),
       breaks_pln_month = breaks_usd_year * k_18_USD / 12)

k_18_salary <- k_18_salary %>% 
  mutate(zarobki = fct_collapse(.f = compensation,
    `do 3 100 PLN` = "0-10,000",
    `powyżej 3 100 do 6 300 PLN` = "10-20,000",
    `powyżej 6 300 do 9 500 PLN` = "20-30,000",
    `powyżej 9 500 do 12 600 PLN` = "30-40,000",
    `powyżej 12 600 do 15 800 PLN` = "40-50,000",
    `powyżej 15 800 do 18 900 PLN` = "50-60,000",
    `powyżej 18 900 do 22 100 PLN` = "60-70,000",
    `brak/odmowa odp.` = "I do not wish to disclose my approximate yearly compensation"),
    zarobki = fct_other(f = zarobki, 
                        keep = c("do 3 100 PLN", "powyżej 3 100 do 6 300 PLN",
                                 "powyżej 6 300 do 9 500 PLN", "powyżej 9 500 do 12 600 PLN",
                                 "powyżej 12 600 do 15 800 PLN", "powyżej 15 800 do 18 900 PLN",
                                 "powyżej 18 900 do 22 100 PLN", "brak/odmowa odp."), 
                        other_level = "powyżej 22 100 PLN"),
    zarobki = fct_explicit_na(f = zarobki, na_level = "brak/odmowa odp."),
    zarobki = fct_relevel(.f = zarobki, "brak/odmowa odp.", after = Inf),
    doswiadczenie = fct_collapse(experience,
                                 `mniej niż 1 rok` = "0-1",
                                 `1 - 2 lata` = "1-2",
                                 `3 - 5 lat` = c("2-3", "3-4", "4-5"),
                                 `6 - 10 lat` = c("5-10"),
                                 `więcej niż 10 lat` = c("10-15", "15-20", "20-25", "30 +")),
    doswiadczenie = fct_relevel(.f = doswiadczenie, "więcej niż 10 lat", after = Inf))

k_18_salary$doswiadczenie %>% table(useNA = "always") # n valid 228 = 252 - 24

bar_k_18_salary <- ggplot(k_18_salary %>% 
         filter(!is.na(doswiadczenie))) + 
  geom_bar(aes(x = doswiadczenie, fill = zarobki), position = "fill", colour = "black", width = 0.7) +
  theme_minimal(base_family = "serif", base_size = 10) +
  scale_fill_brewer(palette = "Set3", name = "Przybliżony przedział\nmiesięcznych zarobków\nbrutto lub netto") +
  scale_y_continuous(labels = scales::percent) +
  # coord_flip() +
  # theme(legend.position = "bottom") +
  labs(title = 'Częstość odmów/braków odpowiedzi o zarobkach rośnie\nwraz z długością doświadczenia zawodowego.\nZarobki rosną wraz z długością doświadczenia zawodowego\npoza kategorią najwyższą "więcej niż 10 lat"',
       caption = 'Dane z ankiety Kaggle 2018 [n ważnych = 228]',
       x = "Doświadczenie zawodowe w analizie danych",
       y = NULL)

# png("bar_k_18_salary.png", width = 160, height = 180, units = "mm", res = 300)
# plot(bar_k_18_salary) # Rys. 16. in chapter 5.1.3.
# dev.off()

# prop.table(table(k_18_salary$zarobki, k_18_salary$doswiadczenie), 2)
    
### so 19 & 18 bind and plot

so_18_schema$QuestionText[so_18_schema$Column == "ConvertedSalary"]
#  exchange rate to PLN for 2018-01-18
# https://www.nbp.pl/home.aspx?navid=archa&c=/ascx/tabarch.ascx&n=a013z180118
so_18_USD <- 3.4108

so_19_schema$QuestionText[so_19_schema$Column == "ConvertedComp"]
#  exchange rate to PLN for 2019-02-01
# https://www.nbp.pl/home.aspx?navid=archa&c=/ascx/tabarch.ascx&n=a023z190201
so_19_USD <- 3.7243

so_18_salary$ConvertedSalary %>% summary() # 52 from 121, 44%. 121 - 68 NA's = 53 entries but one is "0" 
so_18_salary %>% filter(ConvertedSalary > 0 | is.na(ConvertedSalary)) %>% 
  group_by(YearsCodingProf) %>% 
  summarise(n = n(), 
           n_zar_T = mean(!is.na(ConvertedSalary)) * sum(n), 
           n_zar_F = mean(is.na(ConvertedSalary)) * sum(n))

# so_19_salary$ConvertedComp %>% summary() # 73 from 111, 66%. 111 - 37 NA's = 74 entries but one is "0" 
# so_19_salary %>% filter(ConvertedComp > 0 | is.na(ConvertedComp)) %>% 
#   group_by(YearsCodePro) %>% 
#   summarise(n = n(), 
#             n_zar_T = mean(!is.na(ConvertedComp)) * sum(n), 
#             n_zar_F = mean(is.na(ConvertedComp)) * sum(n)) %>% 
#   View() # 0-29 years of exp

so_18_salary <- so_18_salary %>% mutate(salary_clean_month_pln = case_when(
  ConvertedSalary > 0 ~ ConvertedSalary * so_18_USD / 12),
  doswiadczenie = fct_recode(YearsCodingProf,
                             `0 - 2 lata` = "0-2 years",
                             `3 - 5 lat` = "3-5 years",
                             `6 - 8 lat` = "6-8 years",
                             `9 - 14 lat` = "9-11 years",
                             `9 - 14 lat` = "12-14 years"),
  doswiadczenie = fct_relevel(doswiadczenie, "9 - 14 lat", after = Inf),
  zrodlo = "Stack Overflow 2018 [n ważnych = 52]")

so_19_salary$ConvertedComp[43] <- 36 * 1000 # fix small outlier

so_19_salary <- so_19_salary %>% mutate(salary_clean_month_pln = case_when(
  ConvertedComp > 0 ~ ConvertedComp * so_19_USD / 12),
  doswiadczenie = fct_collapse(YearsCodePro,
                             `0 - 2 lata` = c("Less than 1 year", "1", "2"),
                             `3 - 5 lat` = c("3", "4", "5"),
                             `6 - 8 lat` = c("6", "7", "8"),
                             `9 - 14 lat` = c("9", "10", "11", "12", "13", "14"),
                             `więcej niż 14 lat` = c("15", "16", "17", "18", "19",
                                                     "20", "21", "22", "23", "24",
                                                     "25", "26", "27", "28", "29")),
  doswiadczenie = fct_relevel(doswiadczenie, "9 - 14 lat", after = Inf),
  doswiadczenie = fct_relevel(doswiadczenie, "więcej niż lat", after = Inf), 
  zrodlo = "Stack Overflow 2019 [n ważnych = 73]")
  

by(so_19_salary$salary_clean_month_pln, so_19_salary$doswiadczenie, summary)
by(so_18_salary$salary_clean_month_pln, so_18_salary$doswiadczenie, summary)

so_18_salary %>% filter(salary_clean_month_pln > 0 | is.na(salary_clean_month_pln)) %>% 
  group_by(doswiadczenie) %>% 
  summarise(n = n(), 
            n_zar_T = mean(!is.na(salary_clean_month_pln)) * sum(n), 
            n_zar_F = mean(is.na(salary_clean_month_pln)) * sum(n))

so_bind_18_19_salary <- bind_rows(so_18_salary, so_19_salary)

boxpl_so_18_19 <- ggplot(so_bind_18_19_salary %>%
                       filter(!is.na(salary_clean_month_pln))) +
  geom_boxplot(aes(x = doswiadczenie, y = salary_clean_month_pln, fill = zrodlo),
               position = position_dodge(1)) +
  geom_dotplot(aes(x = doswiadczenie, y = salary_clean_month_pln, fill = zrodlo),
               binaxis = 'y', stackdir = "center",
               position = position_dodge(1),
               alpha = 1/3, dotsize = 2,
               binwidth = 500, show.legend = FALSE) +
  coord_flip() +
  theme_minimal(base_family = "serif", base_size = 10) +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#80b1d3", "#fdb462"),
                    name = "Źródło") +
  labs(title = "Zarobki rosną wraz z długością doświadczenia zawodowego.\nZarobki dla tych samych kategorii doświadczenia wzrosły w czasie",
       caption = "Dane z ankiet Stack Overflow 2018 i 2019",
       y = "Miesięczne zarobki brutto na pełen etat [PLN]",
       x = "Doświadczenie w zawodowym pisaniu kodu")

# png("boxpl_so_18_19.png", width = 160, height = 140, units = "mm", res = 300)
# plot(boxpl_so_18_19) # Rys. 17. in chapter 5.1.3.
# dev.off()
