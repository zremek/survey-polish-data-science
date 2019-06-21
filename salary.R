# we want min-max monthly salary in PLN for junior, regular, senior 

# kaggle 2018 Q8 experience Q9 compensation 
# k 2017 CompensationAmount CompensationCurrency Tenure as experience
# so 18 ConvertedSalary yearly in USD, YearsCodingProf
# so 19 YearsCodingProf, ConvertedSalary

kaggle_17_pl$CompensationCurrency %>% table(useNA = "always")
kaggle_17_pl$CompensationAmount %>% head(10)
parse_number(kaggle_17_pl$CompensationAmount) %>% summary()

k_17_salary <- kaggle_17_pl %>% select(CompensationAmount, CompensationCurrency, Tenure)
k_18_salary <- kaggle_18_pl %>% select(Q8, Q9)
so_18_salary <- so_18_pl %>% filter(is_ds_or_ml == TRUE) %>% 
  select(ConvertedSalary, YearsCodingProf)
so_19_salary <- so_19_pl %>% filter(is_ds_or_ml == TRUE) %>% 
  select(ConvertedComp, YearsCodePro)

### write stage above to csv?

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
      salary_dirty_year_pln > 23000 ~ salary_dirty_year_pln / 12))

# fix 96 PLN yearly as it should be 96K
k_17_salary$salary_clean_month_pln[5] <- 96 * 1000 / 12

boxplot(k_17_salary$salary_clean_month_pln ~ k_17_salary$Tenure)

# exchange rate to PLN for October 29th 2018
# https://www.nbp.pl/home.aspx?navid=archa&c=/ascx/tabarch.ascx&n=a210z181029
k_18_USD <- 3.7930

k_18_salary <- k_18_salary %>% 
  mutate(case_when(
    Q9 != "I do not wish to disclose my approximate yearly compensation" ~ salary_dirty_year_pln = parse_number(Q9) * k_18_USD),
         salary_dirty_month_pln = salary_dirty_year_pln / 12)

#### Q9 is a range!
