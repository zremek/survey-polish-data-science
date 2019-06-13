library(tidyverse)
library(scales)

demogr <- read_csv("demographics.csv")

# demogr %>% group_by(płeć) %>% summarise(n = n())
plec_levels <- c(Kobieta = "female", 
                 Kobieta = "Female", 
                 Kobieta = "Woman", 
                 Mężczyzna = "male",
                 Mężczyzna = "Male",
                 Mężczyzna = "Man",
                 Inna = "A different identity",
                 Inna = "Non-binary, genderqueer, or gender non-conforming",
                 Inna = "Woman;Man",
                 `Brak/odmowa odp.` = "Prefer not to say")
demogr <- demogr %>% mutate(Płeć = fct_recode(.f = płeć, 
                                              !!!plec_levels),
                            Płeć = fct_explicit_na(f = Płeć,
                                                   na_level = "Brak/odmowa odp."),
                            Płeć = fct_relevel(Płeć, "Inna", after = Inf),
                            Źródło = fct_relevel(Źródło, "PyData 2018 [n = 284]", after = Inf))
table(demogr$płeć, demogr$Płeć, useNA = "always")
prop.table(table(demogr$Źródło, demogr$Płeć), 1)

demogr %>% group_by(Źródło) %>% summarise(n = n(), 
                                          n_K = sum(Płeć == "Kobieta"),
                                          n_M = sum(Płeć == "Mężczyzna"),
                                          M_per_K = n_M / n_K) %>% 
  arrange(M_per_K)

plec <- ggplot(demogr) +
  geom_bar(aes(Źródło, fill = Płeć), position = "fill", colour = "black", width = 0.7) +
  theme_minimal(base_family = "serif", base_size = 10) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Na jedną kobietę przypada od 2 (WhyR? 2017)\ndo 13 mężczyzn (Stack Overflow 2018)",
       caption = 'Połączono dane z ankiet wymienionych na osi "Źródło"',
       x = "Źródło",
       y = NULL)

# png("plec.png", width = 160, height = 70, units = "mm", res = 300)
# plot(plec) # Rys. 7. in chapter 5.1.
# dev.off()

########### to do:
demogr %>% group_by(wiek) %>% summarise(n = n()) %>% View()
wiek_levels <- c(`mniej niż 18` = "17", 
                 `mniej niż 18` = "Under 18 years old", 
                 `18 - 24` = "18 - 24 years old", 
                 `25 - 34` = "25 - 34 years old",
                 `35 - 44` = "35 - 44 years old",
                 `45 - 54` =  "45 - 54 years old", 
                 `55 - 69` = "55-59",)