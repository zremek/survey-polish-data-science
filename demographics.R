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
                            Płeć = fct_relevel(Płeć, "Brak/odmowa odp.", after = Inf),
                            Płeć = fct_relevel(Płeć, "Inna", after = 2),
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


# demogr %>% group_by(wiek) %>% summarise(n = n()) %>% View()

wiek_levels <- c(`mniej niż 18 lat` = "17", 
                 `mniej niż 18 lat` = "Under 18 years old", 
                 `18 - 24` = "18 - 24 years old", 
                 `18 - 24` = "18-21",
                 `18 - 24` = "22-24",
                 `18 - 24` = "19",
                 `18 - 24` = "20",
                 `18 - 24` = "21",
                 `18 - 24` = "22",
                 `18 - 24` = "23",
                 `18 - 24` = "24",
                 `25 - 34` = "25 - 34 years old",
                 `25 - 34` = "25",
                 `25 - 34` = "25-29",
                 `25 - 34` = "26",
                 `25 - 34` = "27",
                 `25 - 34` = "28",
                 `25 - 34` = "29",
                 `25 - 34` = "30",
                 `25 - 34` = "30-34",
                 `25 - 34` = "31",
                 `25 - 34` = "32",
                 `25 - 34` = "33",
                 `25 - 34` = "34",
                 `35 - 44` = "35",
                 `35 - 44` = "35 - 44 years old",
                 `35 - 44` = "35-39",
                 `35 - 44` = "36",
                 `35 - 44` = "37",
                 `35 - 44` = "38",
                 `35 - 44` = "39",
                 `35 - 44` = "40",
                 `35 - 44` = "40-44",
                 `35 - 44` = "41",
                 `35 - 44` = "42",
                 `35 - 44` = "43",
                 `35 - 44` = "44",
                 `45 - 54` = "45 - 54 years old", 
                 `45 - 54` = "45",
                 `45 - 54` = "45-49",
                 `45 - 54` = "47",
                 `45 - 54` = "48",
                 `45 - 54` = "49",
                 `45 - 54` = "50",
                 `45 - 54` = "50-54",
                 `45 - 54` = "52",
                 `45 - 54` = "53",
                 `55 - 69` = "55-59",
                 `55 - 69` = "55",
                 `55 - 69` = "55-59",
                 `55 - 69` = "56",
                 `55 - 69` = "57",
                 `55 - 69` = "58",
                 `55 - 69` = "60-69",
                 `Brak/odmowa odp.` = "0")

demogr <- demogr %>% mutate(Wiek = fct_recode(.f = wiek, 
                                              !!!wiek_levels),
                            Wiek = fct_explicit_na(f = Wiek,
                                                   na_level = "Brak/odmowa odp."),
                            Wiek = fct_relevel(.f = Wiek, "Brak/odmowa odp.", after = Inf))
                  
table(demogr$wiek, demogr$Wiek, useNA = "always")
prop.table(table(demogr$Źródło, demogr$Wiek), 1)
demogr %>% mutate(older_than_34 = Wiek %in% c("35 - 44", "45 - 54", "55 - 69")) %>% 
  group_by(Źródło) %>% summarise(n = n(), n_older = sum(older_than_34), 
                                 prop_older = n_older / n)

wiek <- ggplot(demogr %>% filter(Źródło != "PyData 2018 [n = 284]")) +
  geom_bar(aes(Źródło, fill = Wiek), position = "fill", colour = "black", width = 0.7) +
  theme_minimal(base_family = "serif", base_size = 10) +
  theme(axis.text.x = element_text(angle = 25)) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) +
  # coord_flip() +
  labs(title = "W każdym ze źródeł najczęściej wskazywano wiek od 25 do 34 lat",
       subtitle = "osoby od 35 r.ż. stanowią nie więcej niż 1/4 badanych (Kaggle 2017 i 2018)",
       caption = 'Połączono dane z ankiet wymienionych na osi "Źródło"',
       x = "Źródło",
       y = NULL)

# png("wiek.png", width = 160, height = 180, units = "mm", res = 300)
# plot(wiek) # Rys. 8. in chapter 5.1.
# dev.off()

demogr %>% group_by(poziom_wykształcenia) %>% summarise(n = n()) %>% View()

poziom_wyksz_levels <- c(`Licencjat/inżynier` = "Bachelor's degree",
                         `Licencjat/inżynier` = "Bachelor’s degree",
                         `Licencjat/inżynier` = "Bachelor’s degree (BA, BS, B.Eng., etc.)",
                         Doktorat = "Doctoral degree",
                         Doktorat = "dr",
                         Doktorat = "dr hab.",
                         Doktorat = "dr inż.",
                         Średnie = "I did not complete any formal education past high school",
                         `Brak/odmowa odp.` = "I prefer not to answer",
                         `Licencjat/inżynier` = "inż.",
                         `Licencjat/inżynier` = "lic.", 
                         Magister = "Master's degree",
                         Magister = "Master’s degree",
                         Magister = "Master’s degree (MA, MS, M.Eng., MBA, etc.)",
                         Magister = "mgr",
                         Magister = "mgr inż.",
                         Średnie = "No formal education past high school",
                         Doktorat = "Other doctoral degree (Ph.D, Ed.D., etc.)",
                         Podstawowe = "Primary/elementary school",
                         Doktorat = "prof. nzw. dr hab.",
                         Doktorat = "prof. nzw. dr hab. inż.",
                         Magister = "Professional degree",
                         Magister = "Professional degree (JD, MD, etc.)",
                         Średnie = "Secondary school (e.g. American high school, German Realschule or Gymnasium, etc.)",
                         Średnie = "Some college/university study without earning a bachelor's degree",
                         Średnie = "Some college/university study without earning a bachelor’s degree",
                         Średnie = "Some college/university study without earning a degree")

demogr <- demogr %>% mutate(`Poziom wykształcenia` = fct_recode(.f = poziom_wykształcenia, 
                                              !!!poziom_wyksz_levels),
                            `Poziom wykształcenia` = fct_explicit_na(f = `Poziom wykształcenia`,
                                                   na_level = "Brak/odmowa odp."),
                            `Poziom wykształcenia` = fct_inorder(f = `Poziom wykształcenia`),
                            `Poziom wykształcenia` = fct_relevel(.f = `Poziom wykształcenia`, "Brak/odmowa odp.",
                                                                 after = Inf),
                            `Poziom wykształcenia` = fct_relevel(.f = `Poziom wykształcenia`, "Doktorat",
                                                                 after = 0))

table(demogr$poziom_wykształcenia, demogr$`Poziom wykształcenia`, useNA = "always")

prop.table(table(demogr$Źródło, demogr$`Poziom wykształcenia`),1)

poziom <- ggplot(demogr %>% filter(Źródło != "PyData 2018 [n = 284]")) +
  geom_bar(aes(Źródło, fill = `Poziom wykształcenia`),
           position = "fill", colour = "black", width = 0.7) +
  theme_minimal(base_family = "serif", base_size = 10) +
  theme(axis.text.x = element_text(angle = 20)) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) +
  # coord_flip() +
  labs(title = "W każdym ze źródeł najczęściej wskazywano wykształcenie magisterskie",
       subtitle = "ale udział osób z doktoratem jak i wykształceniem średnim\nsięga 1/6 wskazań dla różnych źródeł",
       caption = 'Połączono dane z ankiet wymienionych na osi "Źródło"',
       x = "Źródło",
       y = NULL,
       fill = "Poziom wykształcenia")

# png("poziom.png", width = 160, height = 140, units = "mm", res = 300)
# plot(poziom) # Rys. 8. in chapter 5.1.
# dev.off()

demogr %>% group_by(kierunek_wykształcenia) %>% summarise(n = n()) %>% View()

kier_wykszt_levels <- c(`Biznes/ekonomia`	= "A business discipline (accounting, economics, finance, etc.)",
                        `Biznes/ekonomia` = "A business discipline (ex. accounting, finance, marketing)",
                        `Medyczne/przyrodnicze` = "A health science (ex. nursing, pharmacy, radiology)",
                        Humanistyczne = "A humanities discipline",
                        Humanistyczne = "A humanities discipline (ex. literature, history, philosophy)",
                        `Medyczne/przyrodnicze` =	"A natural science (ex. biology, chemistry, physics)",
                        Społeczne = "A social science",
                        Społeczne = "A social science (ex. anthropology, psychology, political science)",
                        Techniczne = "Another engineering discipline (ex. civil, electrical, mechanical)", 
                        Informatyczne = "Computer Science",
                        Informatyczne = "Computer science (software engineering, etc.)", 
                        Informatyczne = "Computer science, computer engineering, or software engineering",
                        Techniczne = "Electrical Engineering",
                        Techniczne = "Engineering (non-computer focused)",
                        `Medyczne/przyrodnicze` = "Environmental science or geology",
                        Humanistyczne = "Humanities (history, literature, philosophy, etc.)",
                        Inne = "I never declared a major",
                        Informatyczne = "Information systems, information technology, or system administration",
                        Informatyczne = "Information technology, networking, or system administration",
                        `Matematyczne/statystyczne` = "Mathematics or statistics",
                        `Medyczne/przyrodnicze` = "Medical or life sciences (biology, chemistry, medicine, etc.)",
                        Inne = "Other",
                        `Medyczne/przyrodnicze` = "Physics",
                        `Medyczne/przyrodnicze` = "Physics or astronomy",
                        Społeczne = "Psychology",
                        Społeczne = "Social sciences (anthropology, psychology, sociology, etc.)")

demogr <- demogr %>% mutate(`Kierunek wykształcenia` = fct_recode(.f = kierunek_wykształcenia, 
                                                                !!!kier_wykszt_levels),
                            `Kierunek wykształcenia` = fct_explicit_na(f = `Kierunek wykształcenia`,
                                                                     na_level = "Brak/odmowa odp."),
                            `Kierunek wykształcenia` = fct_infreq(`Kierunek wykształcenia`),
                            `Kierunek wykształcenia` = fct_relevel(.f = `Kierunek wykształcenia`, "Inne", after = Inf),
                            `Kierunek wykształcenia` = fct_relevel(.f = `Kierunek wykształcenia`, "Brak/odmowa odp.",
                                                                 after = Inf))

prop.table(table(demogr$`Kierunek wykształcenia`, demogr$`Poziom wykształcenia`))

kierunek <- ggplot(demogr %>%
         filter(Źródło != "PyData 2018 [n = 284]" & Źródło != "WhyR? 2017 [n = 202]")) +
  geom_bar(aes(Źródło, fill = `Kierunek wykształcenia`),
           position = "fill", colour = "black", width = 0.7) +
  theme_minimal(base_family = "serif", base_size = 10) +
  theme(axis.text.x = element_text(angle = 20)) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) +
  # coord_flip() +
  labs(title = "W każdym ze źródeł najczęściej wskazywano wykształcenie informatyczne",
       subtitle = "w drugiej kolejności wskazania dotyczą kierunków matematycznych/statystycznych\nlub medycznych/przyrodniczych",
       caption = 'Połączono dane z ankiet wymienionych na osi "Źródło"',
       x = "Źródło",
       y = NULL,
       fill = "Kierunek wykształcenia")

# png("kierunek.png", width = 160, height = 140, units = "mm", res = 300)
# plot(kierunek) # Rys. 8. in chapter 5.1.
# dev.off()

##### to do

# demogr %>% mutate(older_than_34 = Wiek %in% c("35 - 44", "45 - 54", "55 - 69"))
# przekrój wiek/płeć piramidka źródło jako facets
ggplot(demogr %>% 
         filter(Źródło != "PyData 2018 [n = 284]")) + 
  geom_count(aes(x = `Płeć`, y = fct_rev(Wiek))) +
  scale_size_continuous(range = c(2, 12)) +
  facet_grid(rows = vars(Źródło))

ggplot(demogr %>% 
         filter(Źródło != "PyData 2018 [n = 284]") %>%
         mutate(older_than_44 = Wiek %in% c("45 - 54", "55 - 69"))) + 
  geom_bar(aes(fill = older_than_44, x = Płeć), position = "dodge") +
  scale_y_log10() +
  facet_grid(rows = vars(Źródło))

ggplot(data = demogr %>% 
         filter(Płeć %in% c("Kobieta", "Mężczyzna")),
       aes(x = fct_rev(Wiek), fill = Płeć)) + 
  geom_bar(data = subset(demogr, Płeć == "Kobieta"),
           mapping = aes(y = ..count..)) + 
  geom_bar(data = subset(demogr, Płeć == "Mężczyzna"), 
           mapping = aes(y = -..count.. ),
           position = "identity") +
  scale_y_continuous(labels = abs) +
  coord_flip()

ggplot(data = demogr %>% 
         filter(Płeć %in% c("Kobieta", "Mężczyzna"),
                Wiek != "Brak/odmowa odp.")) +
  geom_bar(aes(x = Płeć, fill = Wiek), position = "fill")  

#### show diff
demogr %>% 
  filter(Płeć %in% c("Kobieta", "Mężczyzna"),
         # Wiek != "Brak/odmowa odp.",
         Źródło != "WhyR? 2017 [n = 202]") %>%
  group_by(Wiek) %>% 
  summarise(n = n(), 
            Mężczyźni = sum(Płeć == "Mężczyzna"),
            Kobiety = sum(Płeć == "Kobieta")) %>% 
  mutate(pct_M = Mężczyźni / sum(Mężczyźni),
         pct_K = Kobiety / sum(Kobiety),
         pct_diff = 100 * (pct_K - pct_M)) %>% 
  ggplot() +
  geom_point(aes(x = Wiek, y = pct_diff, 
                 colour = pct_diff >= 0), size = 5) +
  geom_segment(aes(x = Wiek, 
                   xend = Wiek, 
                   y = 0, 
                   yend = pct_diff,
                   colour = pct_diff >= 0), 
               size = 1) +
  geom_hline(yintercept = 0, colour = "gray") +
  scale_colour_brewer(palette = "Set3") +
  theme_minimal(base_family = "serif", base_size = 10) +
  # scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "W każdym ze źródeł najczęściej wskazywano wykształcenie informatyczne",
       subtitle = "w drugiej kolejności wskazania dotyczą kierunków matematycznych/statystycznych\nlub medycznych/przyrodniczych",
       caption = 'Połączono dane z ankiet wymienionych na osi "Źródło"',
       y = "Różnica w udziale kategorii wiekowej [% kobiet - % mężczyzn]")

# płeć poziom jw


# płeć kierunek
# wiek poziom
# wiek kierunek