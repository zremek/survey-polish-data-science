library(tidyverse)
library(scales)
library(cowplot)

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
  # theme(axis.text.x = element_text(angle = 25)) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "W każdym ze źródeł najczęściej wskazywano wiek od 25 do 34 lat",
       caption = 'Połączono dane z ankiet wymienionych na osi "Źródło"',
       x = "Źródło",
       y = NULL)

# png("wiek.png", width = 160, height = 80, units = "mm", res = 300)
# plot(wiek) # Rys. 8. in chapter 5.1.
# dev.off()

# demogr %>% group_by(poziom_wykształcenia) %>% summarise(n = n()) %>% View()

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
  # theme(axis.text.x = element_text(angle = 20)) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "W każdym ze źródeł najczęściej wskazywano wykształcenie\nmagisterskie, ale udział osób z doktoratem, jak i wykszt. średnim\nsięga 1/6 wskazań",
       caption = 'Połączono dane z ankiet wymienionych na osi "Źródło"',
       x = "Źródło",
       y = NULL,
       fill = "Poziom wykształcenia")

# png("poziom.png", width = 160, height = 80, units = "mm", res = 300)
# plot(poziom) # Rys. 9. in chapter 5.1.
# dev.off()

# demogr %>% group_by(kierunek_wykształcenia) %>% summarise(n = n()) %>% View()

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

prop.table(table(demogr$Źródło, demogr$`Kierunek wykształcenia`), 1)

kierunek <- ggplot(demogr %>%
         filter(Źródło != "PyData 2018 [n = 284]" & Źródło != "WhyR? 2017 [n = 202]")) +
  geom_bar(aes(Źródło, fill = `Kierunek wykształcenia`),
           position = "fill", colour = "black", width = 0.7) +
  theme_minimal(base_family = "serif", base_size = 10) +
  # theme(axis.text.x = element_text(angle = 20)) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "W każdym ze źródeł najczęściej wskazywano wykształcenie\ninformatyczne, w drugiej kolejności wskazywano wykszt.\nmatematyczne/statystyczne lub medyczne/przyrodnicze",
       caption = 'Połączono dane z ankiet wymienionych na osi "Źródło"',
       x = "Źródło",
       y = NULL,
       fill = "Kierunek wykształcenia")

# png("kierunek.png", width = 160, height = 95, units = "mm", res = 300)
# plot(kierunek) # Rys. 8. in chapter 5.1.
# dev.off()

##### to do

# demogr %>% mutate(older_than_34 = Wiek %in% c("35 - 44", "45 - 54", "55 - 69"))
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
plec_wiek_df <- demogr %>% 
  filter(Płeć %in% c("Kobieta", "Mężczyzna"),
         Wiek != "Brak/odmowa odp.", 
         Źródło != "WhyR? 2017 [n = 202]") %>%
  group_by(Wiek) %>% 
  summarise(n = n(), 
            Mężczyźni = sum(Płeć == "Mężczyzna"),
            Kobiety = sum(Płeć == "Kobieta")) %>% 
  mutate(pct_M = Mężczyźni / sum(Mężczyźni),
         pct_K = Kobiety / sum(Kobiety),
         pct_diff = pct_K - pct_M)

plec_wiek_lolipop <- plec_wiek_df %>% 
  ggplot() +
  geom_point(aes(x = fct_rev(Wiek), y = pct_diff, 
                 colour = pct_diff >= 0), size = 5) +
  geom_segment(aes(x = Wiek, 
                   xend = Wiek, 
                   y = 0, 
                   yend = pct_diff,
                   colour = pct_diff >= 0), 
               size = 1) +
  geom_hline(yintercept = 0, colour = "gray") +
  scale_colour_manual(values = c("#80b1d3", "#fdb462")) +
  theme_minimal(base_family = "serif", base_size = 10) +
  guides(colour = "none") +
  coord_flip() +
  labs(title = '11B',
       caption = 'Połączono dane z ankiet Kaggle 2017 i 2018 oraz Stack Overflow 2018 i 2019',
       x = 'Wiek\n[wykluczono kategorię "Brak/odmowa odp."]',
       y = "Różnica w udziale kategorii wiekowej dla wybranych płci [frakcja kobiet - frakcja mężczyzn]")

plec_wiek_col <- plec_wiek_df %>% 
  select(Wiek, pct_M, pct_K) %>% 
  pivot_longer(cols = pct_M:pct_K, 
               names_to = 'plec',
               values_to = 'pct') %>% 
  ggplot(aes(x = Wiek, y = pct, fill = plec)) + 
  geom_col(position = 'dodge', colour = "black") +
  labs(title = '11A', 
       subtitle = 'Udział osób w wieku 18 - 24 jest o ponad 6 pp. wyższy wśród kobiet niż wśród mężczyzn.\nBrak kobiet w grupach wiekowych: "mniej niż 18 lat" i "55 - 69"',
       x = 'Wiek [wykluczono kategorię "Brak/odmowa odp."]',
       y = "Frakcja [każdy Wymiar = 1]") + 
  scale_fill_brewer(palette = "Set3", name = "Wymiar:",
                    labels = c('Kobiety [n = 71]', 'Mężczyźni [n = 487]')) +
  theme_minimal(base_family = "serif", base_size = 10) +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.box.just = "left")

plec_wiek <- plot_grid(plot_grid(plec_wiek_col,
                               plec_wiek_lolipop,
                               nrow = 2,
                               rel_heights = c(2, 1)))

# png("plec_wiek.png", width = 160, height = 180, units = "mm", res = 300)
# plot(plec_wiek) # Rys. 11. in chapter 5.1.
# dev.off()

plec_poziom_df <- demogr %>% 
  filter(Płeć %in% c("Kobieta", "Mężczyzna"),
         `Poziom wykształcenia` != "Brak/odmowa odp.", 
         Źródło != "WhyR? 2017 [n = 202]") %>%
  group_by(`Poziom wykształcenia`) %>% 
  summarise(n = n(), 
            Mężczyźni = sum(Płeć == "Mężczyzna"),
            Kobiety = sum(Płeć == "Kobieta")) %>% 
  mutate(pct_M = Mężczyźni / sum(Mężczyźni),
         pct_K = Kobiety / sum(Kobiety),
         pct_diff = pct_K - pct_M)

plec_poziom_lolipop <- plec_poziom_df %>% 
  ggplot() +
  geom_point(aes(x = fct_rev(`Poziom wykształcenia`), y = pct_diff, 
                 colour = pct_diff >= 0), size = 5) +
  geom_segment(aes(x = `Poziom wykształcenia`, 
                   xend = `Poziom wykształcenia`, 
                   y = 0, 
                   yend = pct_diff,
                   colour = pct_diff >= 0), 
               size = 1) +
  geom_hline(yintercept = 0, colour = "gray") +
  scale_colour_manual(values = c("#80b1d3", "#fdb462")) +
  theme_minimal(base_family = "serif", base_size = 10) +
  guides(colour = "none") +
  coord_flip() +
  labs(title = "12B" ,
       caption = 'Połączono dane z ankiet Kaggle 2017 i 2018 oraz Stack Overflow 2018 i 2019',
       x = 'Poziom wykształcenia\n[wykluczono kategorię "Brak/odmowa odp."]',
       y = "Różnica w udziale poziomu wykształcenia dla wybranych płci\n[frakcja kobiet - frakcja mężczyzn]")

plec_poziom_col <- plec_poziom_df %>% 
  select(`Poziom wykształcenia`, pct_M, pct_K) %>% 
  pivot_longer(cols = pct_M:pct_K, 
               names_to = 'plec',
               values_to = 'pct') %>% 
  ggplot(aes(x = `Poziom wykształcenia`, y = pct, fill = plec)) + 
  geom_col(position = 'dodge', colour = "black") +
  labs(title = '12A', 
       subtitle = 'Udział osób z wykształceniem magisterskim jest o ponad 9 pp. wyższy wśród kobiet\nniż wśród mężczyzn. Brak kobiet z wykształceniem podstawowym',
       x = 'Poziom wykształcenia [wykluczono kategorię "Brak/odmowa odp."]',
       y = "Frakcja [każdy Wymiar = 1]") + 
  scale_fill_brewer(palette = "Set3", name = "Wymiar:",
                    labels = c('Kobiety [n = 71]', 'Mężczyźni [n = 487]')) +
  theme_minimal(base_family = "serif", base_size = 10) +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.box.just = "left")

plec_poziom <- plot_grid(plot_grid(plec_poziom_col,
                                 plec_poziom_lolipop,
                                 nrow = 2,
                                 rel_heights = c(2, 1)))
# 
# png("plec_poziom.png", width = 160, height = 180, units = "mm", res = 300)
# plot(plec_poziom) # Rys. 12. in chapter 5.1.
# dev.off()

# exploration
demogr %>% filter(Płeć != "Brak/odmowa odp.", 
                  Wiek != "Brak/odmowa odp.", 
                  `Poziom wykształcenia` != "Brak/odmowa odp.", 
                  `Kierunek wykształcenia` != "Brak/odmowa odp.") %>%
  group_by(Płeć, Wiek, `Poziom wykształcenia`, `Kierunek wykształcenia`) %>%
  summarise(n = n()) %>% ungroup() %>% mutate(pct = n / sum(n)) %>% 
  top_n(n = 5, wt = n)
 
demogr %>% 
  filter(`Kierunek wykształcenia` != "Brak/odmowa odp.") %>%
  mutate(older_than_44 = Wiek %in% c("45 - 54", "55 - 69")) %>%  
  group_by(`Kierunek wykształcenia`) %>% 
  summarise(n = n(), 
            do_44 = sum(older_than_44 == FALSE),
            powyzej_44 = sum(older_than_44 == TRUE)) %>% 
  mutate(pct_do_44 = do_44 / sum(do_44),
         pct_powyzej_44 = powyzej_44 / sum(powyzej_44),
         pct_diff = 100 * (pct_do_44 - pct_powyzej_44)) %>% 
  ggplot() +
  geom_point(aes(x = fct_rev(`Kierunek wykształcenia`), y = pct_diff, 
                 colour = pct_diff >= 0), size = 5) +
  geom_segment(aes(x = `Kierunek wykształcenia`, 
                   xend = `Kierunek wykształcenia`, 
                   y = 0, 
                   yend = pct_diff,
                   colour = pct_diff >= 0), 
               size = 1) +
  geom_hline(yintercept = 0, colour = "gray") +
  scale_colour_manual(values = c("#80b1d3", "#fdb462")) +
  theme_minimal(base_family = "serif", base_size = 10) +
  guides(colour = "none") +
  coord_flip() 
  # labs(title = "Udział osób z wykształceniem magisterskim jest\no ponad 9 pp. wyższy wśród kobiet niż wśród mężczyzn",
  #      subtitle = 'brak kobiet z wykształceniem podstawowym' ,
  #      caption = 'Połączono dane z ankiet Kaggle 2017 i 2018 oraz Stack Overflow 2018 i 2019',
  #      x = 'Poziom wykształcenia\n[wykluczono kategorię "Brak/odmowa odp."]',
  #      y = "Różnica w udziale poziomu wykształcenia dla wybranych płci\n[pp. = % kobiet - % mężczyzn]")


wiek_kier_df <- demogr %>% 
  filter(`Kierunek wykształcenia` != "Brak/odmowa odp.") %>%
  mutate(older_than_34 = Wiek %in% c("35 - 44", "45 - 54", "55 - 69")) %>%  
  group_by(`Kierunek wykształcenia`) %>% 
  summarise(n = n(), 
            do_34 = sum(older_than_34 == FALSE),
            powyzej_34 = sum(older_than_34 == TRUE)) %>% 
  mutate(pct_do_34 = do_34 / sum(do_34),
         pct_powyzej_34 = powyzej_34 / sum(powyzej_34),
         pct_diff = pct_do_34 - pct_powyzej_34) 

wiek_kier_lolipop <- wiek_kier_df %>% 
  ggplot() +
  geom_point(aes(x = fct_rev(`Kierunek wykształcenia`), y = pct_diff, 
                 colour = pct_diff >= 0), size = 5) +
  geom_segment(aes(x = `Kierunek wykształcenia`, 
                   xend = `Kierunek wykształcenia`, 
                   y = 0, 
                   yend = pct_diff,
                   colour = pct_diff >= 0), 
               size = 1) +
  geom_hline(yintercept = 0, colour = "gray") +
  scale_colour_manual(values = c("#80b1d3", "#fdb462")) +
  theme_minimal(base_family = "serif", base_size = 10) +
  guides(colour = "none") +
  coord_flip() +
labs(title = "13B" ,
     caption = 'Połączono dane z ankiet Kaggle 2017 i 2018 oraz Stack Overflow 2018 i 2019',
     x = 'Kierunek wykształcenia\n[wykluczono kategorię "Brak/odmowa odp."]',
     y = "Różnica w udziale kierunku wykształcenia dla grup wiekowych\n[frakcja do 34 r.ż. - frakcja od 35 r.ż.]")


wiek_kier_col <- wiek_kier_df %>% 
  select(`Kierunek wykształcenia`, pct_do_34, pct_powyzej_34) %>% 
  pivot_longer(cols = pct_do_34:pct_powyzej_34, 
               names_to = 'wiek_34',
               values_to = 'pct') %>% 
  ggplot(aes(x = `Kierunek wykształcenia`, y = pct, fill = wiek_34)) + 
  geom_col(position = 'dodge', colour = "black") +
  labs(title = '12A', 
       subtitle = 'Udział osób z wykształceniem informatycznym jest o prawie 13 pp. wyższy wśród osób do 34 r.ż.\nniż dla osób w wieku 35 lat i starszych. Odwrotnie jest dla wykształcenia medycznego/przyrodnicznego',
       x = 'Kierunek wykształcenia [wykluczono kategorię "Brak/odmowa odp."]',
       y = "Frakcja [każdy Wymiar = 1]") + 
  scale_fill_brewer(palette = "Set3", name = "Wymiar:",
                    labels = c('do 34 r.ż. [n = 425]', 'od 35 r.ż. [n = 114]')) +
  theme_minimal(base_family = "serif", base_size = 10) +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.box.just = "left",
        axis.text.x = element_text(angle = 20))

wiek_kier <- plot_grid(plot_grid(wiek_kier_col,
                                   wiek_kier_lolipop,
                                   nrow = 2,
                                   rel_heights = c(2, 1)))

# png("wiek_kier.png", width = 160, height = 180, units = "mm", res = 300)
# plot(wiek_kier) # Rys. 12. in chapter 5.1.
# dev.off()
