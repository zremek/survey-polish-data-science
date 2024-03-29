library(tidyverse)
library(scales)
library(forcats)
library(ggtext)

# save.image("survey_phd_image.RData")
# load("survey_phd_image.RData")

# 4_3	 Płeć uczestników według ankiet związanych z polskim światem społecznym data science
# 4_4	 Miesięczne zarobki brutto data scientistów w Polsce w podziale na liczbę lat doświadczenia

ggplot2::theme_set(theme_minimal(base_size = 8))

# 4_3 płeć wg ankiet #############
ggplot(demogr) +
  geom_bar(aes(Źródło, fill = Płeć), position = "fill", colour = "black", width = 0.7) +
  scale_fill_brewer(palette = "Set3", 
                    name = "Płeć:") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = c("Kaggle 2017 [<i>n</i> = 130]",
                              "Kaggle 2018 [<i>n</i> = 252]", 
                              "Stack Overflow 2018 [<i>n</i> = 121]", 
                              "Stack Overflow 2019 [<i>n</i> = 111]", 
                              "WhyR? 2017 [<i>n</i> = 202]", 
                              "PyData 2018 [<i>n</i> = 284]")) +
  labs(title = "Na jedną kobietę przypada od 2 (WhyR? 2017)\ndo 13 mężczyzn (Stack Overflow 2018)",
       caption = 'Połączono dane z ankiet wymienionych na osi „Źródło”',
       x = "Źródło",
       y = NULL) +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.box.just = "left", 
        axis.text.y = element_markdown()) + 
  coord_flip() ### odd that markdown with coord_flip works only when axis.text.y (not .x)

# ggsave("4_3_plec_wg_ankiet.jpg", units = "cm", width = 12.6, height = 6)

# 4_4 zarobki do doświadczenia SO 

ggplot(so_bind_18_19_salary %>%
         filter(!is.na(salary_clean_month_pln))) +
  geom_boxplot(aes(x = doswiadczenie, y = salary_clean_month_pln, fill = zrodlo),
               position = position_dodge(1)) +
  geom_dotplot(aes(x = doswiadczenie, y = salary_clean_month_pln, fill = zrodlo),
               binaxis = 'y', stackdir = "center",
               position = position_dodge(1),
               alpha = 1/3, dotsize = 2,
               binwidth = 500, show.legend = FALSE) +
  scale_y_continuous(labels = scales::comma_format(big.mark = " ",
                                                   decimal.mark = ",")) +
  coord_flip() +
  theme(legend.position = "bottom", legend.text = element_markdown()) +
  scale_fill_manual(values = c("#80b1d3", "#fdb462"),
                    name = "Źródło:", 
                    labels = c(
                      "Stack Overflow 2018 [<i>n</i> ważnych = 52]",
                      "Stack Overflow 2019 [<i>n</i> ważnych = 73]"
                    )) +
  labs(title = "Zarobki rosną wraz z długością doświadczenia zawodowego",
       y = "Miesięczne zarobki brutto na pełen etat [PLN]",
       x = "Doświadczenie w zawodowym pisaniu kodu")

# ggsave("4_4_zarobki_boxplot.jpg", units = "cm", width = 12.6, height = 10)

