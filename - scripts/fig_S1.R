#------------------------------------------------------#
# 3. Methods: Survey results                           #
# 22-11-2023                                           #
# JPA                                                  #
#------------------------------------------------------#

# 0. Setup -----

## 0.1 Packages ----

list_of_packages <- c("tidyverse", "extrafont")
lapply(list_of_packages, library, character.only = TRUE)


## 0.2 Data -----


# 1.Figure

ocareer <- c("Full professor","Associate professor","Assistant professor","PhD student","Leadership","Other")

fig_survey <- srg %>%
  mutate(career_label_class = case_when(
    career_label == "PhD student" ~ "PhD student",
    career_label == "other" ~ "Other",
    career_label == "leadership" ~ "Leadership",
    career_label == "full professor" ~ "Full professor",
    career_label == "associate professor" ~ "Associate professor",
    career_label == "assistant professor" ~ "Assistant professor"
  )) %>%
  complete(career_label_class, current_gender_class, fill = list(p = 0)) %>%
  # Order career_label manually
  mutate(career_label_class = factor(career_label_class, levels = ocareer)) %>%
  ggplot(aes(x = career_label_class, group = current_gender_class, fill = current_gender_class, y = p)) +
  geom_bar(stat = "identity", position="dodge") +
  theme_ebm_hbar(10) + 
  coord_flip() +
  scale_x_discrete("Career stage") + 
  scale_y_continuous("Share of respondents, %",
                     limits = c(0, 25),
                     expand = c(0, 0)) +
  scale_fill_manual("Reported current gender",
                    values = c(colour_scale_desat[5], colour_scale_desat[1], colour_scale_desat[2])) +
  theme(legend.position = "bottom",
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "lines"))


ggsave("- figures/fig_survey.tiff", fig_survey, width = 5.5, height = 3.5, units = "in", compression = "lzw", dpi = 300)
ggsave("- figures/fig_survey.pdf", fig_survey, width = 5.5, height = 3.5, units = "in", device = cairo_pdf)
