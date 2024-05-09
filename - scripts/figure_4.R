#------------------------------------------------------#
# 1. Descriptives: Funder comparison and success rates #
# 15-11-2023                                           #
# EBM                                                  #
#------------------------------------------------------#


# 0. Setup -----

## 0.1 Packages ----

list_of_packages <- c("tidyverse", "tidybayes", "ggbeeswarm", "patchwork", "rio", "extrafont")
lapply(list_of_packages, library, character.only = TRUE)


## 0.2 Data -----

grants <- import("- data/grants_data_final_2023.csv")

grants <- grants %>% 
  mutate(career_clustered = case_when(career_stage_category == "Professor" ~ "Late-career",
                                      career_stage_category == "Associate Professor" ~ "Mid-career",
                                      career_stage_category %in% c("Assistant Professor", 
                                                                   "Postdoc", 
                                                                   "PhD Student", 
                                                                   "Medical Doctor") ~ "Early-career",
                                      TRUE ~ "Other"))

rates <- import("- data/success_rates.xlsx") %>% 
  mutate(success_rate = case_when(funder == "danmarks frie forskningsfond" ~ 13,
                                  TRUE ~ success_rate))
# 1. Funding composition by funder -----

df_total <- grants %>% 
  group_by(funder) %>% 
  summarise(total_grants = n(),
            pct_pi_women = mean(gender == "f"))

df <- grants %>% 
  group_by(funder, gender_category) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            sum_award = sum(amount_awarded)) %>% 
  ungroup() %>% 
  group_by(funder) %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award)) %>% 
  pivot_longer(cols = - c(funder, gender_category), names_to = "measure", values_to = "pct") %>% 
  pivot_wider(id_cols = c(funder, measure), names_from = gender_category, values_from = pct, values_fill = 0) %>% 
  filter(measure %in% c("pct_grants", "pct_pi", "pct_award")) %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of grants",
                             measure == "pct_pi" ~ "No. of grantees",
                             TRUE ~ "Grant amounts")) %>% 
  mutate(measure = factor(measure, levels = c("No. of grantees", "No. of grants", "Grant amounts"))) %>% 
  left_join(df_total, by = "funder") %>% 
  mutate(funder = case_when(funder == "novo nordisk" ~ "Novo Nordisk Foundation",
                            funder == "gigtforeningen" ~ "Danish Rheumatism Association",
                            funder == "region midt" ~ "Central Denmark Region",
                            funder == "region hovedstaden" ~ "Capital Region of Denmark",
                            funder == "velux fonden" ~ "Velux Foundation",
                            funder == "danish cancer society" ~ "Danish Cancer Society",
                            funder == "helsefonden" ~ "The Health Foundation",
                            funder == "hjerteforeningen" ~ "Danish Heart Association",
                            funder == "carlsbergfondet" ~ "Carlsberg Foundation",
                            funder == "lundbeckfonden" ~ "Lundbeck Foundation",
                            funder == "danmarks frie forskningsfond" ~ "Independent Research Fund Denmark",
                            funder == "villum fonden" ~ "Villum Foundation",
                            funder == "danish national research foundation" ~ "Danish National Research Foundation",
                            funder == "poul due jensen foundation" ~ "Poul Due Jensen Foundation"))


b <- df %>% 
  ggplot(aes(x = Women, y = fct_reorder(funder, pct_pi_women))) +
  geom_point(aes(size = total_grants, fill = Women),
             shape = 21,
             colour = "black") +
  facet_wrap(~ measure) +
  scale_x_continuous(limits = c(0, 1), 
                     expand = c(0,0), 
                     labels = scales::percent_format()) +
  scale_size(guide = "none") +
  scale_fill_gradient(low = "white", high = colour_scale_desat[2],
                      #midpoint = 50,
                      limits = c(0, 1),
                      breaks = seq(0, 1, 0.1),
                      labels = c("0", "", "20", "", "40", "", "60", "", "80", "", "100")) +
  guides(fill = guide_coloursteps(barwidth = 7.5, 
                                  barheight = 0.5, 
                                  title = "% of women",
                                  title.position = "top",
                                  title.theme = element_text(size = 7, family = font_fam, hjust = 0.5),
                                  label.position = "bottom",
                                  #frame.colour = "black",
                                  ticks = FALSE,
                                  ticks.colour = "black")
  ) +
  coord_cartesian(xlim = c(-0.05, 1.05)) +
  labs(x = "", y = NULL) +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "bottom",
        #legend.justification = "left",
        legend.direction = "horizontal",
        legend.margin = margin(grid::unit(0, "cm")),
        legend.text = element_text(size = 8),
        legend.key.height = grid::unit(0.8, "cm"),
        legend.key.width = grid::unit(0.2, "cm"),
        panel.spacing = unit(2, "lines"))

# 2. Success rates

labels <- rates %>% 
  filter(funder %in% c("carlsbergfondet", "region midt", "danish ministry for science and technology", "innovation fund denmark: explorer")) %>% 
  mutate_if(is.numeric, ~ .x/100) %>% 
  mutate(label = c("Ministry for\n Science and Tech.", "Innovation Fund Denmark:\n Explorer", "Central Denmark Region", "Carlsberg Foundation"))

rates <- rates %>% 
  mutate(funder = case_when(funder == "novo nordisk" ~ "Novo Nordisk Foundation",
                            funder == "gigtforeningen" ~ "Danish Rheumatism Association",
                            funder == "region midt" ~ "Central Denmark Region",
                            funder == "region hovedstaden" ~ "Capital Region of Denmark",
                            funder == "velux fonden" ~ "Velux Foundation",
                            funder == "danish cancer society" ~ "Danish Cancer Society",
                            funder == "helsefonden" ~ "The Health Foundation",
                            funder == "hjerteforeningen" ~ "Danish Heart Association",
                            funder == "carlsbergfondet" ~ "Carlsberg Foundation",
                            funder == "lundbeckfonden" ~ "Lundbeck Foundation",
                            funder == "danmarks frie forskningsfond" ~ "Independent Research Fund Denmark",
                            funder == "villum fonden" ~ "Villum Foundation",
                            funder == "danish national research foundation" ~ "Danish National Research Foundation",
                            funder == "innovation fund denmark: explorer" ~ "Innovation Fund Denmark: Explorer grants",
                            funder == "innovation fund denmark: overall" ~ "Innovation Fund Denmark: Overall",
                            funder == "danish ministry for science and technology" ~ "Danish Ministry for Science and Technology", 
                            TRUE ~ funder),
         grant_data = case_when(funder %in% df$funder ~ "Present in grant data",
                                TRUE ~ "Not present in grant data"))

 a <- rates %>% 
  mutate_if(is.numeric, ~ .x/100) %>% 
  ggplot(aes(x = success_rate_women, success_rate_men)) +
  geom_abline(linetype = "dashed") +
  ggrepel::geom_text_repel(data = labels,
            aes(label = label),
            size = 6 * text_scaler,
            min.segment.length = 0.05,
            segment.color = "gray40") +
  geom_point(aes(size = applications), colour = "gray40",
            alpha = 0.6) +
  scale_x_continuous(limits = c(0, 1), 
                     labels = scales::percent_format()) +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent_format()) +
  scale_size(name = "No. of applications:") +
  guides(size = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(x = "Success rate [Women]", y = "Success rate [Men]") +
  #coord_cartesian(clip = "off") +
  theme_ebm_grid(base_size = 7) +
   theme(legend.position = c(0.3, 0.85),
         #legend.justification = "left",
         legend.direction = "horizontal",
         legend.margin = margin(0, 0 , 0, 0, unit = "lines"),
         legend.background = element_blank())
 
 c <- rates %>% 
   mutate(success_rate = success_rate/100) %>% 
   ggplot(aes(x = success_rate, y = fct_reorder(funder, success_rate))) +
   geom_col(aes(fill = grant_data)) +
   geom_text(aes(label = paste0(round(success_rate, 3) * 100, " %")),
             hjust = -0.1,
             size = 7 * text_scaler) +
   scale_fill_manual(values = c("gray80", "gray40")) +
   scale_x_continuous(limits = c(0, 1),
                      breaks = seq(0, 1, 0.5),
                      expand = c(0, 0), 
                      labels = scales::percent_format()) +
   labs(x = "Overall success rate", y = "") +
   theme_ebm_hbar(base_size = 7) +
   theme(legend.position = "none",
         legend.title = element_blank())
 
 
layout <- "
ABB
ABB
CCC
CCC
CCC
"

fig_4 <- c + a + b + plot_layout(design = layout) + plot_annotation(tag_levels = "A") & theme(plot.tag = element_text(face = "bold", size = 10))


ggsave("- figures/-- descriptives/fig_4.tiff", plot = fig_4, width = 7, height = 7, units = "in",  compression = "lzw", dpi = 300)
ggsave("- figures/-- descriptives/fig_4.pdf", plot = fig_4, width = 7, height = 7, units = "in", device = cairo_pdf)



