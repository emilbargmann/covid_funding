#----------------------------------------------#
# 1. Descriptives: Gender and career stage     #
# 29-09-2023                                   #
# EBM                                          #
#----------------------------------------------#


# 0. Setup -----

## 0.1 Packages ----

list_of_packages <- c("tidyverse", "tidybayes", "ggbeeswarm", "patchwork", "rio", "extrafont",
                      "ggpubr")
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

# 1. Figure 1 and Table 1: Gender and career stage differences in funding ----

## 1.1 Gender differences ----

# Table of descriptives

table <- grants %>% 
  group_by(gender_category) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded, na.rm = TRUE),
            median_award = median(amount_awarded, na.rm = TRUE),
            sum_award = sum(amount_awarded, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_award = sum_award/sum(sum_award))

total <- grants %>% 
  summarise(gender_category = "Total",
            n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded, na.rm = TRUE),
            median_award = median(amount_awarded, na.rm = TRUE),
            sum_award = sum(amount_awarded, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_award = sum_award/sum(sum_award))


table <- rbind(table, total)

table <- table %>% 
  mutate(pct_grants = paste0(round(pct_grants, 4) * 100, " %"),
         pct_award = paste0(round(pct_award, 4) * 100, " %")
  ) 

export(table, "- tables/table_1.xlsx")

# Figure

fig_1_a <- grants %>% 
  group_by(gender_category) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded),
            sum_award = sum(amount_awarded)) %>% 
  ungroup() %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award)) %>% 
  select(gender_category, pct_grants, pct_pi, pct_award) %>% 
  pivot_longer(cols = -gender_category, names_to = "measure", values_to = "pct") %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of grants",
                             measure == "pct_pi" ~ "No. of grantees",
                             TRUE ~ "Grant amounts")) %>% 
  mutate(measure = factor(measure, levels = c("Grant amounts", "No. of grants", "No. of grantees")),
         gender_category = factor(gender_category, levels = c("Other", "Men", "Women"))) %>% 
  ggplot(aes(x = pct, y = measure, fill = gender_category)) +

  geom_col(colour = "white",
           linewidth = 1,
           width = 0.75) +
  geom_text(aes(label = ifelse(gender_category %in% c("Women", "Men"), paste0(round(pct, 3)*100, "%"), "")), 
            position = position_stack(vjust = 0.5),
            size = 7 * text_scaler) +
  geom_text(aes(label = ifelse(gender_category == "Other", paste0(round(pct, 3)*100, "%"), ""),
                x = 1.05), 
            #position = position_stack(vjust = 0),
            size = 7 * text_scaler,
            fontface = "bold",
            colour = colour_scale_desat[1]) +
  geom_text(data = . %>% filter(measure == "No. of grantees"), 
            aes(label = gender_category,
                y = 3.55,
                colour = gender_category), 
            fontface = "bold", 
            size = 7 * text_scaler,
            position = position_stack(vjust = 0.5)) +
  labs(x = "", y = "", fill = "") +
  scale_fill_manual(values = c(colour_scale_desat[1], colour_scale_desat[5], colour_scale_desat[2])) +
  scale_colour_manual(values = c(colour_scale_desat[1], colour_scale_desat[5], colour_scale_desat[2])) +
  scale_x_continuous(expand = c(0, 0), labels = scales::percent_format()) +
  coord_cartesian(clip = "off", xlim = c(0, 1)) +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "none")

#ggsave("- figures/-- descriptives/fig_1_a.tiff", plot = fig_1_a, width = 4, height = 2.5, units = "in",  compression = "lzw", dpi = 300)
#ggsave("- figures/-- descriptives/fig_1_a.pdf", plot = fig_1_a, width = 4, height = 2.5, units = "in", dpi = 300, device = cairo_pdf)

## 1.2 Career stage differences ----

sorting <- c("Technical/administrative staff", "Management", "Medical Doctor", 
             "PhD Student", "Other academic personnel", "Postdoc", 
             "Assistant Professor", "Associate Professor", "Professor")

df <- grants %>% 
  group_by(career_stage_category) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded),
            sum_award = sum(amount_awarded)) %>% 
  ungroup() %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award),
         ratio = sum_award/n_pi,
         career_stage_category = factor(career_stage_category, levels = sorting)
         ) %>% 
  select(career_stage_category, pct_grants, pct_pi, pct_award, ratio) %>% 
  pivot_longer(cols = -career_stage_category, names_to = "measure", values_to = "pct") %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of grants",
                             measure == "pct_pi" ~ "No. of grantees",
                             measure == "ratio" ~ "Avg. grant amount",
                             TRUE ~ "Grant amounts")) %>% 
  mutate(measure = factor(measure, levels = c("No. of grantees", "No. of grants", 
                                              "Grant amounts", "Avg. grant amount")
                          )
         )

fig_1_b <- df %>% 

  ggplot(aes(x = pct, y = career_stage_category)) +
  geom_col(fill = "gray40") +
  geom_text(data = filter(df, measure != "Avg. grant amount"),
            aes(label = paste0(round(pct, 3)*100, "%")), hjust = -0.1,
            size = 7 * text_scaler) +
  geom_text(data = filter(df, measure == "Avg. grant amount"),
            aes(label = paste0(round(pct/1000000, 1), "m")), hjust = -0.1,
            size = 7 * text_scaler) +
  labs(x = "", y = "") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 0.75),
                     breaks = seq(0, 0.75, 0.25),
                     labels = scales::percent_format()) +
  facet_wrap_custom(~ measure, ncol = 4, scales = "free_x",
                    scale_overrides = list(
                      scale_override(4, scale_x_continuous(expand = c(0, 0), 
                                                           limits = c(0, 20000000),
                                                           breaks = seq(0, 20000000, 5000000),
                                                           labels = c("0", "5m", "10m", "15m", "20m"))
                                     )
                      )
                    )  +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "none",
        panel.spacing = unit(1, "lines"),
        axis.ticks.y = element_blank())

#ggsave("- figures/-- descriptives/fig_1_b.tiff", plot = fig_1_b, width = 6.5, height = 3, units = "in",  compression = "lzw", dpi = 300)
#ggsave("- figures/-- descriptives/fig_1_b.pdf", plot = fig_1_b, width = 6.5, height = 3, units = "in", dpi = 300, device = cairo_pdf)


## 1.3 Gender x career stage differences ----

fig_1_c_alt <- grants %>% 
  group_by(gender_category, career_stage_category) %>% 
  summarise(n = n_distinct(pi_id),
            n_grants = n_distinct(grant_id),
            amount = sum(amount_awarded)) %>% 
  group_by(career_stage_category) %>% 
  mutate(pct_pi = (n/sum(n)) * 100,
         pct_grants = (n_grants/sum(n_grants)) * 100,
         pct_amount = (amount/sum(amount)) *100,
         career_stage_category = factor(career_stage_category, levels = sorting),
         facet = factor(ifelse(career_stage_category %in% c("Technical/administrative staff", "Management"), "Research", "Non-research"))) %>% 
  filter(gender_category == "Women") %>%
  select(gender_category, career_stage_category, facet, pct_pi:pct_amount) %>%
  pivot_longer(cols = - c("gender_category", "career_stage_category", "facet"),
               names_to = "measure",
               values_to = "pct") %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of\ngrants",
                             measure == "pct_pi" ~ "No. of\ngrantees",
                             TRUE ~ "Grant\namounts")) %>% 
  mutate(measure = factor(measure, levels = c("Grant\namounts", "No. of\ngrants", "No. of\ngrantees"))) %>% 
  ggplot(aes(x = measure, y = career_stage_category)) +
    geom_tile(aes(fill = pct),
              width = 0.9,
              height = 0.9) + 
    geom_text(aes(label = round(pct, 1)),
              size = 7 * text_scaler) +
    geom_hline(yintercept = 2.5) +
    scale_fill_gradient(low = "white", high = colour_scale_desat[2],
                         #midpoint = 50,
                         limits = c(0, 100),
                         breaks = seq(0, 100, 10),
                         labels = c("0", "", "20", "", "40", "", "60", "", "80", "", "100")) +
    guides(fill = guide_coloursteps(barwidth = 5, 
                                    barheight = 0.5, 
                                    title = "% of women",
                                    title.position = "top",
                                    title.theme = element_text(size = 7, family = font_fam, hjust = 0.5),
                                    label.position = "bottom",
                                    #frame.colour = "black",
                                    ticks = FALSE,
                                    ticks.colour = "black")
            ) +
  annotate(geom = "text", y = 1.5, x = 3.9, 
           label = "Non\nacademic\nstaff",
           angle = -90,
           size = 7 * text_scaler,
           fontface = "bold") +
  annotate(geom = "text", y = 5.5, x = 3.9, 
           label = "Academic\nstaff",
           size = 7 * text_scaler,
           angle = -90,
           fontface = "bold") +
    labs(x = "", y = "") +
  coord_cartesian(xlim = c(1, 3.9)) +
    theme_ebm_minimal(base_size = 7) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        legend.justification = c(0.3, 1),
        legend.direction = "horizontal",
        legend.margin = margin(grid::unit(0, "cm")),
        legend.text = element_text(size = 6),
        legend.key.height = grid::unit(0.6, "cm"),
        legend.key.width = grid::unit(0.1, "cm"))

#ggsave("- figures/-- descriptives/fig_1_c_alt.tiff", plot = fig_1_c_alt, width = 5, height = 4.5, units = "in",  compression = "lzw", dpi = 300)
#ggsave("- figures/-- descriptives/fig_1_c_alt.pdf", plot = fig_1_c_alt, width = 5, height = 4.5, units = "in", dpi = 300, device = cairo_pdf)


layout <- "
AAABBB
AAABBB
CCCCCC
CCCCCC
CCCCCC
"

fig_1 <- fig_1_a + fig_1_c_alt + fig_1_b + 
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = "A") & 
  theme(plot.tag = element_text(face = "bold", size = 10),
        plot.margin = margin(t = 0.15, r = 0.15, b = 0.15, l = 0.15, unit = "cm"))

ggsave("- figures/-- descriptives/fig_1.tiff", plot = fig_1, width = 7, height = 8, units = "in",  compression = "lzw", dpi = 300)
ggsave("- figures/-- descriptives/fig_1.pdf", plot = fig_1, width = 7, height = 8, units = "in", device = cairo_pdf)

# 2. Gender and career differences across covid and non-covid funding ----

## 2.1 Gender differences -----

  fig_3_a <- grants %>%
    mutate(covid_related = ifelse(covid_related > 0, "Covid related", "Not Covid related")) %>%
    group_by(gender_category, covid_related) %>%   
    summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded),
            sum_award = sum(amount_awarded)) %>% 
    group_by(covid_related) %>% 
    mutate(pct_grants = n_grants/sum(n_grants),
           pct_pi = n_pi/sum(n_pi),
           pct_award = sum_award/sum(sum_award)) %>% 
    select(gender_category, covid_related, pct_grants, pct_pi, pct_award) %>% 
    pivot_longer(cols = pct_grants:pct_award, names_to = "measure", values_to = "pct") %>% 
    mutate(measure = case_when(measure == "pct_grants" ~ "No. of grants",
                               measure == "pct_pi" ~ "No. of grantees",
                               TRUE ~ "Grant amounts")) %>% 
    mutate(measure = factor(measure, levels = c("Grant amounts", "No. of grants", "No. of grantees")),
           gender_category = factor(gender_category, levels = c("Other", "Men", "Women"))) %>% 
    
    ggplot(aes(x = pct, y = measure, fill = gender_category)) +
    geom_col(colour = "white",
             linewidth = 1,
             width = 0.75) +
    geom_text(aes(label = ifelse(gender_category %in% c("Women", "Men"), paste0(round(pct, 3)*100, "%"), "")), 
              position = position_stack(vjust = 0.5),
              size = 7 * text_scaler) +
    geom_text(aes(label = ifelse(gender_category == "Other", paste0(round(pct, 3)*100, "%"), ""),
                  x = 1.075), 
              #position = position_stack(vjust = 0),
              size = 7 * text_scaler,
              fontface = "bold",
              colour = colour_scale_desat[1]) +
    geom_text(data = . %>% filter(measure == "No. of grantees" & covid_related == "Not Covid related"), 
              aes(label = gender_category,
                  y = 3.55,
                  colour = gender_category), 
              fontface = "bold", 
              size = 7 * text_scaler,
              position = position_stack(vjust = 0.5)) +
    facet_wrap(~ covid_related) +
    labs(x = "", y = "", fill = "") +
    scale_fill_manual(values = c(colour_scale_desat[1], colour_scale_desat[5], colour_scale_desat[2])) +
    scale_colour_manual(values = c(colour_scale_desat[1], colour_scale_desat[5], colour_scale_desat[2])) +
    scale_x_continuous(expand = c(0, 0), labels = scales::percent_format()) +
    coord_cartesian(clip = "off", xlim = c(0, 1)) +
    theme_ebm_hbar(base_size = 8) +
    theme(legend.position = "none",
          #plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
          panel.spacing = unit(2, "lines"),
          strip.text = element_text(vjust = 5, margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"))
          )

## 2.2 Career stage differences ----

ann_arrow <- tibble(measure = factor("No. of grantees"), 
                    career_stage_category = factor("Assistant Professor"),
                    label = "Covid\nrelated")


fig_3_b <- grants %>% 
  mutate(covid_related = ifelse(covid_related > 0, "Covid related", "Not Covid related")) %>%
  group_by(career_stage_category, covid_related) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded),
            sum_award = sum(amount_awarded)) %>% 
  group_by(covid_related) %>% 
  
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award),
         ratio = sum_award/n_pi,
         career_stage_category = factor(career_stage_category, levels = sorting)
  ) %>% 
  select(career_stage_category, pct_grants, pct_pi, pct_award, ratio) %>% 
  pivot_longer(cols = - c(career_stage_category, covid_related), names_to = "measure", values_to = "pct") %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of grants",
                             measure == "pct_pi" ~ "No. of grantees",
                             measure == "ratio" ~ "Avg. grant amounts",
                             TRUE ~ "Grant amounts")) %>% 
  mutate(measure = factor(measure, levels = c("No. of grantees", "No. of grants", 
                                              "Grant amounts", "Avg. grant amounts")
                          )
         ) %>% 
  
  ggplot(aes(x = pct, y = career_stage_category)) +
  geom_col(data = . %>% filter(covid_related == "Not Covid related"),
           fill = "gray60") +
  geom_col(data = . %>% filter(covid_related == "Covid related"),
           alpha = 0.6,
           colour = "black",
           fill = NA) +
  #geom_hline(yintercept = 2.5) +
  facet_wrap_custom(~ measure, ncol = 4, scales = "free_x",
                    scale_overrides = list(
                      scale_override(4, scale_x_continuous(expand = c(0, 0), 
                                                           limits = c(0, 20000000),
                                                           breaks = seq(0, 20000000, 5000000),
                                                           labels = c("0", "5M", "10M", "15M", "20M"))
                      )
                    )
  )  +
  geom_text(data = . %>% filter(measure != "Avg. grant amounts" & covid_related == "Covid related"),
            aes(label = paste0(round(pct, 3)*100, "%")),
            fontface = "bold", 
            hjust = -0.1,
            size = 7 * text_scaler,
            vjust = -0.2) +
  geom_text(data = . %>% filter(measure != "Avg. grant amounts" & covid_related != "Covid related"),
            aes(label = paste0(round(pct, 3)*100, "%")),
            hjust = -0.1,
            size = 7 * text_scaler,
            vjust = 1) +

  geom_text(data = . %>% filter(measure == "Avg. grant amounts" & covid_related == "Covid related"),
            aes(label = paste0(round(pct/1000000, 1), "M")),
            fontface = "bold", 
            hjust = -0.1,
            size = 7 * text_scaler,
            vjust = -0.2) +
  geom_text(data = . %>% filter(measure == "Avg. grant amounts" & covid_related != "Covid related"),
            aes(label = paste0(round(pct/1000000, 1), "M")),
            hjust = -0.1,
            size = 7 * text_scaler,
            vjust = 1) +
  
  geom_text(data = ann_arrow, aes(x = 0.80, label = label),
            size = 7 * text_scaler,
            fontface = "bold") +
  
  geom_curve(data = ann_arrow, aes(x = 0.80, y = 7.4, xend = 0.65, yend = 8.15),
            arrow = arrow(length = unit(0.03, "npc"), type = "closed")) +
  labs(x = "", y = "", fill = "") +
  scale_x_continuous(expand = c(0, 0), 
                     limits = c(0, 1),
                     breaks = seq(0, 1, 0.25),
                     labels = scales::percent_format()) +
  theme_ebm_hbar(base_size = 7) +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.text = element_text(size = 8),
        panel.spacing = unit(2, "lines"),
        axis.ticks.y = element_blank())

## 2.3 Table ----

fig_3_c <- grants %>% 
  mutate(covid_related = ifelse(covid_related > 0, "Covid related", "Not Covid related")) %>%
  group_by(covid_related) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            sum_award = sum(amount_awarded)) %>%
  mutate(sum_award = round(sum_award/1000000, 1)) %>% 
  pivot_longer(cols = - covid_related) %>% 
  #pivot_wider(id_cols = name, names_from = covid_related, values_from = value) %>% 
  mutate(name = case_when(name == "n_grants" ~ "No. of grants",
                          name == "n_pi" ~ "No. of grantees",
                          TRUE ~ "Grant amounts"),
         name = factor(name, levels = c("Grant amounts", "No. of grants", "No. of grantees"))
         ) %>% 
  
  ggplot(aes(x = covid_related, y = name)) +
  #geom_tile(fill = "gray75",
  #          width = 0.9,
  #          height = 0.9) +
  geom_text(aes(label = ifelse(name == "Grant amounts", paste0(value, "M"), value)),
            size = 7 * text_scaler) +
  geom_vline(xintercept = 1.5) +
  scale_x_discrete(labels = c("Covid\nrelated", "Not Covid\nrelated")) +
  labs(x = "",
       y = "") +
  theme_ebm_hbar(base_size = 7) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(face = c("bold", "plain")))

  

layout <- "
AAAB
CCCC
CCCC
"


fig_3 <- fig_3_a + fig_3_c + fig_3_b + plot_annotation(tag_levels = "A") + plot_layout(design = layout) & 
  theme(plot.tag = element_text(face = "bold", size = 10),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "lines"))

ggsave("- figures/-- descriptives/fig_3.tiff", plot = fig_3, width = 7, height = 8, units = "in",  compression = "lzw", dpi = 300)
ggsave("- figures/-- descriptives/fig_3.pdf", plot = fig_3, width = 7, height = 8, units = "in", device = cairo_pdf)



