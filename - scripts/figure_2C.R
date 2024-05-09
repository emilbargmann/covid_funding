#----------------------------------------------#
# 1. Descriptives: Historical comparison       #
# 29-09-2023                                   #
# EBM                                          #
#----------------------------------------------#


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

population_gender <- import("- data/dst_gender_fte.csv")

population_career <- import("- data/dst_career_fte.csv")

grants_2008_2016 <- import("- data/grants_data_2008_2016.csv", encoding = "UTF-8")

grants_2008_2016 <- grants_2008_2016 %>% 
  rename(pi_id = person_id, amount_awarded = amount_granted) %>% 
  filter(institution_name != "foreign")


# 1. Gender 

data <- grants %>% 
  filter(funder %in% c("carlsbergfondet", "danish cancer society", 
                       "danish national research foundation", "lundbeckfonden", 
                       "velux fonden", "danmarks frie forskningsfond", "novo nordisk", "villum fonden")) %>% 
  group_by(gender_category) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded),
            sum_award = sum(amount_awarded)) %>% 
  ungroup() %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award),
         year_cat = "2020-2021",
         group = "Total")

data_old <- grants_2008_2016 %>% 
  group_by(gender_category, year_cat) %>% 
  filter(funder_name %in% c("carlsbergfondet", "kræftens bekæmpelse", 
                       "danmarks grundforskningsfond", "lundbeck fonden", 
                       "velux fonden", "det frie forskningsråd", "novo nordisk", 
                       "villum kann rasmussen fonden", "velux fonden & villum kann rasmussen fonden")) %>% 
  summarise(n_grants = n(),
            n_pi = n_distinct(pi_id),
            mean_award = mean(amount_awarded),
            sum_award = sum(amount_awarded)) %>% 
  group_by(year_cat) %>% 
  mutate(pct_grants = n_grants/sum(n_grants),
         pct_pi = n_pi/sum(n_pi),
         pct_award = sum_award/sum(sum_award),
         group = "Total")

grants_fte_gender <- bind_rows(data_old, data) %>% 
  left_join(population_gender, by = c("gender_category", "year_cat", "group"))

# 1.1 Comparison

fig_2_c <- grants_fte_gender %>% 
  select(gender_category, year_cat, group, contains("pct")) %>% 
  pivot_longer(cols = pct_grants:fte_pct, names_to = "measure", values_to = "pct") %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of grants",
                             measure == "pct_pi" ~ "No. of grantees",
                             measure == "fte_pct" ~ "FTE",
                             TRUE ~ "Grant amounts")) %>% 
  mutate(measure = factor(measure, levels = c("Grant amounts", "No. of grants", "No. of grantees", "FTE")),
         gender_category = factor(gender_category, levels = c("Men", "Women", "Other"))) %>% 
  ggplot(aes(x = measure, y = pct, fill = gender_category)) + 
  geom_col() +
  facet_wrap(~ year_cat) +
  geom_text(aes(label = ifelse(gender_category %in% c("Men", "Women"), paste0(round(pct, 3)*100, "%"), "")), 
            position = position_stack(vjust = 0.5),
            size = 8 * text_scaler) +
  labs(x = "", y = "", fill = "") +
  scale_fill_manual(values = c(colour_scale_light[1], colour_scale_light[3], colour_scale_light[5])) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent_format()) +
  theme_ebm_bar(base_size = 8) +
  theme(legend.position = "right")

fig_2_c_alt <- grants_fte_gender %>% 
  select(gender_category, year_cat, group, contains("pct")) %>% 
  pivot_longer(cols = pct_grants:fte_pct, names_to = "measure", values_to = "pct") %>% 
  mutate(measure = case_when(measure == "pct_grants" ~ "No. of grants",
                             measure == "pct_pi" ~ "No. of grantees",
                             measure == "fte_pct" ~ "Full-time equivalents",
                             TRUE ~ "Grant amounts")) %>% 
  mutate(measure = factor(measure, levels = c("Grant amounts", "No. of grants", "No. of grantees", "Full-time equivalents")),
         gender_category = factor(gender_category, levels = c("Other", "Men", "Women")),
         year_cat = factor(year_cat)) %>% 
  ggplot(aes(x = year_cat, y = pct, fill = gender_category)) + 
  geom_col(data = . %>% filter(year_cat == "2020-2021"),
           colour = "white",
           linewidth = 1) +
  geom_col(data = . %>% filter(year_cat != "2020-2021"),
           alpha = 0.5,
           colour = "white",
           linewidth = 1) +
  facet_wrap(~ measure, ncol = 4) +
  geom_text(aes(label = ifelse(gender_category %in% c("Men", "Women"), paste0(round(pct, 3)*100, "%"), "")), 
            position = position_stack(vjust = 0.5),
            size = 7 * text_scaler) +
  labs(x = "", y = "", fill = "") +
       #caption = "Proportion of men, women, or grantees identifying otherwise across the total grant amount, number of grants, number of\ngrantees, and the full-time equivalent number of researchers at Danish institutions of higher education and hospitals") +
  scale_fill_manual(values = c(colour_scale_desat[1], colour_scale_desat[5], colour_scale_desat[2])) +
  scale_x_discrete(labels = c("'08-'10", "'11-'13", "'14-'16", "'20-'21")) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent_format()) +
  theme_ebm_bar(base_size = 8) +
  theme(axis.text.x = element_text(face = c("plain","plain","plain", "bold")),
        legend.position = "bottom", 
        legend.justification = "left",
        legend.text = element_text(size = 7))


