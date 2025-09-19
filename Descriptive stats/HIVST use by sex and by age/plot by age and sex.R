
#code for descrp plot age and sex wise stratification
setwd("D:\\Downloads\\MSc Thesis\\hiv-selftesting\\HIVST use by sex and by age")

plot <- read.csv("final_hivstuse_byage&sex.csv")

library(ggplot2)

#plot
ggplot(plot, aes(x = hivst_use_prop, y = age_group)) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
                 height = 0.2, position = position_dodge(width = 0.5)) +
  geom_point(aes(shape = survey_name, color = factor(sex)), 
             position = position_dodge(width = 0.5), size = 3) +
  facet_wrap(~country, scales = "free_x") +
  scale_shape_manual(values = c(15, 16, 17, 18, 19)) +
  scale_color_manual(values = c("#FFB347", "#A8E6CF"),  # Peach orange for female, light green for male
                     labels = c("Female", "Male")) +
  labs(x = "HIVST Use Proportion", y = "Age Group",
       shape = "Survey Type", color = "Sex") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "grey90"),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

ggsave("hivst_use_by_country_age_sex_peach_green.png", width = 15, height = 16)