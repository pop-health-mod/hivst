
rm(list = ls())
gc()

library(ggplot2)
library(ggsci)
library(cowplot)
library(clipr)



# ------model fit--------
setwd("D:\\Downloads\\MSc Thesis\\hivst\\Model results")
fit <- readRDS("hivst_stan_fit_jul21.rds") 

#--run model codes from the final model all countries age and sex stratification script-----
# then we run the following
#-----------------plots from fit--------------------

# testing rate
pars_beta_t <- matrix(paste0("beta_t[", rep(1:data_stan$n_cnt, each = data_stan$n_yr), ",", rep(1:data_stan$n_yr, data_stan$n_cnt), "]"), 
                      nrow = data_stan$n_cnt, ncol = data_stan$n_yr, byrow = TRUE)
r <- as.data.frame(rstan::summary(fit, pars = pars_beta_t, probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
r$`50%`
max(r$`50%`)
max(r$`97.5%`)

# ------ retesting rate ratio --------
rr_overall <- as.data.frame(rstan::summary(fit, pars = c("beta_retest_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
0.5 + (2.5 - 0.5) * plogis(rr_overall$`50%`)
0.5 + (2.5 - 0.5) * plogis(rr_overall$`2.5%`)
0.5 + (2.5 - 0.5) * plogis(rr_overall$`97.5%`)

rr <- as.data.frame(rstan::summary(fit, pars = c("beta_retest"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
rr$`50%`
rr$`2.5%`
rr$`97.5%`

# dataframe for forest plot of RR retesting
df_rr_rt <- data.frame(country = names(cnt_data),
                       median = rr$`50%`,
                       lci = rr$`2.5%`,
                       uci = rr$`97.5%`)
df_rr_ov <- rbind(df_rr_rt,
                  data.frame( country = "overall",
                              median = 0.5 + (2.5 - 0.5) * plogis(rr_overall$`50%`),
                              lci = 0.5 + (2.5 - 0.5) * plogis(rr_overall$`2.5%`),
                              uci = 0.5 + (2.5 - 0.5) * plogis(rr_overall$`97.5%`)))

# Renaming selected countries
df_rr_ov$country <- as.character(df_rr_ov$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_rr_ov$country[df_rr_ov$country == old_name] <- rename_map[[old_name]]
}

cap_first_letter <- function(x) {
  if (x == "overall") return("Overall")   
  if (nchar(x) == 0)   return(x)      
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}
df_rr_ov$country <- sapply(df_rr_ov$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_rr_ov$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))
df_rr_ov$country     <- factor(df_rr_ov$country, levels = new_levels)
df_rr_ov$style <- ifelse(df_rr_ov$country == "Overall", "pooled", "individual")

rr_retesting_forest <- ggplot(df_rr_ov, aes(x = country, y = median, color = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci, size = style)) +
  scale_color_manual(values = c("individual" = "green3", "pooled" = "midnightblue")) +
  scale_size_manual(values = c("Country" = 0.2, "Overall" = 1.2)) +
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Re-testing rate ratio", color = "Estimates (95% CrI)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  theme(legend.position = "right")
rr_retesting_forest

#write_clip(df_rr_ov, object_type = "table")


#ggsave("rr_retest_plot.png", plot = rr_retesting_forest, width = 8, height = 6, dpi = 300)

# ------- phi -----------------
phi_overall <- as.data.frame(rstan::summary(fit, pars = c("phi_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
0.5 + (1 - 0.5) * plogis(phi_overall$`50%`)
0.5 + (1 - 0.5) * plogis(phi_overall$`2.5%`)
0.5 + (1 - 0.5) * plogis(phi_overall$`97.5%`)
phi <- as.data.frame(rstan::summary(fit, pars = c("phi"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
phi$`50%`
phi$`2.5%`
phi$`97.5%`

# df and forest plot for phi
df_phi <- data.frame(country = names(cnt_data),
                     median = phi$`50%`,
                     lci = phi$`2.5%`,
                     uci = phi$`97.5%`)
df_phi_ov <- rbind(df_phi,
                   data.frame( country = "overall",
                               median = 0.5 + (1 - 0.5) * plogis(phi_overall$`50%`),
                               lci = 0.5 + (1 - 0.5) * plogis(phi_overall$`2.5%`),
                               uci = 0.5 + (1 - 0.5) * plogis(phi_overall$`97.5%`)))

# Renaming selected countries
df_phi_ov$country <- as.character(df_phi_ov$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_phi_ov$country[df_phi_ov$country == old_name] <- rename_map[[old_name]]
}

df_phi_ov$country <- sapply(df_phi_ov$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_phi_ov$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))

df_phi_ov$country     <- factor(df_phi_ov$country, levels = new_levels)
df_phi_ov$style <- ifelse(df_phi_ov$country == "Overall", "pooled", "individual")


phi_forest <- ggplot(df_phi_ov, aes(x = country, y = median, color = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci, size = style)) +
  scale_color_manual(values = c("individual" = "palevioletred2", "pooled" = "tomato4")) +  # Colors
  scale_size_manual(values = c("Country" = 0.2, "Overall" = 1.2)) +       # Line widths
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Proportion of distributed HIVST kits that are used", color = "Estimates (95% CrI)") +
  #geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +  # Reference line
  theme(legend.position = "right") +
  scale_x_discrete(labels = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2)))
phi_forest

#write_clip(df_phi_ov, object_type = "table")

#ggsave("phi_plot.png", plot = phi_forest, width = 8, height = 6, dpi = 300)


#---rr retest and phi side by side panel-----

library(patchwork)

rrretest_phi <- rr_retesting_forest +
  phi_forest +
  plot_annotation(tag_levels = "A") &
  theme(
    text        = element_text(size = 18),
    axis.title  = element_text(size = 16),
    axis.text   = element_text(size = 14),
    plot.tag    = element_text(size = 20),
    strip.text  = element_text(size = 16)
  )

ggsave("rrretest_phi_plot.png",
       plot   = rrretest_phi,
       width  = 13, height = 10, dpi = 300)


#-------- rate ratio male 15-24 years ---------------
rr_m_overall <- as.data.frame(rstan::summary(fit, pars = c("beta_male_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_m_overall$`50%`)
exp(rr_m_overall$`2.5%`)
exp(rr_m_overall$`97.5%`)

rr_m <- as.data.frame(rstan::summary(fit, pars = c("beta_male"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
rr_m$`50%`
rr_m$`2.5%`
rr_m$`97.5%`

# dataframe and forest plot for RR male
df_rrm_ <- data.frame(country = names(cnt_data),
                      median = rr_m$`50%`,
                      lci = rr_m$`2.5%`,
                      uci = rr_m$`97.5%`)
df_rr_m <- rbind(df_rrm_,
                 data.frame( country = "overall",
                             median = exp(rr_m_overall$`50%`),
                             lci = exp(rr_m_overall$`2.5%`),
                             uci = exp(rr_m_overall$`97.5%`)))

# Renaming selected countries
df_rr_m$country <- as.character(df_rr_m$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_rr_m$country[df_rr_m$country == old_name] <- rename_map[[old_name]]
}

df_rr_m$country <- sapply(df_rr_m$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_rr_m$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))

df_rr_m$country     <- factor(df_rr_m$country, levels = new_levels)
df_rr_m$style <- ifelse(df_rr_m$country == "Overall", "pooled", "individual")


rr_male_forest <- ggplot(df_rr_m, aes(x = country, y = median, color = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci, size = style)) +
  scale_color_manual(values = c("individual" = "darkorchid3", "pooled" = "mediumblue")) +  # Colors
  scale_size_manual(values = c("Country" = 0.2, "Overall" = 1.2)) +       # Line widths
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Rate ratio for 15-24 year old men (reference: 15-24 year old women) ", color = "Estimates (95% CrI)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +  # Reference line
  theme(legend.position = "right") +
  scale_x_discrete(labels = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))) +
  theme(legend.position = "right")
rr_male_forest

write_clip(df_rr_m, object_type = "table")
#ggsave("rr_male_plot.png", plot = rr_male_forest, width = 8, height = 6, dpi = 300)

#-------- rate ratio age ---------------
#---men----
rr_age_overall_m <- as.data.frame(rstan::summary(fit, pars = c("beta_age_male_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_age_overall_m$`50%`)
exp(rr_age_overall_m$`2.5%`)
exp(rr_age_overall_m$`97.5%`)

rr_age_m <- as.data.frame(rstan::summary(fit, pars = c("beta_age_male"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
rr_age_m$`50%`
rr_age_m$`2.5%`
rr_age_m$`97.5%`

# data.frame and forest plot for RR male
df_rr_a_m <- NULL
for (i in 1:n_cnt) {
  df_rr_a_m_i <- data.frame(country = names(cnt_data)[i],
                          age = c("25-34", "35-49", "50+"),
                          median = rr_age_m$`50%`[grepl(paste(paste0("beta_age_male\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age_m))],
                          lci = rr_age_m$`2.5%`[grepl(paste(paste0("beta_age_male\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age_m))],
                          uci = rr_age_m$`97.5%`[grepl(paste(paste0("beta_age_male\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age_m))])
  df_rr_a_m <- rbind(df_rr_a_m, df_rr_a_m_i)
}
df_rr_age_m <- rbind(df_rr_a_m,
                   data.frame( age = c("25-34", "35-49", "50+"),
                               country = "overall",
                               median = exp(rr_age_overall_m$`50%`),
                               lci = exp(rr_age_overall_m$`2.5%`),
                               uci = exp(rr_age_overall_m$`97.5%`)))

# Renaming selected countries
df_rr_age_m$country <- as.character(df_rr_age_m$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_rr_age_m$country[df_rr_age_m$country == old_name] <- rename_map[[old_name]]
}

df_rr_age_m$country <- sapply(df_rr_age_m$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_rr_age_m$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))

df_rr_age_m$country     <- factor(df_rr_age_m$country, levels = new_levels)
df_rr_age_m$style <- ifelse(df_rr_age_m$country == "Overall", "Pooled", "Individual")
df_rr_age_m$age <- factor(df_rr_age_m$age, levels = c("50+", "35-49", "25-34")) # to match with position.dodge()

rr_age_forest_m <- ggplot(df_rr_age_m, aes(x = country, y = median, color = age, size = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci),
                  position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("25-34" = "#845699", 
                                "35-49" = "#66BBBB",
                                "50+" = "#D61717E6")) +  # Colors
  scale_size_manual(values = c("individual" = 0.2, "pooled" = 1.2)) +       # Line widths
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Rate ratio by age for men (reference: 15-24 year old men)", color = "Estimates (95% CrI)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +  # Reference line
  theme(legend.position = "right") +
  scale_x_discrete(labels = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))) +
  theme(legend.position = "right") +
  guides(color = guide_legend(reverse = TRUE), size = "none")
rr_age_forest_m

#write_clip(df_rr_age_m, object_type = "table")
#ggsave("rr_age_plot_men.png", plot = rr_age_forest_m, width = 8, height = 6, dpi = 300)

#---women----
rr_age_overall_f <- as.data.frame(rstan::summary(fit, pars = c("beta_age_female_overall"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
exp(rr_age_overall_f$`50%`)
exp(rr_age_overall_f$`2.5%`)
exp(rr_age_overall_f$`97.5%`)

rr_age_f <- as.data.frame(rstan::summary(fit, pars = c("beta_age_female"), probs = c(0.025, 0.25, 0.5, 0.75, 0.975))$summary)
rr_age_f$`50%`
rr_age_f$`2.5%`
rr_age_f$`97.5%`

# data.frame and forest plot for RR male
df_rr_a_f <- NULL
for (i in 1:n_cnt) {
  df_rr_a_f_i <- data.frame(country = names(cnt_data)[i],
                            age = c("25-34", "35-49", "50+"),
                            median = rr_age_f$`50%`[grepl(paste(paste0("beta_age_female\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age_f))],
                            lci = rr_age_f$`2.5%`[grepl(paste(paste0("beta_age_female\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age_f))],
                            uci = rr_age_f$`97.5%`[grepl(paste(paste0("beta_age_female\\[", i, ",", 2:4, "\\]"), collapse = "|"), rownames(rr_age_f))])
  df_rr_a_f <- rbind(df_rr_a_f, df_rr_a_f_i)
}
df_rr_age_f <- rbind(df_rr_a_f,
                     data.frame( age = c("25-34", "35-49", "50+"),
                                 country = "overall",
                                 median = exp(rr_age_overall_f$`50%`),
                                 lci = exp(rr_age_overall_f$`2.5%`),
                                 uci = exp(rr_age_overall_f$`97.5%`)))

# Renaming selected countries
df_rr_age_f$country <- as.character(df_rr_age_f$country)
rename_map <- c(
  "burkinafaso"  = "Burkina Faso",
  "cotedivoire"  = "Côte d'Ivoire",
  "sierraleone"  = "Sierra Leone",
  "southafrica"  = "South Africa",
  "guineabissau" = "Guinea-Bissau",
  "drc"          = "DRC"
)
for (old_name in names(rename_map)) {
  df_rr_age_f$country[df_rr_age_f$country == old_name] <- rename_map[[old_name]]
}

df_rr_age_f$country <- sapply(df_rr_age_f$country, cap_first_letter, USE.NAMES = FALSE)

countries_no_overall <- setdiff(df_rr_age_f$country, "Overall")
countries_sorted     <- sort(countries_no_overall)    
new_levels           <- c("Overall", rev(countries_sorted))

df_rr_age_f$country     <- factor(df_rr_age_f$country, levels = new_levels)
df_rr_age_f$style <- ifelse(df_rr_age_f$country == "Overall", "Pooled", "Individual")
df_rr_age_f$age <- factor(df_rr_age_f$age, levels = c("50+", "35-49", "25-34")) # to match with position.dodge()

rr_age_forest_f <- ggplot(df_rr_age_f, aes(x = country, y = median, color = age, size = style)) +
  geom_pointrange(aes(ymin = lci, ymax = uci),
                  position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("25-34" = "#845699", 
                                "35-49" = "#66BBBB",
                                "50+" = "#D61717E6")) +  # Colors
  scale_size_manual(values = c("individual" = 0.2, "pooled" = 1.2)) +       # Line widths
  coord_flip() +
  theme_minimal() +
  labs(title = "", x = "Country", y = "Rate ratio by age for women (reference: 15-24 year old women)", color = "Estimates (95% CrI)") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +  # Reference line
  theme(legend.position = "right") +
  scale_x_discrete(labels = function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))) +
  theme(legend.position = "right") +
  guides(color = guide_legend(reverse = TRUE), size = "none")
rr_age_forest_f

#write_clip(df_rr_age_f, object_type = "table")
#ggsave("rr_age_plot_women.png", plot = rr_age_forest_f, width = 8, height = 6, dpi = 300)

#---rr ref, rr age men women side by side panel-----
#rrage_malefemale <- plot_grid(rr_age_forest_m, rr_age_forest_f, ncol = 2)
#ggsave("rrage_malefemale.png", plot = rrage_malefemale, width = 13, height = 6, dpi = 300)

library(patchwork)

# rrage_malefemale <- rr_male_forest + rr_age_forest_m +
#   rr_age_forest_f +
#   plot_annotation(tag_levels = "A")


top_row <- rr_male_forest +
  theme(
    plot.margin = margin(t = 5, r = 150, b = 5, l = 150)
  )

top_row_wrapped <- wrap_elements(top_row)

bottom_row <- rr_age_forest_m + rr_age_forest_f

rrage_malefemale <- 
  top_row_wrapped /                   # top row isolated
  bottom_row +                        # bottom row normal
  plot_layout(ncol = 1, heights = c(1, 1.2)) +
  plot_annotation(tag_levels = "A")
rrage_malefemale



ggsave("rrage_malefemale.png",
       plot   = rrage_malefemale,
       width  = 13, height = 13, dpi = 600)




# for poster, all 3 rr age plots side by side

rrage_malefemale <- 
  ( rr_male_forest + rr_age_forest_m + rr_age_forest_f ) +   # wrap in ()
  plot_layout(
    ncol   = 3,         
    guides = "collect"  
  ) +
  plot_annotation(tag_levels = "A") &  
  theme(
    legend.position = "bottom",            
    plot.margin     = margin(5, 5, 5, 5)    
  )

ggsave(
  "rrage_malefemale_wide.png",
  rrage_malefemale,
  width  = 20,    # tweak as needed
  height = 6,
  dpi    = 600
)


# alternative plot for poster
# removing duplicate y‑axes so that country names appear only on the leftmost y axis
rr_age_forest_m <- rr_age_forest_m +
  theme(axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

rr_age_forest_f <- rr_age_forest_f +
  theme(axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

rrage_malefemale <- 
  (rr_male_forest + rr_age_forest_m + rr_age_forest_f) +
  plot_layout(
    ncol   = 3,
    widths = c(1, 1.3, 1.3),  
    guides = "collect"
  ) +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    panel.spacing   = unit(1.5, "lines"),   
    plot.margin     = margin(5.5, 5.5, 5.5, 5.5)
  )

ggsave(
  "rrage_malefemale_wide2.png",
  rrage_malefemale,
  width  = 20,
  height = 11,   
  dpi    = 300
)


# left plot keeps labels
rr_male_forest <- rr_male_forest +
  theme(axis.text.y = element_text(size = 12, colour = "black", face = "bold"))

# middle & right plots lose labels
no_y <- theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank())

rr_age_forest_m <- rr_age_forest_m + no_y
rr_age_forest_f <- rr_age_forest_f + no_y

rrage_malefemale <-
  (rr_male_forest + rr_age_forest_m + rr_age_forest_f) +
  plot_layout(ncol = 3, widths = c(1.1, 1.3, 1.3), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(
    legend.position = "bottom",
    panel.spacing   = unit(1.5, "lines")
  )

ggsave("rrage_malefemale_wide3.png",
       rrage_malefemale,
       width = 20, height = 11, dpi = 300)


# ---all parameters side by side panel---
top <- wrap_plots(
  rr_male_forest,
  rr_age_forest_m,
  rr_age_forest_f,
  ncol = 3
)

bottom <- wrap_plots(
  plot_spacer(),       # empty left cell
  rr_retesting_forest, # 4th plot
  phi_forest,          # 5th plot
  ncol = 3,
  widths = c(0.2, 1, 1)
)

allpar <- top / bottom +
  plot_annotation(tag_levels = "A") +
  plot_layout(heights = c(1.2, 1))  

allpar

ggsave("all_par.png",
       plot   = allpar,
       width  = 25, height = 15, dpi = 600)


#-----plot code for trends--------

# overall trend by sex (age aggregated)
# svy_prd_m [country=1(kenya), 2(ghana).., niter=130, agegrp=1:4]
ext_fit_m <- rstan::extract(fit, pars = "svy_prd_m")$svy_prd_m
ext_fit_f <- rstan::extract(fit, pars = "svy_prd_f")$svy_prd_f

dim(ext_fit_m) # draws, country, iterations (time), age groups
n_draws <- dim(ext_fit_m)[1]
n_cnt   <- dim(ext_fit_m)[2]
niter   <- dim(ext_fit_m)[3]
n_age   <- dim(ext_fit_m)[4]

data_stan$pop #  4 rows per country (one per age group)
# pop_mat_m[c, a] => population of males in country c, age group a
# pop_mat_f[c, a] => population of females in country c, age group a

pop_mat_m <- matrix(NA, nrow = n_cnt, ncol = 4)
pop_mat_f <- matrix(NA, nrow = n_cnt, ncol = 4)
for (c in seq_len(n_cnt)) {
  row_start <- (c - 1)*4 + 1
  row_end   <- row_start + 3
  # male population in column 1 of data_stan$pop
  pop_mat_m[c, ] <- data_stan$pop[row_start:row_end, 1]
  # female population in column 2 of data_stan$pop
  pop_mat_f[c, ] <- data_stan$pop[row_start:row_end, 2]
}
# total region‐level population (across all countries and all age groups)
total_pop_male <- sum(pop_mat_m)
total_pop_female <- sum(pop_mat_f)


# getting overall proportion for trend stratified by sex
male_prp <- matrix(NA, nrow = n_draws, ncol = niter)
female_prp <- matrix(NA, nrow = n_draws, ncol = niter)
for (i in seq_len(n_draws)) {
  for (t in seq_len(niter)) {
    # summing for men
    sum_m <- 0
    for (c in seq_len(n_cnt)) {
      for (a in seq_len(n_age)) {
        # ext_fit_m[i, c, t, a] = predicted proportion (0 to 1)
        # multiplying proportion by population in that group
        sum_m <- sum_m + ext_fit_m[i, c, t, a] * pop_mat_m[c, a]
      }
    }
    male_prp[i, t] <- sum_m / total_pop_male
    
    # summing for women
    sum_f <- 0
    for (c in seq_len(n_cnt)) {
      for (a in seq_len(n_age)) {
        sum_f <- sum_f + ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
      }
    }
    female_prp[i, t] <- sum_f / total_pop_female
  }
}

# median and 95% CrI
male_lci <- apply(male_prp, 2, quantile, 0.025)
male_med <- apply(male_prp, 2, quantile, 0.5)
male_uci <- apply(male_prp, 2, quantile, 0.975)
female_lci <- apply(female_prp, 2, quantile, 0.025)
female_med <- apply(female_prp, 2, quantile, 0.5)
female_uci <- apply(female_prp, 2, quantile, 0.975)

# plotting
#dev.off()
df_sextrend <- data.frame(
  time       = time,
  male_med   = male_med * 100,
  male_lci   = male_lci * 100,
  male_uci   = male_uci * 100,
  female_med = female_med * 100,
  female_lci = female_lci * 100,
  female_uci = female_uci * 100
)

sex_trend_plot <- ggplot(df_sextrend, aes(x = time)) +
  geom_ribbon(aes(ymin = male_lci, ymax = male_uci),
              fill = "lightblue", alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = male_med, color = "Male"), size = 1.1) +
  geom_ribbon(aes(ymin = female_lci, ymax = female_uci),
              fill = "pink", alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = female_med, color = "Female"), size = 1.1) +
  scale_color_manual(
    name = "Sex",
    values = c("Male" = "deepskyblue4", "Female" = "deeppink1")
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom", 
    plot.title      = element_text(hjust = 0.5)
  ) +
  ggtitle("HIVST uptake by sex")

sex_trend_plot

#write.table(df_sextrend, "clipboard", sep = "\t", row.names = FALSE)
#ggsave("trend_sex_plot.png", plot = sex_trend_plot, width = 8, height = 6, dpi = 300)

#------------- overall trend by age groups ------------------
ext_fit_m <- rstan::extract(fit, pars = "svy_prd_m")$svy_prd_m
ext_fit_f <- rstan::extract(fit, pars = "svy_prd_f")$svy_prd_f

n_draws <- dim(ext_fit_m)[1]
n_cnt   <- dim(ext_fit_m)[2]
niter   <- dim(ext_fit_m)[3]
n_age   <- dim(ext_fit_m)[4]

pop_mat_m <- matrix(NA, nrow = n_cnt, ncol = n_age)  # male population
pop_mat_f <- matrix(NA, nrow = n_cnt, ncol = n_age)  # female population
for (c in seq_len(n_cnt)) {
  row_start <- (c - 1)*4 + 1
  row_end   <- row_start + 3
  # male population in column 1
  pop_mat_m[c, ] <- data_stan$pop[row_start:row_end, 1]
  # female population in column 2
  pop_mat_f[c, ] <- data_stan$pop[row_start:row_end, 2]
}

#  array to store sex-aggregated proportions by age group
# prp_age[i, t, a] = proportion who have used HIVST in age group a, time t, draw i, aggregated over all countries & sexes
prp_age <- array(NA, dim = c(n_draws, niter, n_age))
for (i in seq_len(n_draws)) {
  for (t in seq_len(niter)) {
    for (a in seq_len(n_age)) {
      sum_counts <- 0
      for (c in seq_len(n_cnt)) {
        # male
        sum_counts <- sum_counts + ext_fit_m[i, c, t, a] * pop_mat_m[c, a]
        # female
        sum_counts <- sum_counts + ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
      }
      total_pop_age <- sum(pop_mat_m[, a]) + sum(pop_mat_f[, a])
      
      prp_age[i, t, a] <- sum_counts / total_pop_age
    }
  }
}

prp_age_median <- apply(prp_age, c(2, 3), median)   
prp_age_lci    <- apply(prp_age, c(2, 3), quantile, 0.025)
prp_age_uci    <- apply(prp_age, c(2, 3), quantile, 0.975)


# plotting
age_labels <- c("15-24 years", "25-34 years", "35-49 years", "50+ years")
df_age <- data.frame()
for (a in seq_len(n_age)) {
  df_a <- data.frame(
    time   = time,
    median = prp_age_median[, a] * 100,  
    lci    = prp_age_lci[, a]    * 100,
    uci    = prp_age_uci[, a]    * 100,
    age_grp = age_labels[a]  
  )
  
  df_age <- rbind(df_age, df_a)
}

p_age <- ggplot(df_age, aes(x = time)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = age_grp),
              alpha = 0.2, color = NA) +
  geom_line(aes(y = median, color = age_grp),
            size = 1.1) +
  scale_color_manual(
    values = c("15-24 years" = "#326df9", 
               "25-34 years" = "#a3d47e", 
               "35-49 years" = "#ff7476", 
               "50+ years"   = "#f9b332")
  ) +
  scale_fill_manual(
    values = c("15-24 years" = "#326df9",
               "25-34 years" = "#a3d47e", 
               "35-49 years" = "#ff7476", 
               "50+ years"   = "#f9b332")
  )+
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) + 
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)",
    color = "Age Group",
    fill  = "Age Group"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(hjust = 0.5)
  ) +
  ggtitle("HIVST uptake by age groups")

p_age

#write_clip(df_age, object_type = "table")

#ggsave("trend_age_plot.png", plot = p_age, width = 8, height = 6, dpi = 300)


#------overall trend---------

prp_total <- matrix(NA, nrow = n_draws, ncol = niter)
total_pop <- sum(pop_mat_m) + sum(pop_mat_f)

for (i in seq_len(n_draws)) {
  for (t in seq_len(niter)) {
    sum_counts <- 0
    for (c in seq_len(n_cnt)) {
      for (a in seq_len(n_age)) {
        sum_counts <- sum_counts +
          ext_fit_m[i, c, t, a] * pop_mat_m[c, a] +
          ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
      }
    }
    prp_total[i, t] <- sum_counts / total_pop
  }
}

prp_total_median <- apply(prp_total, 2, median)
prp_total_lci    <- apply(prp_total, 2, quantile, 0.025)
prp_total_uci    <- apply(prp_total, 2, quantile, 0.975)


df_total <- data.frame(
  time   = time,
  median = prp_total_median * 100,
  lci    = prp_total_lci    * 100,
  uci    = prp_total_uci    * 100
)

p_total <- ggplot(df_total, aes(x = time)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), 
              fill = "palegreen2", alpha = 0.2) +
  geom_line(aes(y = median), color = "palegreen2", size = 1.2) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Overall HIVST uptake")

p_total

#write_clip(df_total, object_type = "table")


#---region wise trend-----

# ---- regional classification from GBD 2015-----
country_to_region <- c(
  "Burundi"                       = "Eastern", 
  "Kenya"                         = "Eastern", 
  "Madagascar"                    = "Eastern", 
  "Malawi"                        = "Eastern", 
  "Mozambique"                    = "Eastern", 
  "Rwanda"                        = "Eastern", 
  "United Republic of Tanzania"   = "Eastern",  
  "Uganda"                        = "Eastern", 
  "Zambia"                        = "Eastern", 
  "Democratic Republic of the Congo" = "Central",
  
  "Botswana"                      = "Southern", 
  "Eswatini"                      = "Southern", 
  "Lesotho"                       = "Southern", 
  "Namibia"                       = "Southern", 
  "South Africa"                  = "Southern", 
  "Zimbabwe"                      = "Southern", 
  
  "Benin"                         = "Western", 
  "Burkina Faso"                  = "Western", 
  "Cameroon"                      = "Western", 
  "Cote d'Ivoire"                 = "Western", 
  "Ghana"                         = "Western", 
  "Guinea"                        = "Western", 
  "Guinea-Bissau"                 = "Western", 
  "Liberia"                       = "Western", 
  "Mali"                          = "Western", 
  "Senegal"                       = "Western", 
  "Sierra Leone"                  = "Western"
)


is_ESA <- function(region) {
  #  Eastern + Southern as "ESA"
  region %in% c("Eastern", "Southern")
}

is_WCA <- function(region) {
  # We'll treat Western + Central as "WCA"
  region %in% c("Western", "Central")
}

# Now for each country in `countries`, find if it belongs to ESA or WCA:
country_super_region <- sapply(countries, function(ctry) {
  if (is_ESA(country_to_region[ctry])) {
    return("ESA")
  } else if (is_WCA(country_to_region[ctry])) {
    return("WCA")
  } else {
    # If, for some reason, there's a mismatch or new region, handle it
    return(NA_character_)
  }
})

stopifnot(length(country_super_region) == n_cnt)

# We'll create region-level population mats:
pop_mat_m_ESA <- matrix(0, nrow = n_cnt, ncol = n_age)
pop_mat_f_ESA <- matrix(0, nrow = n_cnt, ncol = n_age)
pop_mat_m_WCA <- matrix(0, nrow = n_cnt, ncol = n_age)
pop_mat_f_WCA <- matrix(0, nrow = n_cnt, ncol = n_age)

for (c in seq_len(n_cnt)) {
  if (country_super_region[c] == "ESA") {
    pop_mat_m_ESA[c, ] <- pop_mat_m[c, ]
    pop_mat_f_ESA[c, ] <- pop_mat_f[c, ]
  } else if (country_super_region[c] == "WCA") {
    pop_mat_m_WCA[c, ] <- pop_mat_m[c, ]
    pop_mat_f_WCA[c, ] <- pop_mat_f[c, ]
  }
}

# Region-level total population
pop_total_ESA <- sum(pop_mat_m_ESA) + sum(pop_mat_f_ESA)
pop_total_WCA <- sum(pop_mat_m_WCA) + sum(pop_mat_f_WCA)

# We'll store draws in:
esa_prp <- matrix(NA, nrow = n_draws, ncol = niter)
wca_prp <- matrix(NA, nrow = n_draws, ncol = niter)

for (i in seq_len(n_draws)) {
  for (t in seq_len(niter)) {
    # ESA
    sum_esa <- 0
    for (c in seq_len(n_cnt)) {
      if (country_super_region[c] == "ESA") {
        for (a in seq_len(n_age)) {
          # sum men
          sum_esa <- sum_esa + ext_fit_m[i, c, t, a] * pop_mat_m[c, a]
          # sum women
          sum_esa <- sum_esa + ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
        }
      }
    }
    esa_prp[i, t] <- sum_esa / pop_total_ESA
    
    # WCA
    sum_wca <- 0
    for (c in seq_len(n_cnt)) {
      if (country_super_region[c] == "WCA") {
        for (a in seq_len(n_age)) {
          sum_wca <- sum_wca + ext_fit_m[i, c, t, a] * pop_mat_m[c, a]
          sum_wca <- sum_wca + ext_fit_f[i, c, t, a] * pop_mat_f[c, a]
        }
      }
    }
    wca_prp[i, t] <- sum_wca / pop_total_WCA
  }
}

esa_lci <- apply(esa_prp, 2, quantile, 0.025)
esa_med <- apply(esa_prp, 2, quantile, 0.5)
esa_uci <- apply(esa_prp, 2, quantile, 0.975)

wca_lci <- apply(wca_prp, 2, quantile, 0.025)
wca_med <- apply(wca_prp, 2, quantile, 0.5)
wca_uci <- apply(wca_prp, 2, quantile, 0.975)

df_regions <- data.frame(
  time = rep(time, times = 2),
  region = rep(c("ESA", "WCA"), each = length(time)),
  
  # Put proportions in percent
  median = c(esa_med, wca_med) * 100,
  lci    = c(esa_lci,  wca_lci)  * 100,
  uci    = c(esa_uci,  wca_uci)  * 100
)


p_regions <- ggplot(df_regions, aes(x = time, y = median, color = region, fill = region)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("ESA" = "darkslateblue", "WCA" = "darkviolet"), 
    labels = c("ESA" = "Eastern & Southern Africa", 
               "WCA" = "Western & Central Africa")
  ) +
  scale_fill_manual(
    values = c("ESA" = "darkslateblue", "WCA" = "darkviolet"),
    labels = c("ESA" = "Eastern & Southern Africa", 
               "WCA" = "Western & Central Africa")
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)",
    color = "Region",
    fill  = "Region"
  ) +
  theme_classic(base_size = 14) +
  ggtitle("HIVST uptake by region") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)  # center the plot title
  )

p_regions

#write_clip(df_regions, object_type = "table")


#ggsave("trend_region_plot.png", plot = p_regions, width = 8, height = 6, dpi = 300)


#---3 trend plots in panel-----------
# removing legend titles for panel
# region
p_regions <- ggplot(df_regions, aes(x = time, y = median, color = region, fill = region)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  scale_color_manual(
    values = c("ESA" = "darkslateblue", "WCA" = "darkviolet"),
    labels = c("ESA" = "Eastern & Southern Africa",
               "WCA" = "Western & Central Africa"),
    name   = NULL            # <--- remove legend title
  ) +
  scale_fill_manual(
    values = c("ESA" = "darkslateblue", "WCA" = "darkviolet"),
    labels = c("ESA" = "Eastern & Southern Africa",
               "WCA" = "Western & Central Africa"),
    name   = NULL            # <--- remove legend title
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)"
    # we omit color= or fill= to avoid legend titles here
  ) +
  theme_classic(base_size = 14) +
  ggtitle("HIVST uptake by region") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

p_regions

# sex
sex_trend_plot <- ggplot(df_sextrend, aes(x = time)) +
  geom_ribbon(aes(ymin = male_lci, ymax = male_uci),
              fill = "lightblue", alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = male_med, color = "Male"), size = 1.1) +
  geom_ribbon(aes(ymin = female_lci, ymax = female_uci),
              fill = "pink", alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = female_med, color = "Female"), size = 1.1) +
  scale_color_manual(
    name = NULL,
    values = c("Male" = "deepskyblue4", "Female" = "deeppink1")
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom", 
    plot.title      = element_text(hjust = 0.5)
  ) +
  ggtitle("HIVST uptake by sex")

sex_trend_plot

# age
p_age <- ggplot(df_age, aes(x = time)) +
  geom_ribbon(aes(ymin = lci, ymax = uci, fill = age_grp),
              alpha = 0.2, color = NA) +
  geom_line(aes(y = median, color = age_grp), size = 1.1) +
  scale_color_manual(
    values = c("15-24 years" = "#326df9", 
               "25-34 years" = "#a3d47e", 
               "35-49 years" = "#ff7476", 
               "50+ years"   = "#f9b332"),
    name   = NULL 
  ) +
  scale_fill_manual(
    values = c("15-24 years" = "#326df9",
               "25-34 years" = "#a3d47e", 
               "35-49 years" = "#ff7476", 
               "50+ years"   = "#f9b332"),
    name   = NULL 
  ) +
  scale_x_continuous(breaks = seq(2012, 2024, by = 2)) +
  scale_y_continuous(breaks = seq(0, 20, by = 2)) +
  labs(
    x = "Year",
    y = "Proportion of people who have ever used HIVST (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title    = element_blank(),     # remove any remaining title
    plot.title      = element_text(hjust = 0.5)
  ) +
  guides(
    color = guide_legend(nrow = 2),
    fill  = guide_legend(nrow = 2)
  ) +
  ggtitle("HIVST uptake by age groups")

p_age

# combined plot (all 3)
library(patchwork)

combined_trend <- p_regions + sex_trend_plot + p_age +
  plot_layout(ncol = 3) +
  plot_annotation(
    tag_levels = "A",              # gives A, B, C
    theme = theme(
      plot.tag.position = c(0.08, 0.92),   # x, y in npc units
      plot.tag           = element_text(size = 16, face = "bold")
    )
  )

combined_trend

combined_trend <- combined_trend &
  theme(
    plot.margin = margin(t = 5, r = 5, b = 5, l = 15)  # in pt by default
  )

ggsave("trend_all3_plot.png", plot = combined_trend, width = 13, height = 6, dpi = 300)


# combined plot (all 4)

library(patchwork)

combined_trend2 <- p_total + p_regions + sex_trend_plot + p_age +
  plot_layout(ncol = 2) +
  plot_annotation(
    tag_levels = "A",              # gives A, B, C
    theme = theme(
      plot.tag.position = c(0.08, 0.92),   # x, y in npc units
      plot.tag           = element_text(size = 16, face = "bold")
    )
  )

combined_trend2

combined_trend2 <- combined_trend2 &
  theme(
    plot.margin = margin(t = 5, r = 5, b = 5, l = 15)  # in pt by default
  )

ggsave("trend_all4_plot.png", plot = combined_trend2, width = 13, height = 13, dpi = 600)


#--- function code to check for survey and program fit--------
#  no separate sex dimension in svy_prd_m because it is only for males
# svy_prd_m [country=1(kenya), 2(ghana).., niter=130, agegrp=1:4]

cnt_lowercase <- c("kenya", "ghana", "malawi", "madagascar", "zimbabwe",
                   "sierraleone", "zambia", "mali", "uganda",
                   "lesotho", "mozambique", "rwanda",
                   "burkinafaso", "burundi", "cameroon", "cotedivoire",
                   "guinea", "liberia", "senegal", "southafrica","tanzania",
                   "namibia", "botswana", "guineabissau","drc", "eswatini", "benin")
alphabetical_cnt <- order(cnt_lowercase)


# ------ men results --------
svy_m_full <- rstan::summary(fit, "svy_prd_m", probs=c(0.025, 0.5, 0.975))$summary
svy_m_full <- as.data.frame(svy_m_full)
svy_m_full$param <- rownames(svy_m_full)

# splitting by countries
n_cnt <- length(countries)
svy_m_list <- vector("list", n_cnt)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), svy_m_full$param)   # only country c
  tmp_c <- svy_m_full[ix_c, ]
  
  #  within that subset, separating each age group
  ages <- vector("list", 4)
  for (a in 1:4) {
    ix_a <- grepl(paste0(",", a, "\\]$"), tmp_c$param)
    ages[[a]] <- tmp_c[ix_a, ]
  }
  svy_m_list[[c]] <- ages
}

# plotting for each country
png("survey_fit_men2.png",
    width = 20, height = 28,
    units = "in", res = 320)
par(mfrow = c(7, 4))
par(
  mar = c(3, 4, 2, 1),  
  oma = c(0, 0, 0, 0) 
)

for (c_idx in alphabetical_cnt) {
  plot(NA, xlim = range(time), ylim = c(0, 0.5),
       main = paste("Men -", countries[c_idx]),
       xlab = "Year", ylab = "Ever used HIVST")
  offsets <- c(-0.2, -0.1, 0.1, 0.2)
  for (a in 1:4) {
    df_age <- svy_m_list[[c_idx]][[a]] 
    lines(df_age$`50%` ~ time, col = a, lwd = 2)
    polygon(
      x = c(time, rev(time)),
      y = c(df_age$`2.5%`, rev(df_age$`97.5%`)),
      col = adjustcolor(a, alpha.f = 0.3), border = NA
    )
    
    obs <- cnt_data[[c_idx]]$svy_dat_m[, a]   
    lci <- cnt_data[[c_idx]]$lci_svy_m[, a]
    uci <- cnt_data[[c_idx]]$uci_svy_m[, a]
    t_obs <- cnt_data[[c_idx]]$yr_svy
    x_dodge <- t_obs + offsets[a]
    points(obs ~ x_dodge, pch = 16, col = a)
    segments(x_dodge, lci, x_dodge, uci, col = a)
  }
  
  legend("topleft",
         legend = c("15-24", "25-34", "35-49", "50+"),
         col = 1:4, lwd = 2, bty = "n")
}

dev.off()

#------ women results --------
svy_f_full <- rstan::summary(fit, "svy_prd_f", probs=c(0.025, 0.5, 0.975))$summary
svy_f_full <- as.data.frame(svy_f_full)
svy_f_full$param <- rownames(svy_f_full)

svy_f_list <- vector("list", n_cnt)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), svy_f_full$param)
  tmp_c <- svy_f_full[ix_c, ]
  
  ages <- vector("list", 4)
  for (a in 1:4) {
    ix_a <- grepl(paste0(",", a, "\\]$"), tmp_c$param)
    ages[[a]] <- tmp_c[ix_a, ]
  }
  svy_f_list[[c]] <- ages
}

# plotting for each country
png("survey_fit_women2.png",
    width = 20, height = 28,
    units = "in", res = 320)
par(mfrow = c(7, 4))
par(
  mar = c(3, 4, 2, 1),  
  oma = c(0, 0, 0, 0) 
)
for (c_idx in alphabetical_cnt) {
  plot(NA, xlim = range(time), ylim = c(0, 0.5),
       main = paste("Women -", countries[c_idx]),
       xlab = "Year", ylab = "Ever used HIVST")
  
  offsets <- c(-0.2, -0.1, 0.1, 0.2)
  
  for (a in 1:4) {
    df_age <- svy_f_list[[c_idx]][[a]]
    
    lines(df_age$`50%` ~ time, col=a, lwd=2)
    polygon(
      x = c(time, rev(time)),
      y = c(df_age$`2.5%`, rev(df_age$`97.5%`)),
      col = adjustcolor(a, alpha.f=0.3),
      border=NA
    )
    
    obs <- cnt_data[[c_idx]]$svy_dat_f[, a]
    lci <- cnt_data[[c_idx]]$lci_svy_f[, a]
    uci <- cnt_data[[c_idx]]$uci_svy_f[, a]
    t_obs <- cnt_data[[c_idx]]$yr_svy
    
    x_dodge <- t_obs + offsets[a]
    
    points(obs ~ x_dodge, pch=16, col=a)
    segments(x_dodge, lci, x_dodge, uci, col=a)
  }
  
  legend("topleft", legend=c("15-24","25-34","35-49","50+"),
         col=1:4, lwd=2, bty="n")
}

dev.off()


#--- hts results ----
hts_full <- as.data.frame(rstan::summary(fit, "hivst_prd")$summary)
hts_full$param <- rownames(hts_full)

# Splitting by countries
n_cnt <- length(countries)
hts_list <- vector("list", n_cnt)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), hts_full$param)
  hts_list[[c]] <- hts_full[ix_c, ]
}

png("program_data_fit2.png",
    width = 14, height = 28,
    units = "in", res = 320)

par(mfrow = c(7, 4))
par(mar = c(3, 4, 2, 1),  
    oma = c(0, 0, 0, 0))

for (c_idx in alphabetical_cnt) {
  df_c <- hts_list[[c_idx]]
  
  plot(time, df_c$`50%`, type = "l", col = "blue", lwd = 2,
       main = paste("HTS -", countries[c_idx]),
       xlab = "Year", ylab = "Number of HIVST kits")
  
  polygon(x = c(time, rev(time)),
          y = c(df_c$`2.5%`, rev(df_c$`97.5%`)),
          col = adjustcolor("blue", alpha.f = 0.3),
          border = NA)
  
  # observed program data
  t_obs   <- cnt_data[[c_idx]]$yr_hts
  obs_hts <- cnt_data[[c_idx]]$hts_dat
  points(obs_hts ~ t_obs, pch = 16, col = "red")
}

dev.off()

#--------plot code for national estimates of proportion of hivst users in 2024 (when model stops)----------------
#---- Overall proportion at the dt, end of 2024 (last niter 130)---------------
ext_fit_m <- rstan::extract(fit, pars = "svy_prd_m")$svy_prd_m
ext_fit_f <- rstan::extract(fit, pars = "svy_prd_f")$svy_prd_f
n_draws <- dim(ext_fit_m)[1]
n_cnt   <- dim(ext_fit_m)[2]
n_iter  <- dim(ext_fit_m)[3]   
n_age   <- dim(ext_fit_m)[4]

res_last_dt <- matrix(NA, nrow = n_draws, ncol = n_cnt,
                      dimnames = list(NULL, c("Kenya", "Ghana", "Malawi", "Madagascar", "Zimbabwe", 
                                              "Sierra Leone", "Zambia", "Mali", "Uganda",
                                              "Lesotho", "Mozambique", "Rwanda",
                                              "Burkina Faso", "Burundi", "Cameroon", "Cote d'Ivoire",
                                              "Guinea", "Liberia", "Senegal", "South Africa", 
                                              "United Republic of Tanzania", "Namibia", "Botswana", 
                                              "Guinea-Bissau", "Democratic Republic of the Congo", "Eswatini", "Benin")
                      ))

for (i in seq_len(n_draws)) {
  for (c in seq_len(n_cnt)) {
    men_count   <- 0
    women_count <- 0
    for (a in seq_len(n_age)) {
      men_count   <- men_count   + ext_fit_m[i, c, n_iter, a] * pop_mat_m[c, a]
      women_count <- women_count + ext_fit_f[i, c, n_iter, a] * pop_mat_f[c, a]
    }
    total_pop_c <- sum(pop_mat_m[c, ]) + sum(pop_mat_f[c, ])
    
    res_last_dt[i, c] <- (men_count + women_count) / total_pop_c
  }
}

res_last_dt_median <- apply(res_last_dt, 2, median)
res_last_dt_lci    <- apply(res_last_dt, 2, quantile, probs = 0.025)
res_last_dt_uci    <- apply(res_last_dt, 2, quantile, probs = 0.975)

df_last_dt <- data.frame(
  Country = c("Kenya", "Ghana", "Malawi", "Madagascar", "Zimbabwe", 
              "Sierra Leone", "Zambia", "Mali", "Uganda",
              "Lesotho", "Mozambique", "Rwanda",
              "Burkina Faso", "Burundi", "Cameroon", "Cote d'Ivoire",
              "Guinea", "Liberia", "Senegal", "South Africa", 
              "United Republic of Tanzania", "Namibia", "Botswana", 
              "Guinea-Bissau", "Democratic Republic of the Congo", "Eswatini", "Benin"), 
  Median  = res_last_dt_median * 100,
  LCI     = res_last_dt_lci * 100,
  UCI     = res_last_dt_uci * 100
)
df_last_dt

# overall_country_estimates <- ggplot(df_last_dt, aes(x = Country, y = Median)) +
#   geom_col(fill = "#CC0066", color = "black") + 
#   geom_errorbar(aes(ymin = LCI, ymax = UCI), 
#                 width = 0.2, color = "black", size = 0.7) +
#   scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
#   labs(
#     x = NULL,
#     y = "Proportion of people who have used HIVST (%)",
#     ggtitle = "HIVST Use by Country"
#   ) +
#   theme_classic(base_size = 14)
# 
# overall_country_estimates



# overall_country_estimates <- ggplot(df_last_dt, aes(x = reorder(Country, Median), y = Median)) +
#   geom_col(fill = "#CC0066", color = "black") + 
#   geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, color = "black", size = 0.7) +
#   coord_flip() +  # Flip the coordinates so that country names are on the y-axis
#   scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
#   labs(
#     x = NULL,
#     y = "Proportion of people who have used HIVST (%)",
#     title = "National estimates of HIVST uptake in 2024"
#   ) +
#   theme_classic(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 14, face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
#   )
# 
# overall_country_estimates


# overall_country_estimates <- ggplot(df_last_dt, aes(x = reorder(Country, Median), y = Median, fill = Median)) +
#   geom_col(color = "black") +
#   geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, color = "black", size = 0.7) +
#   scale_fill_gradient(low = "#B3CDE3", high = "#011F4B") +
#   coord_flip() +
#   scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
#   labs(
#     x = NULL,
#     y = "Proportion of people who have used HIVST (%)",
#     title = "HIVST Use by Country (2024)"
#   ) +
#   theme_classic(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 14, face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
#   )
# 
# overall_country_estimates


# overall_country_estimates <- ggplot(df_last_dt, aes(x = reorder(Country, Median), y = Median, fill = Median)) +
#   geom_col(color = "black") +
#   geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, color = "black", size = 0.7) +
#   scale_fill_gradientn(colors = c("#B3CDE3", "#66B2FF", "#1E90FF", "#00509E")) +
#   coord_flip() +
#   scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
#   labs(
#     x = NULL,
#     y = "Proportion of people who have used HIVST (%)",
#     title = "National estimates of HIVST uptake in 2024"
#   ) +
#   theme_classic(base_size = 14) +
#   theme(
#     axis.text = element_text(size = 12),
#     axis.title = element_text(size = 14, face = "bold"),
#     plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
#   )
# 
# overall_country_estimates

overall_country_estimates <- ggplot(df_last_dt, aes(x = reorder(Country, Median), y = Median, fill = Median)) +
  geom_col(color = NA) +  # Remove borders from the bars
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.2, color = "#8B0000", size = 0.7) +  # Deep red error bars
  scale_fill_gradientn(colors = c("#FFF5E1", "#FFA500", "#FF4500", "#FF0000")) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 55), expand = c(0, 0)) +
  labs(
    x = NULL,
    y = "Proportion of people who have used HIVST (%)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  )

overall_country_estimates

write_clip(df_last_dt, object_type = "table")

ggsave("country-estimate_2024.png", plot = overall_country_estimates, width = 8, height = 6, dpi = 300)


#-----------------------------------------------------------------------------

# survey data for men and women (no age group)
cnt_data <- list(
  kenya = list(
    yr_svy = c(2012.5, 2018.5, 2022.5),
    ind_svy = (c(2012.5, 2018.5, 2022.5) - start) / dt,
    den_svy = round(cbind(c(4605, 16082, 11562), c(6350, 17880, 25725))),
    num_svy = round(cbind(c(148, 340, 1044), c(116, 436, 1242))),
    yr_hts = c(2018,  2019,   2020,    2021,   2022,  2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(197200, 400000, 595953, 630000, 342610, 617317),
    se_hts = c(197200, 400000, 595953, 630000, 342610, 617317) * 0.1
  ),
  ghana = list(
    yr_svy = c(2017.5, 2022.5),
    ind_svy = (c(2017.5, 2022.5) - start) / dt,
    den_svy = round(cbind(c(2553, 4558), c(5575, 6250))),
    num_svy = round(cbind(c(37, 83), c(132, 151))),
    yr_hts = c(2020,  2021,   2022,  2023) + 0.5,
    ind_hts = (c(2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(20000, 1323, 235000, 140500),
    se_hts = c(20000, 1323, 235000, 140500) * 0.1
  ),
  malawi = list(
    yr_svy = c(2015.5, 2019.5, 2020.5),
    ind_svy = (c(2015.5, 2019.5, 2020.5) - start) / dt,
    den_svy = round(cbind(c(2796, 2150, 5165), c(14792, 6669, 5920))),
    num_svy = round(cbind(c(30, 214, 406), c(136, 443, 373))),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(408900, 101256, 561282, 602657, 735385, 910088),
    se_hts =  c(408900, 101256, 561282, 602657, 735385, 910088) * 0.1 
  ),
  madagascar = list(
    yr_svy = c(2018.5, 2021.5),
    ind_svy = (c(2018.5, 2021.5) - start) / dt,
    den_svy = round(cbind(c(3055, 6178), c(5039, 6825))),
    num_svy = round(cbind(c(35, 44), c(84, 20))),
    yr_hts = c(2022,  2023) + 0.5,
    ind_hts = (c(2022, 2023) - start + 0.5) / dt,
    hts_dat = c(2500, 2500),
    se_hts = c(2500, 2500) * 0.1
  ),
  zimbabwe = list(
    yr_svy = c(2015.5, 2019.5, 2020.5),
    ind_svy = (c(2015.5, 2019.5, 2020.5) - start) / dt,
    den_svy = round(cbind(c(6717, 3343, 6576), c(7964, 8104, 10058))),
    num_svy = round(cbind(c(118, 171, 381), c(21, 447, 594))),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(197408, 174566, 240434, 459517, 414499, 513090),
    se_hts = c(197408, 174566, 240434, 459517, 414499, 513090) * 0.1
  ),
  sierraleone = list(
    yr_svy = c(2017.5, 2019.5),
    ind_svy = (c(2017.5, 2019.5) - start) / dt,
    den_svy = round(cbind(c(2465, 2907), c(5096, 2607))),
    num_svy = round(cbind(c(50, 62), c(165, 101))),
    yr_hts = c(2021, 2022, 2023) + 0.5,
    ind_hts = (c(2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(2678, 1173, 50340),
    se_hts = c(2678, 1173, 50340) * 0.1
  ),
  zambia = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy = round(cbind(3756,2869)),
    num_svy = round(cbind(104,82)),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(315348, 781175, 639225, 23750, 33153, 95559),
    se_hts = c(315348, 781175, 639225, 23750, 33153, 95559) * 0.1
  ),
  mali = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy = round(cbind(3740,2990)),
    num_svy = round(cbind(15,31)),
    yr_hts = c(2019, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2019, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(7763, 169962, 11375, 235729),
    se_hts = c(7763, 169962, 11375, 235729) * 0.1
  ),
  uganda = list(
    yr_svy =  2016.5,
    ind_svy = (2016.5 - start) / dt,
    den_svy = round(cbind(3640,8640)),
    num_svy = round(cbind(203,389)),
    yr_hts = c(2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(42570, 306421, 750698, 681602),
    se_hts = c(42570, 306421, 750698, 681602) * 0.1
  ),
  lesotho  = list(
    yr_svy =  c(2020.5, 2023.5),
    ind_svy = (c(2020.5, 2023.5) - start) / dt,
    den_svy = round(cbind(c(4729, 2284), c(5038,5131))),
    num_svy = round(cbind(c(474, 837), c(435, 2529))),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(58917, 42650, 164236, 281277, 301762, 262915),
    se_hts = c(58917, 42650, 164236, 281277, 301762, 262915) * 0.1
  ),
  mozambique = list(
    yr_svy =  c(2021.5, 2022.5),
    ind_svy = (c(2021.5, 2022.5) - start) / dt,
    den_svy = round(cbind(c(1483, 3164), c(1590, 8289))),
    num_svy = round(cbind(c(128, 107), c(174, 176))),
    yr_hts = c(2021, 2022, 2023) + 0.5,
    ind_hts = (c(2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(67883, 203966, 683345),
    se_hts = c(67883, 203966, 683345) * 0.1
  ),
  rwanda = list(
    yr_svy =  2019.5,
    ind_svy = (2019.5 - start) / dt,
    den_svy = round(cbind(3840,10700)),
    num_svy = round(cbind(62,140)),
    yr_hts = 2023 + 0.5,
    ind_hts = (2023 - start + 0.5) / dt,
    hts_dat = 62683,
    se_hts = 62683 * 0.1
  ),
  burkinafaso = list(
    yr_svy =  2021.5,
    ind_svy = (2021.5 - start) / dt,
    den_svy = round(cbind(3570,11000)),
    num_svy = round(cbind(18,27)),
    yr_hts = c(2022, 2023) + 0.5,
    ind_hts = (c(2022, 2023) - start + 0.5) / dt,
    hts_dat = c(968, 3367),
    se_hts = c(968, 3367) * 0.1
  ),
  burundi = list(
    yr_svy =  2016.5,
    ind_svy = (2016.5 - start) / dt,
    den_svy = round(cbind(6620,6340)),
    num_svy = round(cbind(25,16)),
    yr_hts = 2023 + 0.5,
    ind_hts = (2023 - start + 0.5) / dt,
    hts_dat = 78013,
    se_hts = 78013 * 0.1
  ),
  cameroon = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy = round(cbind(2660,7090)),
    num_svy = round(cbind(90,147)),
    yr_hts = c(2021, 2022) + 0.5,
    ind_hts = (c(2021,2022) - start + 0.5) / dt,
    hts_dat = c(15000, 33073),
    se_hts = c(15000,33073) * 0.1
  ),
  cotedivoire = list(
    yr_svy =  2021.5,
    ind_svy = (2021.5 - start) / dt,
    den_svy = round(cbind(2490,3900)),
    num_svy = round(cbind(31,41)),
    yr_hts = c(2018, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(1159, 111184, 117556, 41774, 60154),
    se_hts = c(1159, 111184, 117556, 41774, 60154) * 0.1
  ),
  guinea = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy = round(cbind(3580,3570)),
    num_svy = round(cbind(14,29)),
    yr_hts = c(2019, 2022) + 0.5,
    ind_hts = (c(2019, 2022) - start + 0.5) / dt,
    hts_dat = c(12, 152),
    se_hts = c(12, 152) * 0.1
  ),
  liberia = list(
    yr_svy =  2019.5,
    ind_svy = (2019.5 - start) / dt,
    den_svy = round(cbind(2350,3630)),
    num_svy = round(cbind(29,55)),
    yr_hts = 2023 + 0.5,
    ind_hts = (2023 - start + 0.5) / dt,
    hts_dat = 12129,
    se_hts = 12129 * 0.1
  ),
  senegal = list(
    yr_svy =  c(2017.5, 2023.5),
    ind_svy = (c(2017.5, 2023.5) - start) / dt,
    den_svy = round(cbind(c(2080,4982), c(12000, 6389))),
    num_svy = round(cbind(c(3,5), c(21, 36))),
    yr_hts = c(2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(7307, 18860, 5505, 4056, 11932),
    se_hts = c(7307, 18860, 5505, 4056, 11932) * 0.1
  ),
  southafrica = list(
    yr_svy =  2016.5,
    ind_svy = (2016.5 - start) / dt,
    den_svy = round(cbind(1150,4290)),
    num_svy = round(cbind(30,127)),
    yr_hts = c(2018, 2019, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(1200000, 794034, 913418, 212000),
    se_hts = c(1200000, 794034, 913418, 212000) * 0.1
  ),
  tanzania = list(
    yr_svy =  2022.5,
    ind_svy = (2022.5 - start) / dt,
    den_svy = round(cbind(3110,4980)),
    num_svy = round(cbind(141,160)),
    yr_hts = c(2018, 2019, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(25810, 14940, 19000, 38717, 809603, 1447029),
    se_hts = c(25810, 14940, 19000, 38717, 809603, 1447029) * 0.1
  ),
  namibia = list(
    yr_svy =  2017.5,
    ind_svy = (2017.5 - start) / dt,
    den_svy = round(cbind(3651,3881)),
    num_svy = round(cbind(148,93)),
    yr_hts = c(2018, 2020, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2018, 2020, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(3910, 40075, 47258, 130000, 27974),
    se_hts = c(3910, 40075, 47258, 130000, 27974) * 0.1
  ),
  botswana = list(
    yr_svy =  2021.5,
    ind_svy = (2021.5 - start) / dt,
    den_svy = round(cbind(3108,2911)),
    num_svy = round(cbind(59,68)),
    yr_hts = c(2019, 2021, 2022, 2023) + 0.5,
    ind_hts = (c(2019, 2021, 2022, 2023) - start + 0.5) / dt,
    hts_dat = c(7000, 3848, 8403, 16405),
    se_hts = c(7000, 3848, 8403, 16405) * 0.1
  ),
  guineabissau = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy = round(cbind(376,2518)),
    num_svy = round(cbind(15,63)),
    yr_hts = 2020 + 0.5,
    ind_hts = (2020 - start + 0.5) / dt,
    hts_dat = 37500,
    se_hts = 37500 * 0.1
  ),
  drc = list(
    yr_svy =  2018.5,
    ind_svy = (2018.5 - start) / dt,
    den_svy = round(cbind(4891,17405)),
    num_svy = round(cbind(146,296)),
    yr_hts = c(2018, 2020, 2021) + 0.5,
    ind_hts = (c(2018, 2020, 2021) - start + 0.5) / dt,
    hts_dat = c(500, 7158, 7480),
    se_hts = c(500, 7158, 7480) * 0.1
  ),
  eswatini = list(
    yr_svy = c(2021.5, 2022.5),
    ind_svy = (c(2021.5, 2022.5) - start) / dt,
    den_svy = round(cbind(c(2040, 1327), c(3394, 1606))),
    num_svy = round(cbind(c(399, 339), c(694, 494))),
    yr_hts = c(2018, 2019, 2020, 2021, 2022) + 0.5,
    ind_hts = (c(2018, 2019, 2020, 2021, 2022) - start + 0.5) / dt,
    hts_dat = c(33159, 32531, 191990, 78570, 111912),
    se_hts = c(33159, 32531, 191990, 78570, 111912) * 0.1
  ),
  benin = list(
    yr_svy =  2017.5,
    ind_svy = (2017.5 - start) / dt,
    den_svy = round(cbind(6766,12443)),
    num_svy = round(cbind(33,77)),
    yr_hts = c(2022, 2023) + 0.5,
    ind_hts = (c(2022, 2023) - start + 0.5) / dt,
    hts_dat = c(5173, 8149),
    se_hts = c(5173, 8149) * 0.1
  )
)



# ----sum across all age groups for male and female----
svy_m_full <- rstan::summary(fit, "svy_prd_m", probs = c(0.025, 0.5, 0.975))$summary
svy_m_full <- as.data.frame(svy_m_full)
svy_m_full$param <- rownames(svy_m_full)


# splitting out each country, then each age group
n_cnt <- length(countries)
svy_m_list <- vector("list", n_cnt)  # each element is a list of 4 data frames (age groups)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), svy_m_full$param)
  tmp_c <- svy_m_full[ix_c, ]
  
  # separateing the 4 age groups
  ages <- vector("list", 4)
  for (a in 1:4) {
    ix_a <- grepl(paste0(",", a, "\\]$"), tmp_c$param)
    ages[[a]] <- tmp_c[ix_a, ]
  }
  svy_m_list[[c]] <- ages
}

# similarly for women
svy_f_full <- rstan::summary(fit, "svy_prd_f", probs = c(0.025, 0.5, 0.975))$summary
svy_f_full <- as.data.frame(svy_f_full)
svy_f_full$param <- rownames(svy_f_full)

svy_f_list <- vector("list", n_cnt)
for (c in seq_len(n_cnt)) {
  ix_c <- grepl(paste0("\\[", c, ","), svy_f_full$param)
  tmp_c <- svy_f_full[ix_c, ]
  
  ages <- vector("list", 4)
  for (a in 1:4) {
    ix_a <- grepl(paste0(",", a, "\\]$"), tmp_c$param)
    ages[[a]] <- tmp_c[ix_a, ]
  }
  svy_f_list[[c]] <- ages
}

# new code for aggregation across all age groups for male and female
time_vec <- time  
n_time <- length(time_vec)

# aggregated results in a list of length n_cnt
svy_m_agg <- vector("list", n_cnt)  # each element: data.frame with columns "time", "p2.5", "p50", "p97.5"
svy_f_agg <- vector("list", n_cnt)  # same for women

for (c_idx in seq_len(n_cnt)) {
  # population for weighting
  pop_c <- pop[[c_idx]]  # 4 x 2 matrix
  pop_m <- pop_c[,1]      # men in each of the 4 age groups
  pop_f <- pop_c[,2]      # women
  
  # retrieve the 4 data frames for men:
  # each df has n_time rows, columns: mean, se_mean, sd, 2.5%, 25%, 50%, 75%, 97.5%, ...
  age_df_m <- svy_m_list[[c_idx]]  # length=4
  age_df_f <- svy_f_list[[c_idx]]
  
  # numeric vectors for aggregated 2.5%, 50%, 97.5%
  p2.5_m <- p50_m <- p97.5_m <- numeric(n_time)
  p2.5_f <- p50_f <- p97.5_f <- numeric(n_time)
  
  # Weighted sums of proportions:
  sum_pop_m <- sum(pop_m)
  sum_pop_f <- sum(pop_f)
  
  for (i in seq_len(n_time)) {
    # For men, each age group a:
    #   proportion in age_df_m[[a]]$`50%`[i] weighted by pop_m[a]
    
    # 2.5% for men
    num_2.5_m <- 0
    num_50_m  <- 0
    num_97.5_m<- 0
    for (a in 1:4) {
      num_2.5_m  <- num_2.5_m  + pop_m[a] * age_df_m[[a]]$`2.5%`[i]
      num_50_m   <- num_50_m   + pop_m[a] * age_df_m[[a]]$`50%`[i]
      num_97.5_m <- num_97.5_m + pop_m[a] * age_df_m[[a]]$`97.5%`[i]
    }
    p2.5_m[i]  <- num_2.5_m  / sum_pop_m
    p50_m[i]   <- num_50_m   / sum_pop_m
    p97.5_m[i] <- num_97.5_m / sum_pop_m
    
    num_2.5_f  <- 0
    num_50_f   <- 0
    num_97.5_f <- 0
    for (a in 1:4) {
      num_2.5_f  <- num_2.5_f  + pop_f[a] * age_df_f[[a]]$`2.5%`[i]
      num_50_f   <- num_50_f   + pop_f[a] * age_df_f[[a]]$`50%`[i]
      num_97.5_f <- num_97.5_f + pop_f[a] * age_df_f[[a]]$`97.5%`[i]
    }
    p2.5_f[i]  <- num_2.5_f  / sum_pop_f
    p50_f[i]   <- num_50_f   / sum_pop_f
    p97.5_f[i] <- num_97.5_f / sum_pop_f
  }
  
  # combining into data frames
  svy_m_agg[[c_idx]] <- data.frame(
    time   = time_vec,
    p2.5   = p2.5_m,
    p50    = p50_m,
    p97.5  = p97.5_m
  )
  svy_f_agg[[c_idx]] <- data.frame(
    time   = time_vec,
    p2.5   = p2.5_f,
    p50    = p50_f,
    p97.5  = p97.5_f
  )
}


png("aggregate_survey_plots2.png", width=14, height=28, units="in", res=300)
par(mfrow=c(9,3), mar=c(4,4,2,1))  

cnt_lowercase <- c("kenya", "ghana", "malawi", "madagascar", "zimbabwe",
                   "sierraleone", "zambia", "mali", "uganda",
                   "lesotho", "mozambique", "rwanda",
                   "burkinafaso", "burundi", "cameroon", "cotedivoire",
                   "guinea", "liberia", "senegal", "southafrica","tanzania",
                   "namibia", "botswana", "guineabissau","drc", "eswatini", "benin")
alphabetical_cnt <- order(cnt_lowercase)

for (c_idx in alphabetical_cnt) {
  men_df  <- svy_m_agg[[c_idx]]  # columns = time, p2.5, p50, p97.5
  women_df<- svy_f_agg[[c_idx]]
  
  # observed survey data
  obs_yr <- cnt_data[[c_idx]]$yr_svy  
  obs_m  <- cnt_data[[c_idx]]$num_svy[,1] / cnt_data[[c_idx]]$den_svy[,1]  # men
  obs_f  <- cnt_data[[c_idx]]$num_svy[,2] / cnt_data[[c_idx]]$den_svy[,2]  # women
  
  ymax <- max(men_df$p97.5, women_df$p97.5,
              obs_m, obs_f, na.rm = TRUE) * 1.05
  plot(NA, xlim = range(time), ylim = c(0, ymax),
         xlab="Year", ylab="Proporition of HIVST use",
       main=paste("Country:", countries[c_idx]))
  
  # men line
  lines(men_df$time, men_df$p50, col="blue", lwd=2)
  polygon(x = c(men_df$time, rev(men_df$time)),
          y = c(men_df$p2.5, rev(men_df$p97.5)),
          col = adjustcolor("blue", alpha.f=0.3),
          border=NA)
  points(obs_yr, obs_m, pch=19, col="blue")
  
  # women line
  lines(women_df$time, women_df$p50, col="red", lwd=2)
  polygon(x = c(women_df$time, rev(women_df$time)),
          y = c(women_df$p2.5, rev(women_df$p97.5)),
          col = adjustcolor("red", alpha.f=0.3),
          border=NA)
  points(obs_yr+0.2, obs_f, pch=19, col="red") # offset a bit on x-axis
  
  legend("topleft", c("Men","Women"), 
         col=c("blue","red"), lwd=2, bty="n")
}
dev.off()

#---side by side age aggregated men women plot and program data fit----
plot_survey_agg_one_country <- function(
    c_idx,
    svy_m_agg, svy_f_agg,
    time, 
    cnt_data, 
    countries
) {
  # Extracting the aggregated men/women data frames
  men_df   <- svy_m_agg[[c_idx]]   # columns: time, p2.5, p50, p97.5
  women_df <- svy_f_agg[[c_idx]]
  
  # Observed data
  obs_yr <- cnt_data[[c_idx]]$yr_svy  
  obs_m  <- cnt_data[[c_idx]]$num_svy[,1] / cnt_data[[c_idx]]$den_svy[,1]  # men
  obs_f  <- cnt_data[[c_idx]]$num_svy[,2] / cnt_data[[c_idx]]$den_svy[,2]  # women
  
  
  plot(
    x    = NA,
    y    = NA,
    xlim = range(time),
    ylim = c(0,ymax),
    xlab = "Year",
    ylab = "Proportion of HIVST use",
    main = paste("HIVST Uptake -", countries[c_idx])
  )
  
  # men 
  lines(men_df$p50 ~ men_df$time, col="blue", lwd=2)
  polygon(
    x = c(men_df$time, rev(men_df$time)),
    y = c(men_df$p2.5, rev(men_df$p97.5)),
    col = adjustcolor("blue", alpha.f=0.3),
    border=NA
  )
  points(obs_yr, obs_m, pch=19, col="blue")
  
  # women 
  lines(women_df$p50 ~ women_df$time, col="red", lwd=2)
  polygon(
    x = c(women_df$time, rev(women_df$time)),
    y = c(women_df$p2.5, rev(women_df$p97.5)),
    col = adjustcolor("red", alpha.f=0.3),
    border=NA
  )
  points(obs_yr+0.2, obs_f, pch=19, col="red")
  
  legend("topleft", c("Men","Women"), col=c("blue","red"), lwd=2, bty="n")
}


plot_hts_one_country <- function(
    c_idx,
    hts_list,
    cnt_data,
    time,
    countries
) {
  df_c <- hts_list[[c_idx]]  # columns: 2.5%, 50%, 97.5%, etc.
  
  plot(
    x    = time,
    y    = df_c$`50%`,
    type = "l",
    col  = "blue",
    lwd  = 2,
    main = paste("HIVST kits -", countries[c_idx]),
    xlab = "Year",
    ylab = "Number of HIVST kits",
    ylim = c(0, max(df_c$`97.5%`, na.rm=TRUE))
  )
  
  polygon(
    x = c(time, rev(time)),
    y = c(df_c$`2.5%`, rev(df_c$`97.5%`)),
    col = adjustcolor("blue", alpha.f = 0.3),
    border = NA
  )
  
  # observed program data
  t_obs   <- cnt_data[[c_idx]]$yr_hts
  obs_hts <- cnt_data[[c_idx]]$hts_dat
  points(t_obs, obs_hts, pch = 16, col = "red")
}



N <- length(alphabetical_cnt)  # 27

png("svypgmfit2.png",
    width = 20, height = 28,   
    units = "in", res = 300)

par(mfrow = c(14, 4), mar = c(4, 4, 2, 1))

i <- 1
while (i <= N) {
  c1 <- alphabetical_cnt[i]
  
  plot_survey_agg_one_country(
    c_idx     = c1,
    svy_m_agg = svy_m_agg,
    svy_f_agg = svy_f_agg,
    time      = time,
    cnt_data  = cnt_data,
    countries = countries
  )
  plot_hts_one_country(
    c_idx    = c1,
    hts_list = hts_list,
    cnt_data = cnt_data,
    time     = time,
    countries= countries
  )
  
  if (i + 1 <= N) {
    c2 <- alphabetical_cnt[i + 1]
    
    plot_survey_agg_one_country(
      c_idx     = c2,
      svy_m_agg = svy_m_agg,
      svy_f_agg = svy_f_agg,
      time      = time,
      cnt_data  = cnt_data,
      countries = countries
    )
    plot_hts_one_country(
      c_idx    = c2,
      hts_list = hts_list,
      cnt_data = cnt_data,
      time     = time,
      countries= countries
    )
  }
  
  i <- i + 2
}

dev.off()



#------------------------------------------------------------------------------
#---2 separate plots
## ----------------------------------------------------------

countries[countries %in% c("Democratic Republic of Congo",
                           "Democratic Republic of the Congo")] <- "DRC"

countries[countries %in% c("United Republic of Tanzania",
                           "United Republic of Tanzania")] <- "Tanzania"

plot_survey_agg_one_country <- function(
    c_idx, svy_m_agg, svy_f_agg, time,
    cnt_data, countries, ylim = NULL)
{
  men_df   <- svy_m_agg[[c_idx]]
  women_df <- svy_f_agg[[c_idx]]
  
  obs_yr <- cnt_data[[c_idx]]$yr_svy
  obs_m  <- cnt_data[[c_idx]]$num_svy[,1] / cnt_data[[c_idx]]$den_svy[,1]
  obs_f  <- cnt_data[[c_idx]]$num_svy[,2] / cnt_data[[c_idx]]$den_svy[,2]
  
  ## ――dynamic y-limit ----------
  if (is.null(ylim)) {
    ymax <- max(men_df$p97.5, women_df$p97.5,
                obs_m, obs_f, na.rm = TRUE) * 1.05
    ylim <- c(0, ymax)
  }

  plot(NA, xlim = range(time), ylim = ylim,
       xlab = "Year", ylab = "HIVST use proportion",
       main = paste("HIVST uptake:", countries[c_idx]))
  
  lines(men_df$p50 ~ men_df$time, col = "blue", lwd = 2)
  polygon(c(men_df$time, rev(men_df$time)),
          c(men_df$p2.5, rev(men_df$p97.5)),
          col = adjustcolor("blue", 0.30), border = NA)
  points(obs_yr, obs_m, pch = 19, col = "blue")
  
  lines(women_df$p50 ~ women_df$time, col = "red",  lwd = 2)
  polygon(c(women_df$time, rev(women_df$time)),
          c(women_df$p2.5, rev(women_df$p97.5)),
          col = adjustcolor("red", 0.30),  border = NA)
  points(obs_yr + 0.2, obs_f, pch = 19, col = "red")
  
  legend("topleft", c("Men","Women"), col = c("blue","red"),
         lwd = 2, bty = "n", cex = 0.8)
}

options(scipen=999)
plot_hts_one_country <- function(
    c_idx, hts_list, cnt_data, time, countries, ylim = NULL)
{
  df_c <- hts_list[[c_idx]]
  
  if (is.null(ylim))
    ylim <- c(0, max(df_c$`97.5%`, cnt_data[[c_idx]]$hts_dat, na.rm = TRUE))
  
  plot(time, df_c$`50%`, type = "l", col = "blue", lwd = 2,
       main = paste("HIVST kits:", countries[c_idx]),
       xlab = "Year", ylab = "Number of kits", ylim = ylim)
  
  polygon(c(time, rev(time)),
          c(df_c$`2.5%`, rev(df_c$`97.5%`)),
          col = adjustcolor("blue", 0.30), border = NA)
  
  points(cnt_data[[c_idx]]$yr_hts,
         cnt_data[[c_idx]]$hts_dat,
         pch = 16, col = "red")
}




## -----------------------------------------------------------------
make_country_page <- function(idx_vec, file_name,
                              svy_m_agg, svy_f_agg,
                              hts_list, cnt_data,
                              time, countries)
{
  n_cnt   <- length(idx_vec)
  n_row   <- ceiling(n_cnt / 2)      # two countries per row
  n_col   <- 4                       # (survey, hts) × 2
  
  png(file_name,
      width  = 8,     # width
      height = 10.5,  
      units  = "in", res = 300)
  
  par(mfrow = c(n_row, n_col),
      mar   = c(3.2, 3.2, 2, 1),     
      mgp   = c(2, 0.6, 0),          
      cex   = 0.5)                   
  
  i <- 1
  while (i <= n_cnt) {
    c1 <- idx_vec[i]
    
    plot_survey_agg_one_country(
      c_idx     = c1,
      svy_m_agg = svy_m_agg,
      svy_f_agg = svy_f_agg,
      time      = time,
      cnt_data  = cnt_data,
      countries = countries)
    
    plot_hts_one_country(
      c_idx     = c1,
      hts_list  = hts_list,
      cnt_data  = cnt_data,
      time      = time,
      countries = countries)
    
    if (i + 1 <= n_cnt) {
      c2 <- idx_vec[i + 1]
      
      plot_survey_agg_one_country(
        c_idx     = c2,
        svy_m_agg = svy_m_agg,
        svy_f_agg = svy_f_agg,
        time      = time,
        cnt_data  = cnt_data,
        countries = countries)
      
      plot_hts_one_country(
        c_idx     = c2,
        hts_list  = hts_list,
        cnt_data  = cnt_data,
        time      = time,
        countries = countries)
    } else {
      ## keep the grid square if n_cnt is odd
      for (k in 1:2) plot.new()
    }
    
    i <- i + 2
  }
  
  dev.off()
}
cnt_first14 <- alphabetical_cnt[1:14]
cnt_last13  <- alphabetical_cnt[15:27]

make_country_page(cnt_first14, "svy_hts_first14.png",
                  svy_m_agg, svy_f_agg, hts_list,
                  cnt_data, time, countries)

make_country_page(cnt_last13,  "svy_hts_last13.png",
                  svy_m_agg, svy_f_agg, hts_list,
                  cnt_data, time, countries)



