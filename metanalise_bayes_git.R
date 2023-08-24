library(meta)
library(readxl)
library(ggplot2)
library(tidyr)
library(tidybayes)
library(stringr)
library(dplyr)
library(brms)
library(ggridges)


setwd("Set your directory here")

#read dataset
toxic <- read_excel("ouctomes.xlsx")

toxic$out_factor <- as.factor(toxic$outcome_code)

# Estimate effect-sizes and respective standard errors
meta_tox <- metabin(tox_sarco, sarcopenia, tox_control, control,
                            data=toxic, random= TRUE, sm="OR")

sme <- summary(meta_tox)
toxic$log_or <- sme$TE
toxic$se_log_or <- sme$seTE

toxic_prev<- filter(toxic, Prevalent==1)


#Random usual model (Bayesian) - most prevalents

meta_prev<- brm(log_or|se(se_log_or) ~ 1 +  (1 | Study) ,
              data = toxic_prev,
              prior = get_prior(log_or|se(se_log_or) ~  + (1 |Study) , data=toxic_prev), 
              control = list(adapt_delta = 0.95),
              iter = 10000, )
meta_prev


#Forest Plot for meta_prev (Figure 1a)

study.draws <- spread_draws(meta_prev, r_Study[Study,], b_Intercept) %>% 
  mutate(b_Intercept = r_Study + b_Intercept)

pooled.effect.draws <- spread_draws(meta_prev, b_Intercept) %>% 
  mutate(Study = "Pooled Effect")

forest.data <- bind_rows(study.draws, 
                         pooled.effect.draws) %>% 
  ungroup() %>%
  mutate(Study = str_replace_all(Study, "[.]", " ")) %>% 
  mutate(Study = reorder(Study, b_Intercept))

forest.data.summary <- group_by(forest.data, Study) %>% 
  mean_qi(b_Intercept)

ggplot(aes(b_Intercept, 
           forcats::fct_relevel(Study, "Pooled Effect")), 
       data = forest.data) +
  
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(meta_prev)[1, 1], 
             color = "grey", size = 1) +
  geom_vline(xintercept = fixef(meta_prev)[1, 3:4], 
             color = "grey", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", 
             size = 1) +
  
  # Add densities
  geom_density_ridges(fill = "grey", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointintervalh(data = forest.data.summary, 
                      size = 1) +
  
  # Add text and Studys
  geom_text(data = mutate_if(forest.data.summary, 
                             is.numeric, round, 2),
            aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), 
                x = Inf), hjust = "inward") +
  labs(x = "Log-odds", # summary measure
       y = element_blank()) +
  theme_classic() +
  
  geom_point(
    data = toxic_prev %>% mutate(StudyOutcomes = str_replace_all(StudyOutcomes, "\\.", " ")), 
    aes(x=log_or), position = position_nudge(y = -.2), shape = 1 
  )
  


#Random usual model (Bayesian) - all outcomes as independent

meta_rm<- brm(log_or|se(se_log_or) ~ +  (1 | StudyOutcomes) ,
                  data = toxic,
                  prior = get_prior(log_or|se(se_log_or) ~  + (1 |StudyOutcomes) , data=toxic), 
                  control = list(adapt_delta = 0.95),
                  iter = 10000, )
meta_rm

#Forest Plot for meta_rm (Figure 1b)

study.draws <- spread_draws(meta_rm, r_StudyOutcomes[StudyOutcomes,], b_Intercept) %>% 
  mutate(b_Intercept = r_StudyOutcomes + b_Intercept)

pooled.effect.draws <- spread_draws(meta_rm, b_Intercept) %>% 
  mutate(StudyOutcomes = "Pooled Effect")

forest.data <- bind_rows(study.draws, 
                         pooled.effect.draws) %>% 
  ungroup() %>%
  mutate(StudyOutcomes = str_replace_all(StudyOutcomes, "[.]", " ")) 
  #mutate(StudyOutcomes = reorder(StudyOutcomes, b_Intercept))

forest.data.summary <- group_by(forest.data, StudyOutcomes) %>% 
  mean_qi(b_Intercept)

ggplot(aes(b_Intercept, 
           forcats::fct_relevel(StudyOutcomes, "Pooled Effect")), 
       data = forest.data) +
  
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(meta_rm)[1, 1], 
             color = "grey", size = 1) +
  geom_vline(xintercept = fixef(meta_rm)[1, 3:4], 
             color = "grey", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", 
             size = 1) +
  
  # # Add densities
  geom_density_ridges(#fill = "grey",
                       rel_min_height = 0.01,
                       col = NA, scale = 1,
                       alpha = 0.8) +
   geom_pointintervalh(data = forest.data.summary,
                      size = 1) +

  # Add text and Studys
  geom_text(data = mutate_if(forest.data.summary, 
                             is.numeric, round, 2),
            aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), 
                x = Inf), hjust = "inward") +
  labs(x = "Log-odds", # summary measure
       y = element_blank()) +
 
   scale_x_continuous(limits = c(-1, 3), breaks = c(-1,0, 1,2))+
  
  theme_classic()



#Bayesian multileval model. Outcomes within studies (3 levels)

meta_study <- brm(log_or|se(se_log_or, sigma=TRUE) ~ +  (1 | Study/ outcome_det) ,
               data = toxic,
       prior = get_prior(log_or|se(se_log_or) ~  + (1 | Study/ outcome_det) , data=toxic), 
               control = list(adapt_delta = 0.95),
               iter = 10000, )
meta_study


#Forest Plot for meta-study (Figure 2)

study.draws <- spread_draws(meta_study, r_Study[Study,], b_Intercept) %>% 
  mutate(b_Intercept = r_Study + b_Intercept)

pooled.effect.draws <- spread_draws(meta_study, b_Intercept) %>% 
  mutate(Study = "Pooled Effect")

forest.data <- bind_rows(study.draws, 
                         pooled.effect.draws) %>% 
  ungroup() %>%
  mutate(Study = str_replace_all(Study, "[.]", " ")) %>% 
  mutate(Study = reorder(Study, b_Intercept))

forest.data.summary <- group_by(forest.data, Study) %>% 
 mean_qi(b_Intercept)

ggplot(aes(b_Intercept, 
           forcats::fct_relevel(Study, "Pooled Effect")), 
       data = forest.data) +
  
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(meta_study)[1, 1], 
             color = "grey", size = 1) +
  geom_vline(xintercept = fixef(meta_study)[1, 3:4], 
             color = "grey", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", 
             size = 1) +
  
  # Add densities
  geom_density_ridges(fill = "grey", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointintervalh(data = forest.data.summary, 
                      size = 1) +
  
  # Add text and Studys
  geom_text(data = mutate_if(forest.data.summary, 
                             is.numeric, round, 2),
            aes(label = glue::glue("{b_Intercept} [{.lower}, {.upper}]"), 
                x = Inf), hjust = "inward") +
  labs(x = "Log-odds", # summary measure
       y = element_blank()) +
  theme_classic() +
  
  geom_point(
    data = toxic %>% mutate(StudyOutcomes = str_replace_all(StudyOutcomes, "\\.", " ")), 
    aes(x=log_or), position = position_nudge(y = -.2), shape = 1 
  )
  


# Posterior distributions for variances (Figure 2)
posterior_samples(meta_study) %>% 
  select(starts_with("sd"), sigma) %>% 
  gather(key, tau) %>% 
  mutate(key = str_remove(key, "sd_") %>% str_remove(., "__Intercept")) %>% 
  
  ggplot(aes(x = tau, fill = key)) +
  geom_density(color = "transparent", alpha = 2/3) +
  scale_fill_viridis_d(NULL, end = .85) +
  scale_y_continuous(NULL, breaks = NULL) +
  xlab(expression(tau)) +
  theme(panel.grid = element_blank()) +
  scale_fill_discrete(labels=c('Between studies', 'Outcome nested in Study'))+
  theme_classic()






#Model checks (not published, but shows model estimation properties)

pp_check(meta_prev, ndraws = 1000)
pp_check(meta_prev, type = "error_hist", ndraws = 10)
pp_check(meta_prev, type = "scatter_avg", ndraws = 1000)
pp_check(meta_prev, type = "stat_2d")
pp_check(meta_prev, type = "rootogram")
pp_check(meta_prev, type = "loo_pit")

pp_check(meta_rm, ndraws = 1000)
pp_check(meta_rm, type = "error_hist", ndraws = 10)
pp_check(meta_rm, type = "scatter_avg", ndraws = 1000)
pp_check(meta_rm, type = "stat_2d")
pp_check(meta_rm, type = "rootogram")
pp_check(meta_rm, type = "loo_pit")

pp_check(meta_study, ndraws = 1000)
pp_check(meta_study, type = "error_hist", ndraws = 10)
pp_check(meta_study, type = "scatter_avg", ndraws = 1000)
pp_check(meta_study, type = "stat_2d")
pp_check(meta_study, type = "rootogram")
pp_check(meta_study, type = "loo_pit")

