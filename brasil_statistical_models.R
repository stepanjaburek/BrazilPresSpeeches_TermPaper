# -----------------------------------------
library(glmmTMB)
library(stargazer)
library(sjPlot)
library(MASS)
library(data.table)
library(tidyverse)
library(flexplot)
library(pscl)

setwd("C:/Users/stepa/OneDrive/Dokumenty/Brazil_Cph")
brasil<-fread("brasil_final.csv")
#------------------------------------------------

brasil$working_bin <- ifelse(brasil$working_class_mentions>0,"1","0")
brasil$working_mag <- ifelse(brasil$working_class_mentions>0,"1","0")
brasil$working_bin <- as.numeric(brasil$working_bin)
brasil <- brasil %>% 
  mutate(year_centered = year-mean(year))
write.csv(brasil,"brasil_final.csv")
#--------------------------------------------------------
# Working class
#--------------------- Poissson ------------------------
working_poisson <- glm(working_class_mentions ~ year_centered  +
                         party + election_period + inflation + unemployment,
                       data = brasil,
                       family = "poisson",
                       offset = log(word_count))
summary(working_poisson)
mean(brasil$working_class_mentions)
var(brasil$working_class_mentions) # overdispersion -> negative binomial

#--------------------- Negative Binomial ------------------------
working_nb <- glm.nb(working_class_mentions ~ year_centered  + 
                       party + election_period + inflation + unemployment + 
                     offset(log(word_count)),
                   data = brasil)
summary(working_nb)
mean(brasil$working_class_mentions == 0) # probably too many 0s -> hurdle negbin

#--------------------- Hurdle model with truncated Negative Binomial ------------------------
working_hurdle <- hurdle(working_class_mentions ~ year_centered + 
                           party + election_period + inflation + unemployment + 
                        offset(log(word_count)),
                      data = brasil,
                       dist = "negbin")
summary(working_hurdle)
cor(as.data.frame(brasil)[, c("working_class_mentions","year_centered", "election_period", "inflation", "unemployment")], use = "complete.obs")


#-----------------------------------------------------
# Middle class
#--------------------- Poissson ------------------------
middle_poisson <- glm(middle_class_mentions ~ year_centered + 
                        party + election_period + inflation + unemployment,
               data = brasil,
               family = "poisson",
               offset = log(word_count))
summary(middle_poisson)
mean(brasil$middle_class_mentions)
var(brasil$middle_class_mentions) # overdispersion -> negative binomial

#--------------------- Negative Binomial ------------------------
middle_nb <- glm.nb(middle_class_mentions ~ year_centered + 
                      party + election_period + inflation + unemployment + 
                     offset(log(word_count)),
                   data = brasil)
summary(middle_nb)
mean(brasil$middle_class_mentions == 0) # probably too many 0s -> hurdle negbin



#--------------------- Hurdle model with truncated Negative Binomial ------------------------
middle_hurdle <- hurdle(middle_class_mentions ~ year_centered +
                          party + election_period + inflation + unemployment + 
                        offset(log(word_count)),
                      data = brasil,
                      dist = "negbin")

summary(middle_hurdle)

#Predict outcomes
brasil$middle_poisson <- predict(middle_poisson, 
                          newdata = brasil,
                          "response")
#Plot two overlayed histograms
brasil %>%
  ggplot() +
  geom_histogram(aes(x = middle_class_mentions),
                 alpha=0.5)+
  geom_histogram(aes(x = middle_poisson),
                 fill = "red", 
                 alpha =  0.3)+
  theme_minimal()

#-----------------------------------------------------
# Elites
#--------------------- Poissson ------------------------
elite_poisson <- glm(elite_mentions ~ year_centered + 
                       party + election_period + inflation + unemployment,
                      data = brasil,
                      family = "poisson",
                      offset = log(word_count))
summary(elite_poisson)
mean(brasil$elite_mentions)
var(brasil$elite_mentions) # maybe not overdispersion -> negative binomial

#--------------------- Negative Binomial ------------------------
elite_nb <- glm.nb(elite_mentions ~ year_centered +
                     party + election_period + inflation + unemployment + 
                      offset(log(word_count)),
                    data = brasil)
summary(elite_nb)
mean(brasil$elite_mentions == 0) # probably too many 0s -> hurdle poisson/negbin



#--------------------- Hurdle model with truncated Negative Binomial ------------------------
elite_hurdle <- hurdle(elite_mentions ~ year_centered + 
                         party + election_period + inflation + unemployment + 
                          offset(log(word_count)),
                        data = brasil,
                        dist = "poisson")

summary(elite_hurdle)
rootogram(working_hurdle)
plot(elite_hurdle)
#----------------------------------------------------------
stargazer:stargazer(working_poisson, working_nb, working_hurdle,
                    title = "Working class mentions, poisson, negative binomial model, hurdle",
                    dep.var.caption = "Number of working class mentions",
                    zero.component = T,
                    digits = 2,
                    no.space = TRUE,
                    column.sep.width = "4pt",
                    omit.stat = c("f", "ser", "adj.rsq"),
                    out = "working.tex")
stargazer:stargazer( working_hurdle,
                    title = "Working class mentions, poisson, negative binomial model, hurdle",
                    dep.var.caption = "Number of working class mentions",
                    zero.component = F,
                    digits = 2,
                    no.space = TRUE,
                    column.sep.width = "4pt",
                    omit.stat = c("f", "ser", "adj.rsq"),
                    out = "working_count.tex")
#----------------------------------------------------------
#----------------------------------------------------------
stargazer:stargazer(middle_poisson, middle_nb, middle_hurdle,
                    title = "Middle class mentions, poisson, negative binomial model, hurdle",
                    dep.var.caption = "Number of Middle class mentions",
                    zero.component = T,
                    digits = 2,
                    no.space = TRUE,
                    column.sep.width = "4pt",
                    omit.stat = c("f", "ser", "adj.rsq"),
                    out = "middle.tex")
stargazer:stargazer( middle_hurdle,
                     title = "Middle class mentions, poisson, negative binomial model, hurdle",
                     dep.var.caption = "Number of Middle class mentions",
                     zero.component = F,
                     digits = 2,
                     no.space = TRUE,
                     column.sep.width = "4pt",
                     omit.stat = c("f", "ser", "adj.rsq"),
                     out = "middle_count.tex")
#----------------------------------------------------------
stargazer:stargazer(elite_poisson, elite_nb, elite_hurdle,
                    title = "Elite mentions, poisson, negative binomial model, hurdle",
                    dep.var.caption = "Number of Elite class mentions",
                    zero.component = T,
                    digits = 2,
                    no.space = TRUE,
                    column.sep.width = "4pt",
                    omit.stat = c("f", "ser", "adj.rsq"),
                    out = "elite.tex")
stargazer:stargazer( elite_hurdle,
                     title = "Middle class mentions, poisson, negative binomial model, hurdle",
                     dep.var.caption = "Number of Middle class mentions",
                     zero.component = F,
                     digits = 2,
                     no.space = TRUE,
                     column.sep.width = "4pt",
                     omit.stat = c("f", "ser", "adj.rsq"),
                     out = "elite_count.tex")
#----------------------------------------------------------


#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------
# Sentiment models

elite <- fread("debate_sentiment_elite_class.csv")
middle <- fread("debate_sentiment_middle_class.csv")
working <- fread("debate_sentiment_working_class.csv")
yearly_unemployment <- data.frame(year2, unemployment)

new_vars <- brasil %>% dplyr::select(16:25,28)

elite <- elite %>% 
  left_join(new_vars, by = "id")
middle <- middle %>% 
  left_join(new_vars, by = "id")
working <- working %>% 
  left_join(new_vars, by = "id")


# ------------------------------------
# define ordered logit
working <- 
  working %>%
  mutate(label = as.factor(label),
         label = ordered(label))
head(working$label)


mod0.ord <- polr(label ~ year_centered + party + election_period + inflation + unemployment,
                 data = working,
                 Hess = T)

summary(mod0.ord)
stargazer::stargazer(mod0.ord,
                     type= "text")

working <- working %>% filter(label!="neutral")
working$label_bin <- ifelse(working$label == "positive", "1","0")
class(working$label_bin)
working$label_bin <- as.numeric(working$label_bin)
working_log <- glm(label_bin ~ year_centered + party + election_period + inflation + unemployment,
                       data = working,
                       family = "binomial")
summary(working_log)
#---------------------------------------
middle <- 
  middle %>%
  mutate(label = as.factor(label),
         label = ordered(label))
head(middle$label)


mod0.ord <- polr(label ~ year_centered + party + election_period + inflation + unemployment,
                 data = middle,
                 Hess = T)

summary(mod0.ord)


elite <- 
  elite %>%
  mutate(label = as.factor(label),
         label = ordered(label))
head(elite$label)


mod.ord <- polr(label ~ year_centered + party + election_period + inflation + unemployment,
                 data = elite,
                 Hess = T)

summary(mod.ord)
stargazer::stargazer(mod.ord,
                     type="text")

install.packages("ggpredict")
library(ggeffects)
scenario <- ggpredict(mod.ord,
                      terms = list(
                        year_centered = seq(-19.400,15.600,1)
                      ))
summary(elite$year_centered)
scenario %>%
  ggplot +
  geom_area(aes(y = predicted,
                x = x,
                fill = response.level))


# ----------------------------------------------------------------
# GLM vs ANOVA


bras_yearly <- brasil %>% 
  group_by(year, party_family ) %>% 
  reframe( 
    working_prop = sum(working_class_mentions >=1) / n(),
    middle_prop = sum(middle_class_mentions >=1) / n(),
    elite_prop = sum(elite_mentions >=1) / n()
    
  ) 
kruskal.test(elite_salience ~ party_family, data = bras_salience)
summary(lm)
kruskal.test(middle_salience ~ party_family, data = bras_salience)
summary(lm)
kruskal.test(working_salience ~ party_family, data = bras_salience)
summary(lm)

stargazer:stargazer( el,mid,work,
                     title = "Social class salience (% of speeches/year) - no intercept GLM",
                     zero.component = F,
                     digits = 2,
                     no.space = TRUE,
                     column.sep.width = "4pt",
                     omit.stat = c("f", "ser", "adj.rsq"),
                     out = "class.tex")

bras_yearly <- brasil %>% 
  group_by(year, social_background) %>% 
  reframe( 
    working_prop = sum(working_class_mentions >=1) / n(),
    middle_prop = sum(middle_class_mentions >=1) / n(),
    elite_prop = sum(elite_mentions >=1) / n()
    
  ) 

bras_yearly$social_background<-  as.factor(bras_yearly$social_background)
wilcox.test(working_prop ~ social_background, data = bras_yearly)
summary(work)
wilcox.test(middle_prop ~ social_background, data = bras_yearly)
summary(m)
wilcox.test(elite_prop ~ social_background, data = bras_yearly)
summary(m)

3.06e-05
boxplot(working_prop ~ party_family, data = bras_yearly)

stargazer:stargazer(work,mid,el,
                     title = "Social class salience (percentage of speeches/year)",
                     zero.component = F,
                     digits = 2,
                     no.space = TRUE,
                     column.sep.width = "4pt",
                    omit = "Constant",
                     omit.stat = c("f", "ser", "adj.rsq"),
                     out = "lula.tex")

lm<-glm(elite_per_1k ~ party_family, data = bras_yearly, family = "beta")
summary(lm)
lm<-lm(middle_per_1k ~ party_family, data = bras_yearly)
summary(lm)
lm <- lm(working_per_1k ~ party_family - 1, data = bras_yearly)
summary(lm)
library(emmeans)
emm <- emmeans(aov_result, ~ party_family)
pairs(emm, adjust = "tukey")
# --------------------------------------
# time series
library(changepoint)
library(strucchange)

bras_yearly <- brasil %>% 
  group_by(year, party_family) %>% 
  reframe( total_words = sum(word_count),
           working = sum(working_class_mentions),
           middle = sum(middle_class_mentions),
           elite = sum(elite_mentions),
           .groups = "drop"
  ) %>%
  # Then normalize per 1000 words
  mutate(
    working_per_1k = working * 1000 / total_words,
    middle_per_1k = middle * 1000 / total_words,
    elite_per_1k = elite * 1000 / total_words
  ) 

yearly_data <- brasil %>%
  group_by(year) %>%
  summarise(
    total_speeches = n(),
    elite_salience = mean(elite_mentions > 0),
    middle_salience = mean(middle_class_mentions > 0),
    working_salience = mean(working_class_mentions > 0),
    .groups = 'drop'
  )

ts_data <- ts(yearly_data$elite_salience, start = min(yearly_data$year), frequency = 1)
bp_test <- breakpoints(ts_data ~ 1, h = 0.15) 
summary(bp_test)
stargazer::stargazer(bp_test)
plot(bp_test)
?breakpoints

#----------------------------------
ts_data <- ts(yearly_data$middle_salience, start = min(yearly_data$year), frequency = 1)
bp_test <- breakpoints(ts_data ~ 1, h = 0.15) 
summary(bp_test)
plot(bp_test)

#----------------------------------
ts_data <- ts(yearly_data$working_salience, start = min(yearly_data$year), frequency = 1)
bp_test <- breakpoints(ts_data ~ 1, h = 0.15) 
summary(bp_test)
plot(bp_test)



library(scales)
# Prepare the data for plotting
plot_data <- yearly_data %>%
  select(year, elite_salience, middle_salience, working_salience) %>%
  pivot_longer(cols = c(elite_salience, middle_salience, working_salience),
               names_to = "class_type", 
               values_to = "salience") %>%
  mutate(
    class_type = case_when(
      class_type == "elite_salience" ~ "Elites",
      class_type == "middle_salience" ~ "Middle Class", 
      class_type == "working_salience" ~ "Working Class"
    ),
    class_type = factor(class_type, levels = c("Elites", "Middle Class", "Working Class"))
  )

# Define breakpoints from BIC analysis
breakpoints <- data.frame(
  class_type = c("Elites", "Elites", "Elites", 
                 "Middle Class", "Middle Class",
                 "Working Class", "Working Class"),
  breakpoint = c(2002, 2010, 2015,
                 2002, 2015,
                 2002, 2014),
  class_type = factor(c("Elites", "Elites", "Elites", 
                        "Middle Class", "Middle Class",
                        "Working Class", "Working Class"),
                      levels = c("Elites", "Middle Class", "Working Class"))
)

# Create the main plot
ggplot(plot_data, aes(x = year, y = salience, color = class_type)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.7) +
  
  # Add structural breakpoints as vertical lines
  geom_vline(data = breakpoints, 
             aes(xintercept = breakpoint, color = class_type),
             linetype = "dashed", alpha = 0.7, size = 0.8) +
  
  # Add presidential transitions
  geom_vline(xintercept = c(1990,1993,1995, 2003, 2011, 2016, 2019),
             color = "gray15", linetype = "dotted", alpha = 1,size = 0.5) +
  
  # Using fixed scales for all facets (same y-axis)
  facet_wrap(~ class_type, ncol = 1, scales = "fixed") +
  
  scale_color_manual(values = c("Elites" = "#4daf4a", 
                                "Middle Class" = "#377eb8", 
                                "Working Class" = "#e41a1c")) +
  
  scale_x_continuous(breaks = seq(1986, 2020, 4)) +
  scale_y_continuous(labels = percent) +
  
  labs(
    title = "Social Class Salience in Brazilian Presidential Speeches (1985-2020)",
    subtitle = "Dashed lines = BIC Structural breakpoints, Dotted lines = Presidential transitions",
    x = "Year",
    y = "Salience (%)",
    color = "Social Class"
  ) +
  
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 14),
          axis.title = element_text(size = 14),
          title = element_text(size = 18),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# -----------------------------------------------------------------------
# sentiment time series

work_sent <- working %>% 
  group_by(year, party_family ) %>% 
  reframe( negative = sum(label=="negative"),
           n = length(id),       
           positive = sum(label=="positive"),
           sentiment = (positive-negative) / n)


ts_data <- ts(work_sent$sentiment, start = min(work_sent$year), frequency = 1)
bp_test <- breakpoints(ts_data ~ 1, h = 0.15) # minimum segment size = 15% of data
summary(bp_test)
plot(bp_test)


#----------------------------------

middle_sent <- middle %>% 
  group_by(year, party_family ) %>% 
  reframe( negative = sum(label=="negative"),
           n = length(id),
           positive = sum(label=="positive"),
           sentiment = (positive-negative) / n)

ts_data <- ts(middle_sent$sentiment, start = min(middle_sent$year), frequency = 1)
bp_test <- breakpoints(ts_data ~ 1, h = 0.15) # minimum segment size = 15% of data
summary(bp_test)
plot(bp_test)

#----------------------------------
elite_sent <- elite %>% 
  group_by(year, party_family ) %>% 
  reframe( negative = sum(label=="negative"),
           n = length(id),
           positive = sum(label=="positive"),
           sentiment = (positive-negative) / n)


ts_data <- ts(elite_sent$sentiment, start = min(elite_sent$year), frequency = 1)
bp_test <- breakpoints(ts_data ~ 1, h = 0.15) # minimum segment size = 15% of data
summary(bp_test)
plot(bp_test)





lm<-lm(sentiment ~ party_family, data = elite_sent)
summary(lm)
lm<-lm(sentiment ~ party_family, data = work_sent)
summary(lm)
lm<-lm(sentiment ~ party_family, data = middle_sent)
summary(lm)

bras_yearly <- brasil %>% 
  group_by(year) %>% 
  reframe( total_words = sum(word_count),
           working = sum(working_class_mentions),
           middle = sum(middle_class_mentions),
           elite = sum(elite_mentions),
           .groups = "drop"
  ) %>%
  # Then normalize per 1000 words
  mutate(
    working_per_1k = working * 1000 / total_words,
    middle_per_1k = middle * 1000 / total_words,
    elite_per_1k = elite * 1000 / total_words
  ) 
