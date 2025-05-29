library(ggbeeswarm)
library(forcats)
library(patchwork)
library(data.table)
library(tidyverse)
library(quanteda)
library(flexplot)
library(readxl)


setwd("C:/Users/stepa/OneDrive/Dokumenty/Brazil_Cph")
brasil<-fread("brasil_final.csv") 

#---------------------------------------------------------------------------------------------------


bras_yearly <- brasil %>% 
  group_by(year, party_family ) %>% 
  reframe( 
    working_prop = sum(working_class_mentions >=1) / n(),
    middle_prop = sum(middle_class_mentions >=1) / n(),
    elite_prop = sum(elite_mentions >=1) / n()
  ) 

# Find global y-axis range for consistent scaling
y_min <- 0
y_max <- 1 

# Function to create a plot with shared y-axis
create_class_plot <- function(data, y_var, title) {
  ggplot(data, aes(x = fct_reorder(party_family, .data[[y_var]]), y = .data[[y_var]], color = party_family)) +
    geom_beeswarm(alpha = 1, size = 2) +  
    geom_boxplot(alpha = 0, width = 0.3, notch = FALSE, size = 1.1) +
    theme_minimal() +
    theme(legend.position = "none") +
    ylim(y_min, y_max) + 
    labs(x = "", y = if(y_var == "working_prop") "Salience - % of speeches" else "", 
         title = title) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_color_brewer(palette = "Set2") +
    theme(axis.text = element_text(size = 26),
          axis.title = element_text(size = 28),
          title = element_text(size = 28))
}

# Create the three plots with consistent y-axis
working_plot <- create_class_plot(bras_yearly, "working_prop", "Working Class Mentions") 
middle_plot <- create_class_plot(bras_yearly, "middle_prop", "Middle Class Mentions")
elite_plot <- create_class_plot(bras_yearly, "elite_prop", "Elite Mentions")


# Combine all plots with patchwork
combined_plots <- working_plot + middle_plot + elite_plot +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Class Mentions by Social Background in Brazilian Presidential Speeches",
    theme = theme(
      plot.title = element_text(size = 38, hjust = 0.5),

    )
  )
combined_plots




#----------------------------------------------------------------

bras_yearly <- brasil %>%
  group_by(year, president_surname ) %>% 
  reframe( 
    working_prop = sum(working_class_mentions >=1) / n(),
    middle_prop = sum(middle_class_mentions >=1) / n(),
    elite_prop = sum(elite_mentions >=1) / n()
    ) 


# Find global y-axis range for consistent scaling
y_min <- 0
y_max <- 1 

# Function to create a consistent plot with shared y-axis
create_class_plot <- function(data, y_var, title) {
  ggplot(data, aes(x = fct_reorder(president_surname, .data[[y_var]]), y = .data[[y_var]], color = president_surname)) +
    geom_beeswarm(alpha = 1, size = 3) +  # bigger points
    geom_boxplot(alpha = 0, width = 0.3, notch = FALSE, size = 1.5) +
    theme_minimal() +
    theme(legend.position = "none") +
    ylim(y_min, y_max) +  # Set consistent y limits
    labs(x = "", y = if(y_var == "working_prop") "Salience - % of speeches" else "", 
         title = title) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_color_brewer(palette = "Set2") +
    theme(  axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 26),
          axis.title = element_text(size = 28),
          title = element_text(size = 28))
}

# Create the three plots with consistent y-axis
working_plot <- create_class_plot(bras_yearly, "working_prop", "Working Class Mentions") 
middle_plot <- create_class_plot(bras_yearly, "middle_prop", "Middle Class Mentions")
elite_plot <- create_class_plot(bras_yearly, "elite_prop", "Elite Mentions")


# Combine all plots with patchwork
combined_plots <- working_plot + middle_plot + elite_plot +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Class Mentions by Presidents in Brazilian Presidential Speeches",
    #subtitle = "All plots share the same y-axis scale",
    theme = theme(
      plot.title = element_text(size = 38, hjust = 0.5),
      #plot.subtitle = element_text(size = 14, hjust = 0.5)
    )
  )

# Display the plot
combined_plots


bras_yearly <- brasil %>% 
  group_by(year, party_family ) %>% 
  reframe( 
    working_prop = sum(working_class_mentions >=1) / n(),
    middle_prop = sum(middle_class_mentions >=1) / n(),
    elite_prop = sum(elite_mentions >=1) / n()
    
  ) 


# Find global y-axis range for consistent scaling
y_min <- 0
y_max <- 1 

# Function to create a consistent plot with shared y-axis
create_class_plot <- function(data, y_var, title) {
  ggplot(data, aes(x = fct_reorder(party_family, .data[[y_var]]), y = .data[[y_var]], color = party_family)) +
    geom_beeswarm(alpha = 1, size = 3) +  # bigger points
    geom_boxplot(alpha = 0, width = 0.3, notch = FALSE, size = 1.5) +
    theme_minimal() +
    theme(legend.position = "none") +
    ylim(y_min, y_max) +  # Set consistent y limits
    labs(x = "", y = if(y_var == "working_prop") "Salience - % of speeches" else "", 
         title = title) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_color_brewer(palette = "Set2") +
    theme(  axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 26),
            axis.title = element_text(size = 28),
            title = element_text(size = 28))
}

# Create the three plots with consistent y-axis
working_plot <- create_class_plot(bras_yearly, "working_prop", "Working Class Mentions") 
middle_plot <- create_class_plot(bras_yearly, "middle_prop", "Middle Class Mentions")
elite_plot <- create_class_plot(bras_yearly, "elite_prop", "Elite Mentions")


# Combine all plots with patchwork
combined_plots <- working_plot + middle_plot + elite_plot +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Class Mentions by Parties in Brazilian Presidential Speeches",
    #subtitle = "All plots share the same y-axis scale",
    theme = theme(
      plot.title = element_text(size = 38, hjust = 0.5),
      #plot.subtitle = element_text(size = 14, hjust = 0.5)
    )
  )

# Display the plot
combined_plots


###############################################################################################
# sentiment
#----------------------------------------------------
elite <- fread("debate_sentiment_elite_class.csv")
middle <- fread("debate_sentiment_middle_class.csv")
working <- fread("debate_sentiment_working_class.csv")

working$party_family <- ifelse(working$party == "PMDB", "PMDB: Catch All", # heuristic help
                              ifelse( working$party == "PRN", "PRN: Econ Lib",
                                      ifelse( working$party == "PSL", "PSL: Far Right",
                                              ifelse( working$party == "PSDB", "PSDB: Soc Dem",
                                                      ifelse( working$party == "PT", "PT: Workers","")))))

work_sent <- working %>% 
  group_by(year, party_family ) %>% 
  reframe( negative = sum(label=="negative"),
           n = length(id),       
           positive = sum(label=="positive"),
           sentiment = (positive-negative) / n)


middle$party_family <- ifelse(middle$party == "PMDB", "PMDB: Catch All", # heuristic help
                              ifelse( middle$party == "PRN", "PRN: Econ Lib",
                                      ifelse( middle$party == "PSL", "PSL: Far Right",
                                              ifelse( middle$party == "PSDB", "PSDB: Soc Dem",
                                                      ifelse( middle$party == "PT", "PT: Workers","")))))

middle_sent <- middle %>% 
  group_by(year, party_family ) %>% 
  reframe( negative = sum(label=="negative"),
           n = length(id),
           positive = sum(label=="positive"),
           sentiment = (positive-negative) / n)

elite$party_family <- ifelse(elite$party == "PMDB", "PMDB: Catch All", # heuristic help
                              ifelse( elite$party == "PRN", "PRN: Econ Lib",
                                      ifelse( elite$party == "PSL", "PSL: Far Right",
                                              ifelse( elite$party == "PSDB", "PSDB: Soc Dem",
                                                      ifelse( elite$party == "PT", "PT: Workers","")))))

elite_sent <- elite %>% 
  group_by(year, party_family ) %>% 
  reframe( negative = sum(label=="negative"),
           n = length(id),
           positive = sum(label=="positive"),
           sentiment = (positive-negative) / n)


create_sentiment_plot <- function(data, title) {
  ggplot(data, aes(x = fct_reorder(party_family, sentiment), y = sentiment, color = party_family)) +
    geom_beeswarm(alpha = 1, size = 3) +  # bigger points
    geom_boxplot(alpha = 0, width = 0.3, notch = FALSE, size = 1.5) +
    theme_minimal() +
    theme(legend.position = "none") +
    ylim(-1, 1) +  # Set consistent y limits for sentiment score
    labs(x = "", y = if(title == "Working Class Sentiment") "Sentiment Score" else "", 
         title = title) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_color_brewer(palette = "Set2") +
    theme(  axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 26),
            axis.title = element_text(size = 28),
            title = element_text(size = 28)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5)  # Add reference line at 0
}

# Create the three sentiment plots with consistent y-axis
working_sentiment_plot <- create_sentiment_plot(work_sent, "Working Class Sentiment")
middle_sentiment_plot <- create_sentiment_plot(middle_sent, "Middle Class Sentiment")
elite_sentiment_plot <- create_sentiment_plot(elite_sent, "Elite Class Sentiment")

# Combine all plots with patchwork
combined_sentiment_plots <- working_sentiment_plot + middle_sentiment_plot + elite_sentiment_plot +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Class Sentiment by Parties in Brazilian Presidential Speeches",
    theme = theme(
      plot.title = element_text(size = 38, hjust = 0.5)
    )
  )

# Display the plot
combined_sentiment_plots


# --------------------------------------------
# lula

work_sent <- working %>% 
  group_by(year, social_background ) %>% 
  reframe( negative = sum(label=="negative"),
           n = length(id),       
           positive = sum(label=="positive"),
           sentiment = (positive-negative) / n)


middle_sent <- middle %>% 
  group_by(year, social_background ) %>% 
  reframe( negative = sum(label=="negative"),
           n = length(id),
           positive = sum(label=="positive"),
           sentiment = (positive-negative) / n)

elite_sent <- elite %>% 
  group_by(year, social_background ) %>% 
  reframe( negative = sum(label=="negative"),
           n = length(id),
           positive = sum(label=="positive"),
           sentiment = (positive-negative) / n)


create_sentiment_plot <- function(data, title) {
  ggplot(data, aes(x = fct_reorder(social_background, sentiment), y = sentiment, color = social_background)) +
    geom_beeswarm(alpha = 1, size = 1) +  # bigger points
    geom_boxplot(alpha = 0, width = 0.3, notch = FALSE, size = 0.9) +
    theme_minimal() +
    theme(legend.position = "none") +
    ylim(-1, 1) +  # Set consistent y limits for sentiment score
    labs(x = "", y = if(title == "Working Class Sentiment") "Sentiment Score" else "", 
         title = title) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_color_brewer(palette = "Set2") +
    theme(#axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(size = 18),
          axis.title = element_text(size = 28),
          title = element_text(size = 28)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5)  # Add reference line at 0
}

# Create the three sentiment plots with consistent y-axis
working_sentiment_plot <- create_sentiment_plot(work_sent, "Working Class Sentiment")
middle_sentiment_plot <- create_sentiment_plot(middle_sent, "Middle Class Sentiment")
elite_sentiment_plot <- create_sentiment_plot(elite_sent, "Elite Class Sentiment")

# Combine all plots with patchwork
combined_sentiment_plots <- working_sentiment_plot + middle_sentiment_plot + elite_sentiment_plot +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Class Sentiment by Social Background in Brazilian Presidential Speeches",
    theme = theme(
      plot.title = element_text(size = 38, hjust = 0.5)
    )
  )

# Display the plot
combined_sentiment_plots

# ---------------------------------------
# parties

working$president_surname <- ifelse(working$president == "José Sarney", "Sarney", 
                         ifelse(working$president =="Fernando Collor", "Collor",
                                ifelse(working$president == "Itamar Franco", "Franco",   
                                       ifelse(working$president =="Fernando Henrique Cardoso", "Cardoso",
                                              ifelse(working$president == "Lula", "Lula",   
                                                     ifelse(working$president =="Dilma Rousseff", "Rousseff",
                                                            ifelse(working$president == "Michel Temer", "Temer",
                                                                   ifelse(working$president =="Jair Bolsonaro", "Bolsonaro",""))))))))


middle$president_surname <- ifelse(middle$president == "José Sarney", "Sarney", 
                                   ifelse(middle$president =="Fernando Collor", "Collor",
                                          ifelse(middle$president == "Itamar Franco", "Franco",   
                                                 ifelse(middle$president =="Fernando Henrique Cardoso", "Cardoso",
                                                        ifelse(middle$president == "Lula", "Lula",   
                                                               ifelse(middle$president =="Dilma Rousseff", "Rousseff",
                                                                      ifelse(middle$president == "Michel Temer", "Temer",
                                                                             ifelse(middle$president =="Jair Bolsonaro", "Bolsonaro",""))))))))

elite$president_surname <- ifelse(elite$president == "José Sarney", "Sarney", 
                                   ifelse(elite$president =="Fernando Collor", "Collor",
                                          ifelse(elite$president == "Itamar Franco", "Franco",   
                                                 ifelse(elite$president =="Fernando Henrique Cardoso", "Cardoso",
                                                        ifelse(elite$president == "Lula", "Lula",   
                                                               ifelse(elite$president =="Dilma Rousseff", "Rousseff",
                                                                      ifelse(elite$president == "Michel Temer", "Temer",
                                                                             ifelse(elite$president =="Jair Bolsonaro", "Bolsonaro",""))))))))



work_sent <- working %>% 
  group_by(year, president_surname ) %>% 
  reframe( negative = sum(label=="negative"),
           n = length(id),       
           positive = sum(label=="positive"),
           sentiment = (positive-negative) / n)


middle_sent <- middle %>% 
  group_by(year, president_surname ) %>% 
  reframe( negative = sum(label=="negative"),
           n = length(id),
           positive = sum(label=="positive"),
           sentiment = (positive-negative) / n)

elite_sent <- elite %>% 
  group_by(year, president_surname ) %>% 
  reframe( negative = sum(label=="negative"),
           n = length(id),
           positive = sum(label=="positive"),
           sentiment = (positive-negative) / n)


create_sentiment_plot <- function(data, title) {
  ggplot(data, aes(x = fct_reorder(president_surname, sentiment), y = sentiment, color = president_surname)) +
    geom_beeswarm(alpha = 1, size = 3) +  # bigger points
    geom_boxplot(alpha = 0, width = 0.3, notch = FALSE, size = 1.5) +
    theme_minimal() +
    theme(legend.position = "none") +
    ylim(-1, 1) +  # Set consistent y limits for sentiment score
    labs(x = "", y = if(title == "Working Class Sentiment") "Sentiment Score" else "", 
         title = title) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_color_brewer(palette = "Set2") +
    theme(  axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text = element_text(size = 26),
            axis.title = element_text(size = 28),
            title = element_text(size = 28)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5)  # Add reference line at 0
}

# Create the three sentiment plots with consistent y-axis
working_sentiment_plot <- create_sentiment_plot(work_sent, "Working Class Sentiment")
middle_sentiment_plot <- create_sentiment_plot(middle_sent, "Middle Class Sentiment")
elite_sentiment_plot <- create_sentiment_plot(elite_sent, "Elite Class Sentiment")

# Combine all plots with patchwork
combined_sentiment_plots <- working_sentiment_plot + middle_sentiment_plot + elite_sentiment_plot +
  plot_layout(ncol = 3) +
  plot_annotation(
    title = "Class Sentiment by Presidents in Brazilian Presidential Speeches",
    theme = theme(
      plot.title = element_text(size = 38, hjust = 0.5)
    )
  )

# Display the plot
combined_sentiment_plots

