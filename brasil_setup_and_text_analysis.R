#--------------------------------------------------
# Text analysis setup
#--------------------------------------
# Setup
library(tidyverse)
library(quanteda)
library(flexplot)
library(readxl)
library(data.table)
#--------------------------------------
# Data prep
setwd("C:/Users/stepa/OneDrive/Dokumenty/Brazil_Cph")
data<-read_excel("C:/Users/stepa/downloads/PresidentialSpeeches-2.xlsx")

data$clean<-tolower(data$text) # all characters to lower

data$party <- ifelse(data$PMDB == 1, "PMDB",   # one variable for parties
                     ifelse( data$PRN == 1, "PRN",
                             ifelse( data$PSL == 1, "PSL",
                                     ifelse( data$PSDB == 1, "PSDB",
                                             ifelse( data$PT == 1, "PT","")))))

data$party_family <- ifelse(data$PMDB == 1, "CatchAll_Centrist", # heuristic help
                         ifelse( data$PRN == 1, "OneMan_EconLiberal",
                             ifelse( data$PSL == 1, "Bolsonaro_FarRight",
                                     ifelse( data$PSDB == 1, "SocialDemocrats",
                                             ifelse( data$PT == 1, "Lula_Workers","")))))

data$president <- ifelse(data$date <= 1990 & data$PMDB ==1, "José Sarney", 
                                   ifelse(data$PRN ==1,"Fernando Collor",
                                          ifelse(data$date >= 1992 & data$date<= 1995 & data$PMDB ==1, "Itamar Franco",   
                                                 ifelse(data$PSDB == 1, "Fernando Henrique Cardoso",
                                                        ifelse(data$date < 2011 & data$PT ==1, "Lula",   
                                                               ifelse(data$date >= 2011  & data$PT ==1, "Dilma Rousseff",
                                                                      ifelse( data$date >=2016 & data$PMDB ==1, "Michel Temer",
                                                                             ifelse(data$PSL ==1, "Jair Bolsonaro", ""))))))))

flexplot(president~date, data)
data$social_background <- ifelse(data$PT == 1 # binary collar
                                  & data$date >= 2003 
                                  & data$date<=2010, "Blue Collar", "White Collar") # catch ony Lula from PT

brasil <- data %>% # delete binarized parties
  select(-5:-10)

brasil$party <- 
  as.factor(brasil$party) # factorize

brasil$party_family <- 
  as.factor(brasil$party_family)

write.csv(brasil, "Brazil_Presidential_Speeches_Clean.csv") # save clean data

# ------------------------------------
# data checkpoint

# -----------------------
# extarct dates fom textual data using regex, claude helped her
brasil$month <- sapply(brasil$clean, function(text) {
  month_pattern <- paste(c(
    'janeiro', 'fevereiro', 'março', 'abril', 'maio', 'junho', 
    'julho', 'agosto', 'setembro', 'outubro', 'novembro', 'dezembro'
  ), collapse = "|")
  
  match <- regexpr(paste0("(\\d{1,2})\\sde\\s(", month_pattern, ")"), 
                   text, ignore.case = TRUE)
  
  if (match[1] > 0) {
    substr(text, match[1], match[1] + attr(match, "match.length") - 1)
  } else {
    NA
  }
})

brasil$month <- sapply(brasil$clean, function(text) {
  month_pattern <- paste(c(
    'janeiro', 'fevereiro', 'março', 'abril', 'maio', 'junho', 
    'julho', 'agosto', 'setembro', 'outubro', 'novembro', 'dezembro'
  ), collapse = "|")
  
  # More flexible pattern with extra space handling
  match <- regexpr(paste0("(\\d{1,2})\\s*[,aeoº°ª]?\\s*de\\s+(", month_pattern, ")"), 
                   text, ignore.case = TRUE, perl = TRUE)
  
  if (match[1] > 0) {
    # Extract the match and strip leading/trailing spaces
    result <- substr(text, match[1], match[1] + attr(match, "match.length") - 1)
    trimws(result)  # Remove leading/trailing whitespace
  } else {
    NA
  }
})
sum(is.na(brasil$month))
length(brasil$month)



#---- to a date 

brasil$date_month <- sapply(1:nrow(brasil), function(i) {
  month_text <- brasil$month[i]
  year <- brasil$date[i]  # assuming this contains the year
  
  if (is.na(month_text) || is.na(year)) {
    return(NA)
  }
  
  # Month mapping (including both spellings)
  month_map <- c(
    "janeiro" = 1, "fevereiro" = 2, "março" = 3, "abril" = 4,
    "maio" = 5, "junho" = 6, "julho" = 7, "agosto" = 8,
    "setembro" = 9, "septembro" = 9,  # both spellings
    "outubro" = 10, "novembro" = 11, "dezembro" = 12
  )
  
  # Find which month is mentioned
  month_num <- NA
  for (month_name in names(month_map)) {
    if (grepl(month_name, tolower(month_text))) {
      month_num <- month_map[month_name]
      break
    }
  }
  
  if (is.na(month_num)) {
    return(NA)
  }
  
  # Format as mm/yyyy (or change to mm.yyyy if you prefer)
  sprintf("%02d/%04d", month_num, year)
})


# ---------------------
# other vars for visuals
brasil$president_surname <- ifelse(brasil$president == "José Sarney", "Sarney", 
                                   ifelse(brasil$president =="Fernando Collor", "Collor",
                                          ifelse(brasil$president == "Itamar Franco", "Franco",   
                                                 ifelse(brasil$president =="Fernando Henrique Cardoso", "Cardoso",
                                                        ifelse(brasil$president == "Lula", "Lula",   
                                                               ifelse(brasil$president =="Dilma Rousseff", "Rousseff",
                                                                      ifelse(brasil$president == "Michel Temer", "Temer",
                                                                             ifelse(brasil$president =="Jair Bolsonaro", "Bolsonaro",""))))))))


brasil$party_family <- ifelse(brasil$party == "PMDB", "PMDB: Catch All", # heuristic help
                              ifelse( brasil$party == "PRN", "PRN: Econ Lib",
                                      ifelse( brasil$party == "PSL", "PSL: Far Right",
                                              ifelse( brasil$party == "PSDB", "PSDB: Soc Dem",
                                                      ifelse( brasil$party == "PT", "PT: Workers","")))))



# ------------- other vars for stat models 

brasil$election <- ifelse(brasil$date_month == "11/1989" | brasil$date_month == "12/1989" | brasil$date_month == "10/1994"
                          | brasil$date_month == "10/1998" | brasil$date_month == "10/2002"
                          | brasil$date_month == "10/2006" | brasil$date_month == "10/2010" | brasil$date_month == "10/2014"
                          |brasil$date_month == "10/2018"| brasil$date_month == "10/2022", "1", "0")

brasil <- brasil %>% 
  rename(year= date)

brasil$date <- as.Date(paste0("01/", brasil$date_month), format = "%d/%m/%Y")
brasil$election_period <- ifelse(brasil$date > as.Date("01/04/1994", format = "%d/%m/%Y") & 
                                   brasil$date <= as.Date("01/10/1994", format = "%d/%m/%Y"), "1",
                                 ifelse(brasil$date > as.Date("01/04/1998", format = "%d/%m/%Y") & 
                                          brasil$date <= as.Date("01/10/1998", format = "%d/%m/%Y"), "1",
                                        ifelse(brasil$date > as.Date("01/04/2002", format = "%d/%m/%Y") & 
                                                 brasil$date <= as.Date("01/10/2002", format = "%d/%m/%Y"), "1",
                                               ifelse(brasil$date > as.Date("01/04/2006", format = "%d/%m/%Y") & 
                                                        brasil$date <= as.Date("01/10/2006", format = "%d/%m/%Y"), "1",
                                                      ifelse(brasil$date > as.Date("01/04/2010", format = "%d/%m/%Y") & 
                                                               brasil$date <= as.Date("01/10/2010", format = "%d/%m/%Y"), "1",
                                                             ifelse(brasil$date > as.Date("01/04/2014", format = "%d/%m/%Y") & 
                                                                      brasil$date <= as.Date("01/10/2014", format = "%d/%m/%Y"), "1",
                                                                    ifelse(brasil$date > as.Date("01/04/2018", format = "%d/%m/%Y") & 
                                                                             brasil$date <= as.Date("01/10/2018", format = "%d/%m/%Y"), "1",
                                                                           ifelse(brasil$date > as.Date("01/04/2022", format = "%d/%m/%Y") & 
                                                                                    brasil$date <= as.Date("01/10/2022", format = "%d/%m/%Y"), "1", "0"))))))))



# ---------- econ data
inflation<-fread("inflation_monthly.csv")

colnames(inflation) <-c("date", "inflation")
inflation$date <- as.Date(inflation$date)


inflation_yearly<-c(225.9896652,	147.1428262,	228.3361626,	629.1145091,	1430.723725,	2947.732772,	432.7866619,
                    951.9620531,	1927.38079,	2075.888398	,66.00703355,	15.7576656	,6.926712516,	3.195076293	,4.858447499,
                    7.044141059,	6.840359025,	8.450164377,	14.71491972	,6.5971851	,6.869537209,	4.183568129	,3.641272991	,
                    5.678593903,	4.888034799	,5.038726901	,6.636449622	,5.40349914,	6.204310666	,6.329040155,	9.029901024,
                    8.739143523,	3.44637335,	3.664850284	,3.732976212,	3.211768038)
unemployment<-c(6.894,	7.086,	6.03	,6.578,	7.092,	8.035	,9.003	,10.15,	11.125,	10.889,	10.649,
                10.641,	11.168,	10.072,	10.551,	9.692,	9.28,	8.268,	9.419,	8.422,	7.578,	7.251,
                7.071,	6.755,	8.538,	11.58,	12.792,	12.329,	11.936,	13.697)


year<-c(1985, 1986,1987,1988,1989,1990,1991,1992, 1993,	1994,	1995,	1996,	1997,	1998,	1999,	2000,	2001,	2002,	2003,	2004,
        2005,	2006,	2007,	2008,	2009,	2010,	2011,	2012,	2013,	2014,
        2015,	2016,	2017,	2018,	2019,	2020)
year2<-c(1991,1992, 1993,	1994,	1995,	1996,	1997,	1998,	1999,	2000,	2001,	2002,	2003,	2004,
         2005,	2006,	2007,	2008,	2009,	2010,	2011,	2012,	2013,	2014,
         2015,	2016,	2017,	2018,	2019,	2020)
yearly_inflation <- data.frame(year, inflation_yearly)
yearly_unemployment <- data.frame(year2, unemployment)

brasil <- brasil %>% 
  left_join(inflation, by = "date")

brasil <- brasil %>% 
  left_join(yearly_inflation, by = "year")

brasil <- brasil %>% 
  left_join(yearly_unemployment, by = c("year"="year2"))


write.csv(brasil,"brasil_final.csv")



#------------------------------------------------
# VOCABULARIES
# working class <-  workers, laborers,blue-collar, working-class, unions, 
# proletariat, farmers, factory workers, miners 

middle_class_en <- c(
  "middle(-| )class", "public servant(s)?", "civil servant(s)?",
  "doctor(s)?", "lawyer(s)?", "engineer(s)?",
  "business(wo)?m(a|e)n", "entrepreneur(s)?"
)

# Elite / Upper Class terms
elite_en <- c(
  "elite(s)?", "rich", "wealthy", "banker(s)?",
  "millionaire(s)?", "billionaire(s)?", "aristocra(t|cy)", "oligarch(s|y)?"
)
# Portuguese equivalents
working_class_pt <- c(
  "trabalhador(es|as)?", "operário(s|as)?", "classe operária", "proletário(s|as)?",
  "sindicat(o|os|al|ais)", "classe trabalhadora", "agricultor(es|as)?", "mineiro(s|as)?",
  "operário(s|as)? de fábrica"
)

middle_class_pt <- c(
  "classe média", "servidor(es|as)? público(s|as)?", "funcionário(s|as)? público(s|as)?",
  "médico(s|as)?", "advogado(s|as)?", "engenheiro(s|as)?",
  "empresário(s|as)?", "empreendedor(es|as)?"
)

elite_pt <- c(
  "elite(s)?", "rico(s|as)?", "abastado(s|as)?", "banqueiro(s|as)?",
  "milionário(s|as)?", "bilionário(s|as)?", "aristocra(ta|cia)", "oligarca(s)?"
)


# Function to create regex pattern from terms
create_regex_pattern <- function(terms) {
  pattern <- paste0("\\b(", paste(terms, collapse = "|"), ")\\b")
  return(pattern)
}

# Create patterns for each social group
working_class_pattern_pt <- create_regex_pattern(working_class_pt)
middle_class_pattern_pt <- create_regex_pattern(middle_class_pt)
elite_pattern_pt <- create_regex_pattern(elite_pt)


#############################################################X
# MENTIONS EXTRACTION

# Calculate word count for each speech
brasil$word_count <- str_count(brasil$clean, "\\b\\w+\\b")
sum(brasil$word_count)

brasil$working_class_mentions <- str_count(brasil$clean, regex(working_class_pattern_pt))

brasil$middle_class_mentions <- str_count(brasil$clean, regex(middle_class_pattern_pt))

brasil$elite_mentions <- str_count(brasil$clean, regex(elite_pattern_pt))


# Add a row ID to dataframe
brasil <- brasil %>% 
  mutate(id = row_number())



write.csv(brasil,"brasil_final.csv")

# final dataset for salience analysis

#------------------------------------------------
#-------------------------------------------------------
# kwic extraction for sentiment analysis
#-------------------
# Create corpus for kwic extarct
corpus_speeches <- corpus(
  brasil$clean,
  docvars = data.frame(
    id = brasil$id,
    year = brasil$date,
    party = brasil$party,
    party_family = brasil$party_family,
    social_background = brasil$social_background
  )
)

# Tokenize
tokens_speeches <- tokens(corpus_speeches)

#############################
# working class mentions extraction
############################
# Keywords-in-context 
kwic_working_class <- kwic(
  tokens_speeches,
  pattern = working_class_pattern_pt,
  window = 50, 
  valuetype = "regex"
)

# Create final data frame with the exact matched word, the full context in the window around and all metadata
kwic_df_working_class <- as_tibble(kwic_working_class) %>%
  left_join(
    data.frame(
      docid = names(corpus_speeches),
      id = docvars(corpus_speeches)$id,
      year = docvars(corpus_speeches)$year,
      party = docvars(corpus_speeches)$party,
      party_family = docvars(corpus_speeches)$party_family,
      social_background = docvars(corpus_speeches)$social_background
    ),
    by = c("docname" = "docid")
  ) %>%
  mutate(
    matched_word = str_extract(keyword, "\\S+"),
    context_full = paste(pre, matched_word, post)
  ) %>%
  select(
    id,
    year,
    party,
    party_family,
    social_background,
    matched_word,   # Added these columns that were missing
    context_full,   # from your select statement
    pre,            # Include if you need the context before
    post            # Include if you need the context after
  )

kwic_df_working_class <- ifelse()
write.csv(kwic_df_working_class, "working_class.csv")

#-----------------------------------
# middle class
# Keywords-in-context 
kwic_middle_class <- kwic(
  tokens_speeches,
  pattern = middle_class_pattern_pt,
  window = 50, 
  valuetype = "regex"
)

# Create final data frame with the exact matched word, the full context in the window around and all metadata
kwic_df_middle_class <- as_tibble(kwic_middle_class) %>%
  left_join(
    data.frame(
      docid = names(corpus_speeches),
      id = docvars(corpus_speeches)$id,
      year = docvars(corpus_speeches)$year,
      party = docvars(corpus_speeches)$party,
      party_family = docvars(corpus_speeches)$party_family,
      social_background = docvars(corpus_speeches)$social_background
    ),
    by = c("docname" = "docid")
  ) %>%
  mutate(
    matched_word = str_extract(keyword, "\\S+"),
    context_full = paste(pre, matched_word, post)
  ) %>%
  select(
    id,
    year,
    party,
    party_family,
    social_background,
    matched_word,   # Added these columns that were missing
    context_full,   # from your select statement
    pre,            # Include if you need the context before
    post            # Include if you need the context after
  )

write.csv(kwic_df_middle_class, "middle_class.csv")

# --------------- elite

# Keywords-in-context 
kwic_elite_class <- kwic(
  tokens_speeches,
  pattern = elite_pattern_pt,
  window = 50, 
  valuetype = "regex"
)

# Create final data frame with the exact matched word, the full context in the window around and all metadata
kwic_df_elite_class <- as_tibble(kwic_elite_class) %>%
  left_join(
    data.frame(
      docid = names(corpus_speeches),
      id = docvars(corpus_speeches)$id,
      year = docvars(corpus_speeches)$year,
      party = docvars(corpus_speeches)$party,
      party_family = docvars(corpus_speeches)$party_family,
      social_background = docvars(corpus_speeches)$social_background
    ),
    by = c("docname" = "docid")
  ) %>%
  mutate(
    matched_word = str_extract(keyword, "\\S+"),
    context_full = paste(pre, matched_word, post)
  ) %>%
  select(
    id,
    year,
    party,
    party_family,
    social_background,
    matched_word,   # Added these columns that were missing
    context_full,   # from your select statement
    pre,            # Include if you need the context before
    post            # Include if you need the context after
  )

write.csv(kwic_df_elite_class, "elite_class.csv")

############################
# Filter out windows that also mention the right 
kwic_df_left$right_mentions <- str_count(kwic_df_left$context_full, regex(pattern_right))
kwic_df_left<-kwic_df_left %>% filter(right_mentions==0)


#############################
#  
kwic_df_right$left_mentions <- str_count(kwic_df_right$context_full, regex(pattern_left))
kwic_df_right<-kwic_df_right %>% filter(left_mentions==0)



##################################
# SWITCH TO PYTHON FOR MACHINE TRANSLATION AND NATURAL LANGUAGE INFERENCE
##################################
