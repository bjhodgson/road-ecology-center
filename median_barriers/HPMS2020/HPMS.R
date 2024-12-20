library(sf)
library(dplyr)
library(lubridate)
library(readxl)
library(car)

# Load in data

# Roadkill w/in 50m of highways
cros <- read_excel("D:\\Downloads\\cros_hpms_2016_50m.xlsx")
cros_df <- cros %>%
  filter(condition %in% c("Dead")) %>%
  filter(observatio > "2016/01/01 00:00:00.000") %>%
  filter(animal_cat == "Mammal (Large)")  #%>%
  #filter(animal == "Mule (or Black tailed) Deer")

cros_df <- cros_df %>%
  group_by(latitude, longitude) %>%
  filter(n() < 2)

cros_summary <- cros_df %>%
  group_by(animal_cat) %>%
  count()

# Random points along highways
random <- read_excel("D:\\Downloads\\random_sample_hpms_2016.xlsx")

# Random points along highways



predictors <- c("AADT", 
                "MEDIAN_TYPE", 
                "MEDIAN_WIDTH", 
                "LANE_WIDTH", 
                "URBAN_CODE",
                "SURFACE_TYPE",
                "roadkill")

roadkill_df <- cros_df %>%
  as.data.frame() %>%
  #filter(URBAN_CODE %in% c(99998, 99999)) %>% # Too few obs
  filter(MEDIAN_TYPE != 0) %>% # Remove null
  #filter(SURFACE_TYPE != 0) %>% # Remove null
  #filter(STRUCTURE_TYPE != 0) %>% # Too few obs
  mutate(roadkill = 1) %>%
  select(all_of(predictors))

roadkill_summary <- roadkill_df %>%
  count(MEDIAN_TYPE) %>%
  mutate(pct = n / sum(n))

nrow(roadkill_df)
set.seed(123)
random_df <- random %>%
  as.data.frame() %>%
  #filter(URBAN_CODE %in% c(99998, 99999)) %>% # Too few obs
  filter(MEDIAN_TYPE != 0) %>% # Remove null
  #filter(SURFACE_TYPE != 0) %>% # Remove null
  #filter(STRUCTURE_TYPE != 0) %>% # Remove null
  mutate(roadkill = 0) %>%
  select(all_of(predictors)) %>%
  sample_n(size = nrow(roadkill_df)) # Set equal sample size
  

random_summary <- random_df %>%
  count(MEDIAN_TYPE) %>%
  mutate(pct = n / sum(n))

data_df <- rbind(roadkill_df, random_df) #%>%
#  filter(MEDIAN_TYPE != 3) #%>%
#  filter(!MEDIAN_TYPE %in% c(4, 3))
#   filter(MEDIAN_TYPE != 5)

#data_df$MEDIAN_TYPE[data_df$MEDIAN_TYPE %in% c(5, 6, 7)] <- 4

data_summary <- data_df %>%
  group_by(MEDIAN_TYPE) %>%
  count(roadkill)

# Fit a logistic regression model
logistic_model <- glm(roadkill ~ 1, family = binomial(link = "logit"), data = data_df)
summary(logistic_model)

# Fit a logistic regression model
logistic_model <- glm(roadkill ~ as.factor(MEDIAN_TYPE), family = binomial(link = "logit"), data = data_df)
summary(logistic_model)
exp(coef(logistic_model))

# Fit a logistic regression model
logistic_model <- glm(roadkill ~ as.factor(MEDIAN_TYPE) + MEDIAN_WIDTH, family = binomial(link = "logit"), data = data_df)
summary(logistic_model)
exp(coef(logistic_model))


# Fit a logistic regression model
logistic_model <- glm(roadkill ~ as.factor(MEDIAN_TYPE) + MEDIAN_WIDTH + AADT, family = binomial(link = "logit"), data = data_df)
summary(logistic_model)
exp(coef(logistic_model))

# Fit a logistic regression model
logistic_model <- glm(roadkill ~ as.factor(MEDIAN_TYPE) + MEDIAN_WIDTH + AADT + as.factor(URBAN_CODE), family = binomial(link = "logit"), data = data_df)
summary(logistic_model) # urban codes refer to cities, exclude

# Fit a logistic regression model
logistic_model <- glm(roadkill ~ as.factor(MEDIAN_TYPE) + MEDIAN_WIDTH + AADT + as.factor(SURFACE_TYPE), family = binomial(link = "logit"), data = data_df)
summary(logistic_model)


# Fit a logistic regression model
logistic_model <- glm(roadkill ~ as.factor(MEDIAN_TYPE) + AADT + as.factor(SURFACE_TYPE), family = binomial(link = "logit"), data = data_df)
summary(logistic_model)
vif(logistic_model)


logistic_model <- glm(roadkill ~ as.factor(MEDIAN_TYPE), family = binomial(link = "logit"), data = data_df)
summary(logistic_model)
vif(logistic_model)



# Assess model fit for effect size
null_model <- glm(roadkill ~ 1, family = binomial(link = "logit"), data = data_df)
full_model <- glm(roadkill ~ as.factor(MEDIAN_TYPE), family = binomial(link = "logit"), data = data_df)

summary(null_model)
summary(full_model)

anova(null_model, full_model, test = "Chisq")

exp(coef(full_model))


