
script_dir <- "C:\\Users\\HP\\Documents\\GitHub\\road-ecology-center\\median_barriers\\District Analysis\\D2"
source(file.path(script_dir, "D2_ProcessInputData.R"))

# logistic regression (median type WVC vs median type random)

# Add binary variables median types
WVC_binary <- WVC_gdf %>% # WVC df
  as.data.frame() %>%
  mutate(Roadkill = 1) %>% # Add binary variable (1 = roadkill)
  select(MedianType, Roadkill, AADT)

random_binary <- merged_random_gdf %>% # Random points df
  as.data.frame() %>% 
  mutate(Roadkill = 0) %>% # Add binary variable (0 = roadkill)
  select(MedianType, Roadkill, AADT)

# Bind dfs and standardize values
binary_df <- rbind(WVC_binary, random_binary) %>%
  mutate(MedianType = case_when( # Standardize values
    MedianType == "vegetative, thrie beam" ~ "thrie beam", # Change to thrie beam, otherwise count too low
    MedianType == "thrie beam, vegetative" ~ "thrie beam", # Change to thrie beam, otherwise count too low
    MedianType == "gravel, thrie beam" ~ "thrie beam", # Change to thrie beam, otherwise count too low
    MedianType == "gravel, vegetative" ~ "vegetative",
    MedianType == "vegetative, gravel" ~ "vegetative",
    MedianType == "vegetative, concrete" ~ "concrete",
    MedianType == "temp concrete" ~ "concrete",
    TRUE ~ MedianType  # Retain original value if no condition is met
  ))

# Define median treatment values
treatment_list <- c("concrete"
                    # "thrie beam",
                    # "cable"
                    # "vegetative",
                    # "gravel"
)



binary_df <- binary_df %>%
  mutate(Barrier = ifelse(MedianType %in% treatment_list, 1, 0)) %>%
  filter(!MedianType %in% c("gravel", "thrie beam", "cable", "vegetative")) # Filter out values

model <- glm(Roadkill ~ Barrier + AADT, 
             data = binary_df, 
             family = binomial)
model

coef(summary(model))["Barrier", "Estimate"]
exp(coef(summary(model))["Barrier", "Estimate"])

summary(model)$coefficients["Barrier", "Pr(>|z|)"]
summary(model)$coefficients["AADT", "Pr(>|z|)"]


confint(model) # This will give you the confidence intervals for the coefficients
exp(confint(model)) # Exponentiate to get the confidence intervals for the odds ratios

model_barrier <- glm(Roadkill ~ Barrier, 
                     data = binary_df, 
                     family = binomial)

model_full <- glm(Roadkill ~ Barrier + AADT, 
                  data = binary_df, 
                  family = binomial)

# Assess model fit
anova(model_barrier, model_full, test = "Chisq")
AIC(model_barrier, model_full)



#  veg

# Define median treatment values
treatment_list <- c("vegetative"
                    # "thrie beam",
                    # "cable"
                    # "vegetative",
                    # "gravel"
)

control_list <- c("no median"
                  )

reg_list <- c(treatment_list, control_list)


reg_df <- binary_df %>%
  filter(MedianType %in% reg_list) %>% # Filter out values
  mutate(Barrier = ifelse(MedianType %in% treatment_list, 1, 0)) 

model <- glm(Roadkill ~ Barrier + AADT, 
             data = reg_df, 
             family = binomial)
model

coef(summary(model))["Barrier", "Estimate"]
exp(coef(summary(model))["Barrier", "Estimate"])

summary(model)$coefficients["Barrier", "Pr(>|z|)"]

confint(model) # This will give you the confidence intervals for the coefficients
exp(confint(model)) # Exponentiate to get the confidence intervals for the odds ratios



# conc on veg

treatment_list <- c("vegetative"
                    # "thrie beam",
                    # "cable"
                    # "vegetative",
                    # "gravel"
)

control_list <- c("concrete"
)

reg_list <- c(treatment_list, control_list)


reg_df <- binary_df %>%
  filter(MedianType %in% reg_list) %>% # Filter out values
  mutate(Barrier = ifelse(MedianType %in% treatment_list, 1, 0)) 

model <- glm(Roadkill ~ Barrier, 
             data = reg_df, 
             family = binomial)
model

coef(summary(model))["Barrier", "Estimate"]
exp(coef(summary(model))["Barrier", "Estimate"])

summary(model)$coefficients["Barrier", "Pr(>|z|)"]

confint(model) # This will give you the confidence intervals for the coefficients
exp(confint(model)) # Exponentiate to get the confidence intervals for the odds ratios




# multi variate


# compare each type
# Ensure 'MedianType' is a factor and specify the reference level
binary_df$MedianType <- factor(binary_df$MedianType, 
                               levels = c("no median", "concrete", "vegetative", "thrie beam", "cable", "gravel"))

# Fit the logistic regression model
model <- glm(Roadkill ~ MedianType, 
             data = binary_df, 
             family = binomial)

# Summary of the model
summary(model)

# Odds ratios and confidence intervals
exp(coef(model)) # Exponentiate to get the odds ratios
exp(confint(model)) # Exponentiate to get the confidence intervals for the odds ratios


# Prepare data for plotting
odds_ratios <- exp(coef(model))
conf_int <- exp(confint(model))
results_df <- data.frame(
  MedianType = names(odds_ratios),
  OddsRatio = odds_ratios,
  CI_lower = conf_int[, 1],
  CI_upper = conf_int[, 2]
)


# Plot
ggplot(results_df, aes(x = MedianType, y = OddsRatio)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  ylab("Odds Ratio") +
  xlab("Median Type") +
  theme_minimal()



# Compute the reduction in risk relative to 'no median'
# Relative risk reduction (RRR) = (1 - Odds Ratio) * 100%
# Note: Exclude the intercept from the calculations
reduction_in_risk <- (1 - odds_ratios[-1]) * 100
ci_lower <- (1 - conf_int[-1, 2]) * 100
ci_upper <- (1 - conf_int[-1, 1]) * 100

# Prepare data for plotting
results_df <- data.frame(
  MedianType = names(odds_ratios)[-1],  # Exclude intercept
  RiskReduction = reduction_in_risk,
  CI_lower = ci_lower,
  CI_upper = ci_upper
)

# Plot
ggplot(results_df, aes(x = MedianType, y = RiskReduction)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), width = 0.2) +
  ylab("Reduction in Risk (%)") +
  xlab("Median Type") +
  theme_minimal()


# why is gravel so high? is power enough?

table(binary_df$MedianType, binary_df$Roadkill)


# Example effect size from your model
odds_ratio <- 0.4573165
effect_size <- (odds_ratio - 1) / (odds_ratio + 1)

# Define parameters
effect_size <- log(odds_ratio)/1.81      # Small effect size
alpha <- 0.05            # Significance level
power <- 0.80            # Desired power
df <- 1                  # Degrees of freedom for one predictor

# Perform power analysis
pwr_result <- pwr.f2.test(u = df, v = NULL, f2 = effect_size, sig.level = alpha, power = power)
print(pwr_result)


#---- chi squared

# Create a contingency table
contingency_table <- table(binary_df$MedianType, binary_df$Roadkill)

# Chi-Square Test of Independence
chisq_test_independence <- chisq.test(contingency_table)
chisq_test_independence
