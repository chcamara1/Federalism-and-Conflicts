# Clear environment
rm(list = ls())
#dev.off()
ls()

# Load required packages
#required_packages <- c("dplyr", "tidyr", "ggplot2", "readr", "haven", "AER", "stargazer", "readxl", "gmm")
#new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
#if (length(new_packages)) install.packages(new_packages)
#install.packages("plm")
#install.packages("writexl")
#install.packages("remotes")
#remotes::install_github("yuchang0321/IVQR")
#install.packages("IVQR")
library(writexl)
library(plm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(haven)
library(AER)
library(stargazer)
library(readxl)
library(broom)
library(gmm)
library(IVQR)

# === 1. Load the data ===
data <- read_excel("C:/Cheick/Thèse doctorale/Ethnic conflict/data/gedevents-2023-10-13.xls")

# === 2. Preprocessing and variable creation ===
# Define armed group lists
secessionist_list <- c("ATTF", "CPI-ML-VM", "CPI-Maoist", "HuM", "Indian Mujahideen", "Kashmir insurgents", 
                       "Lashkar-e-Taiba", "MCC", "NSCN-IM", "NSCN-K", "Sikh insurgents", "ULFA")
creation_of_states_list <- c("ABSU", "BLTF", "Bodo", "DHD", "DHD-BW", "Dimasa", "HPC", "KRA", "Kuki", 
                             "NDFB", "NDFB-RD", "NDFB-S", "UPDS", "Assamese", "Meitei", "Al-Umma")
natural_resources_list <- c("Bangladeshi Immigrants", "PLFI", "Ranvir Sena", "Tripura Non-Tribal Communities")
other_objectives_list <- c("Government of Bangladesh", "Government of China", "Government of India", 
                           "Government of Myanmar (Burma)", "Hindus (India)", "IS", "PCPA", "VHP")

# Add indicator variables and classify objectives
data <- data %>%
  mutate(
    secession = as.integer(side_a %in% secessionist_list | side_b %in% secessionist_list),
    creation = as.integer(side_a %in% creation_of_states_list | side_b %in% creation_of_states_list),
    natural = as.integer(side_a %in% natural_resources_list | side_b %in% natural_resources_list),
    other = as.integer(side_a %in% other_objectives_list | side_b %in% other_objectives_list),
    objective = case_when(
      side_a %in% secessionist_list ~ "Secession",
      side_a %in% creation_of_states_list ~ "Creation of States",
      side_a %in% natural_resources_list ~ "Natural Resources",
      side_a %in% other_objectives_list ~ "Other Objectives",
      TRUE ~ "Unknown"
    )
  )

# === 3. Fill missing years and aggregate data ===
# Generate missing years for each state and fill with 0
complete_data <- data %>%
  group_by(adm_1) %>%
  complete(year = 1989:2020) %>%
  replace_na(list(secession = 0, creation = 0, natural = 0, other = 0, best_est = 0))

# Aggregate by state, year, and objective
grouped_data <- complete_data %>%
  group_by(adm_1, year, objective) %>%
  summarise(event_count = n(),
            death_sum = sum(best_est, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = objective, values_from = c(event_count, death_sum), values_fill = 0)

# === 4. Standardize intensities ===
grouped_data <- grouped_data %>%
  group_by(year) %>%
  mutate(
    mean_intensity_secession = mean(death_sum_Secession, na.rm = TRUE),
    sd_intensity_secession = sd(death_sum_Secession, na.rm = TRUE),
    intensity_standardisee_secession = (death_sum_Secession - mean_intensity_secession) / sd_intensity_secession,
    mean_intensity_creation = mean(`death_sum_Creation of States`, na.rm = TRUE),
    sd_intensity_creation = sd(`death_sum_Creation of States`, na.rm = TRUE),
    intensity_standardisee_creation = (`death_sum_Creation of States` - mean_intensity_creation) / sd_intensity_creation,
    mean_intensity_natural = mean(`death_sum_Natural Resources`, na.rm = TRUE),
    sd_intensity_natural = sd(`death_sum_Natural Resources`, na.rm = TRUE),
    intensity_standardisee_natural = (`death_sum_Natural Resources` - mean_intensity_natural) / sd_intensity_natural
  ) %>%
  ungroup()

# === 5. Modify state names ===
grouped_data <- grouped_data %>%
  mutate(
    adm_1 = ifelse(adm_1 == "Orissa", "Odisha", adm_1), # Merge Orissa into Odisha
    States = case_when(
      adm_1 == "Jammu and Kashmir" ~ "Jammu_Kashmir",
      adm_1 == "Andaman and Nicobar Islands" ~ "Andaman_Nicobar",
      adm_1 == "Himachal Pradesh" ~ "Himachal_Pradesh",
      TRUE ~ gsub(" ", "_", adm_1)
    )
  )
grouped_data$States <- gsub(" state|Union territory of | Union territory|Union Territory of | union territory|National Capital territory|Delhi", "", grouped_data$adm_1)

# === 6. Merge with fiscal data ===
fiscal_data <- read_dta('C:/Cheick/Thèse doctorale/Ethnic conflict/data/data R/ethnicconflicts1.dta')
grouped_data <- grouped_data %>%
  mutate(
    States = ifelse(States == "Orissa", "Odisha", States), # Merge Orissa into Odisha
    States = case_when(
      States == "Jammu and Kashmir" ~ "Jammu_Kashmir",
      States == "Andaman and Nicobar Islands" ~ "Andaman_Nicobar",
      States == "Himachal Pradesh" ~ "Himachal_Pradesh",
      TRUE ~ gsub(" ", "_", States)
    )
  )
merged_data <- merge(fiscal_data, grouped_data, by = c("States", "year"), all.x = TRUE) %>%
  mutate(
    intensity_standardisee_secession = ifelse(is.na(intensity_standardisee_secession), 0, intensity_standardisee_secession),
    intensity_standardisee_creation = ifelse(is.na(intensity_standardisee_creation), 0, intensity_standardisee_creation),
    intensity_standardisee_natural = ifelse(is.na(intensity_standardisee_natural), 0, intensity_standardisee_natural)
  )

merged_data <- merged_data %>%
  filter(!is.na(States))
merged_data <- merged_data %>% filter(States != "")

summary(merged_data$intensity_standardisee_creation)
merged_data$logintensity1<-log(merged_data$intensity_standardisee_creation +1.25567)
summary(merged_data$intensity_standardisee_secession)
merged_data$logintensity<-log(merged_data$intensity_standardisee_secession +1.58995)
summary(merged_data$intensity_standardisee_natural)
merged_data$logintensity_natural<-log(merged_data$intensity_standardisee_natural +1.24761)

merged_data$year_fe <- factor(merged_data$year)
merged_data$state_fe <- factor(merged_data$States)

formation_year <- data.frame(
  States = c("Andhra Pradesh", "Arunachal Pradesh", "Assam", "Bihar", "Chhattisgarh", "Goa", 
             "Gujarat", "Haryana", "Himachal Pradesh", "Jharkhand", "Karnataka", "Kerala", 
             "Madhya Pradesh", "Maharashtra", "Manipur", "Meghalaya", "Mizoram", "Nagaland", 
             "Odisha", "Punjab", "Rajasthan", "Sikkim", "Tamil Nadu", "Telangana", "Tripura", 
             "Uttar Pradesh", "Uttarakhand", "West Bengal", "Jammu and Kashmir"),
  formation_year = c(1956, 1987, 1950, 1950, 2000, 1987, 1960, 1966, 1971, 2000, 1956, 1956, 
                     1956, 1960, 1972, 1972, 1987, 1963, 1950, 1966, 1949, 1975, 1956, 2014, 
                     1972, 1950, 2000, 1950, 1954)
)

# Join with merged_data to add formation_years, then calculate the duration
merged_data <- merged_data %>%
  left_join(formation_year, by = "States") %>%
  mutate(duration = year - formation_year,
         duration = case_when(
           duration < 0 ~ 0,  # Remplace les valeurs négatives par 0
           TRUE ~ duration     # Garde les autres valeurs
         ))

merged_data$secession<-merged_data$intensity_standardisee_secession
merged_data$creation<-merged_data$intensity_standardisee_creation
merged_data$natural<-merged_data$intensity_standardisee_natural

# === 7. IV regressions (2SLS) ===
iv_model_secession <- ivreg(
  secession ~ decentralization + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

iv_model_creation <- ivreg(
  creation ~ decentralization + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

iv_model_natural <- ivreg(
  natural ~ decentralization + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

# Display regression results
stargazer(iv_model_secession, iv_model_creation, iv_model_natural, type = "text")


# === 7. IV regressions (2SLS) for autonomy===
merged_data$agri_gdp<-(merged_data$Agriculture/merged_data$GDP)*100
iv_model_secession1 <- ivreg(
  secession ~ autonomy + loggdppc + urbanization + logpop + trend + fiscal_rule + coal_opening |
    loggdppc + urbanization + logpop + trend + fiscal_rule + coal_opening + index+duration,
  data = merged_data
)

iv_model_creation1 <- ivreg(
  creation ~ autonomy + loggdppc + urbanization + logpop + trend + fiscal_rule + coal_opening|
    loggdppc + urbanization + logpop + trend + fiscal_rule + coal_opening + index+duration,
  data = merged_data
)

iv_model_natural1 <- ivreg(
  natural ~ autonomy + loggdppc + urbanization + logpop + trend + fiscal_rule + coal_opening |
    loggdppc + urbanization + logpop + trend + fiscal_rule + coal_opening + index+duration,
  data = merged_data
)

# Display regression results
stargazer(iv_model_secession1, iv_model_creation1, iv_model_natural1, type = "text")
# === 8. Visualizations ===
ggplot(merged_data, aes(x = States, y = intensity_standardisee_natural)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Intensity of Secessionist Events", x = "State", y = "Standardized Intensity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
saveRDS(merged_data, "C:/Cheick/Thèse doctorale/Ethnic conflict/data/data R//my_data1.rds")


# Extract results for each IV model
results_secession_decentralization <- tidy(iv_model_secession, conf.int = TRUE) %>%
  filter(term == "decentralization") %>%
  mutate(model = "Secession", variable = "Decentralization")

results_creation_decentralization <- tidy(iv_model_creation, conf.int = TRUE) %>%
  filter(term == "decentralization") %>%
  mutate(model = "Creation", variable = "Decentralization")

results_natural_decentralization <- tidy(iv_model_natural, conf.int = TRUE) %>%
  filter(term == "decentralization") %>%
  mutate(model = "Natural Resources", variable = "Decentralization")

results_secession_autonomy <- tidy(iv_model_secession1, conf.int = TRUE) %>%
  filter(term == "autonomy") %>%
  mutate(model = "Secession", variable = "Autonomy")

results_creation_autonomy <- tidy(iv_model_creation1, conf.int = TRUE) %>%
  filter(term == "autonomy") %>%
  mutate(model = "Creation", variable = "Autonomy")

results_natural_autonomy <- tidy(iv_model_natural1, conf.int = TRUE) %>%
  filter(term == "autonomy") %>%
  mutate(model = "Natural Resources", variable = "Autonomy")

# Combine results into a single data frame
combined_results <- bind_rows(
  results_secession_decentralization, results_creation_decentralization, results_natural_decentralization)

# Plot coefficients with confidence intervals for both decentralization and autonomy
ggplot(combined_results, aes(x = model, y = estimate, ymin = conf.low, ymax = conf.high, color = variable)) +
  geom_pointrange(position = position_dodge(width = 0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "IV Regression Results for Decentralization",
    x = "Outcome Variable",
    y = "Coefficient Estimate",
    caption = "Error bars represent 95% confidence intervals."
  ) +
  scale_color_manual(values = c("Decentralization" = "blue", "Autonomy" = "green")) # Change colors as needed

combined_results1 <- bind_rows(results_secession_autonomy, results_creation_autonomy, results_natural_autonomy
)

# Plot coefficients with confidence intervals for both decentralization and autonomy
ggplot(combined_results1, aes(x = model, y = estimate, ymin = conf.low, ymax = conf.high, color = variable)) +
  geom_pointrange(position = position_dodge(width = 0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "IV Regression Results for Autonomy",
    x = "Outcome Variable",
    y = "Coefficient Estimate",
    caption = "Error bars represent 95% confidence intervals."
  ) +
  scale_color_manual(values = c("Decentralization" = "blue", "Autonomy" = "green")) # Change colors as needed

write_xlsx(merged_data, "C:/Cheick/Thèse doctorale/Ethnic conflict/data/IV_conflicts.xlsx")

########Autonomy and decentralization intensity
merged_data <- merged_data %>%
  group_by(year) %>%
  mutate(
    mean_intensity_secession = mean(autonomy, na.rm = TRUE),
    sd_intensity_secession = sd(autonomy, na.rm = TRUE),
    random_autonomy = (autonomy - mean_intensity_secession) / sd_intensity_secession,
    mean_intensity_creation = mean(decentralization, na.rm = TRUE),
    sd_intensity_creation = sd(decentralization, na.rm = TRUE),
    random_decentralization = (decentralization - mean_intensity_creation) / sd_intensity_creation
  ) %>%
  ungroup()
merged_data <- merged_data %>%
  mutate(
    random_decentralization = ifelse(is.na(random_decentralization), 0, random_decentralization),
    random_autonomy = ifelse(is.na(random_autonomy), 0, random_autonomy),
    intensity_standardisee_natural = ifelse(is.na(intensity_standardisee_natural), 0, intensity_standardisee_natural)
  )
iv_model_secession <- ivreg(
  secession ~ random_decentralization + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

iv_model_creation <- ivreg(
  creation ~ random_decentralization + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

iv_model_natural <- ivreg(
  natural ~ random_decentralization + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

# Display regression results
stargazer(iv_model_secession, iv_model_creation, iv_model_natural, type = "text")

###Results for autonomy
iv_model_secession <- ivreg(
  secession ~ random_autonomy + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

iv_model_creation <- ivreg(
  creation ~ random_autonomy + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

summary(merged_data$random_autonomy)
iv_model_natural <- ivreg(
  natural ~ random_autonomy + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)
# Display regression results
stargazer(iv_model_secession, iv_model_creation, iv_model_natural, type = "text")

summary(merged_data$secession)
summary(merged_data$creation)
summary(merged_data$natural)

merged_data$random_secession<-runif(887, min=-0.58684, max=5.56249)
merged_data$random_creation<-runif(887, min=-0.25567, max=5.57048)
merged_data$random_natural<-runif(887, min=-0.24761, max=5.57048)
summary(merged_data$random_secession)

iv_model_secession <- ivreg(
  random_secession ~ decentralization + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

iv_model_creation <- ivreg(
  random_creation ~ decentralization + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

iv_model_natural <- ivreg(
  random_natural ~ decentralization + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

# Display regression results
stargazer(iv_model_secession, iv_model_creation, iv_model_natural, type = "text")

###Results for autonomy
iv_model_secession <- ivreg(
  random_secession ~ autonomy + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

iv_model_creation <- ivreg(
  random_creation ~ autonomy + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

iv_model_natural <- ivreg(
  random_natural ~ autonomy + loggdppc + urbanization + logpop + trend + fiscal_rule |
    loggdppc + urbanization + logpop + trend + fiscal_rule + index+duration,
  data = merged_data
)

# Display regression results
stargazer(iv_model_secession, iv_model_creation, iv_model_natural, type = "text")


### Sample Code for *ivDiag* ###


##########################
## Installation
##########################

 #library(remotes)
 #install_github("apoorvalal/ivDiag")

 #install.packages("ivDiag")

##########################
## Example 1: Rueda (2017)
##########################
merged_data1<-merged_data

#rm(list=ls())
library(ivDiag)
#data(ivDiag)
#ls()
#merged_data$loggdppc<-log(merged_data$gdppc)
Y <- "secession" # Y: outcome of interest
D <-"autonomy" # D: endogenous treatment
Z <- "index" # Z: instrumental variable
controls <- c("loggdppc", "urbanization", "logpop", "trend", "fiscal_rule") # covariates of control variables
cl <- "id" # clusters
weights <- FE <- NULL # no weights or fixed effects
merged_data <- na.omit(merged_data[, c("States", "secession", "index", "loggdppc", "urbanization", "logpop", "trend", "fiscal_rule", "autonomy", "duration", "natural")])
# Assuming `States` is a column in your dataframe `data`
merged_data$id <- as.integer(as.factor(merged_data$States))

# first stage (raw)
par(mar = c(4, 4, 2, 2))
plot(merged_data$index, merged_data$autonomy, col = "#777777", cex = 0.5, 
     main = "Raw Data", xlab = "Instrument", ylab = "Treatment")
abline(lm(autonomy ~ index, data = merged_data), col = 2, lwd = 2, lty = 2)

# first stage (partial out)
z_res <- lm(index ~  loggdppc + urbanization + logpop + trend + fiscal_rule, data = merged_data)$residuals
d_res <- lm(autonomy ~  loggdppc + urbanization + logpop + trend + fiscal_rule, data = merged_data)$residuals
plot(z_res, d_res, col = "#777777", cex = 0.5, main = "Covariates Partialled Out", 
     xlab = "Residualized Instrument", ylab = "Residualized Treatment")
abline(lm(d_res ~ z_res), col = 2, lwd = 2, lty = 2)

# omnibus funcdtion
library(ivDiag)
g <- ivDiag(data = merged_data, Y=Y, D = D, Z = Z, controls = controls, cl = cl)
names(g)
g

# plot coefficients and CIs
plot_coef(g)

plot_coef(g, ols.methods = c("analy"), iv.methods = c("analy", "ar", "tf"),
  main = "Comparison between OLS and IV Estimates", ylab = "Estimates", 
  grid = FALSE, stats = FALSE, ylim = c(-2, 0.5))

## separate functions
eff_F(data = merged_data, Y = Y, D = D, Z = Z, controls = controls, cl = cl, 
      FE = NULL, weights = NULL)

AR_test(data = merged_data, Y = Y, D = D, Z = Z, controls = controls, cl = cl, 
        FE = NULL, weights = NULL)

tF(coef = -0.9835, se = 0.1540, Fstat = 8598)

# without bootstrap or AR test
g1<- ivDiag(data = merged_data, Y=Y, D = D, Z = Z, controls = controls,     cl = cl, bootstrap = FALSE, run.AR = FALSE)
g1
plot_coef(g1, ylim = c(-2, 1))

g <- ivDiag(data = merged_data, Y = Y, D = D, Z = Z, controls = controls, 
            weights = weights)
g
plot_coef(g)
#install.packages("estimatr")
library(estimatr)
zfs <- lm_robust(secession ~ index + loggdppc + urbanization + logpop + trend + fiscal_rule, data = merged_data, 
                 weights = weights, se_type = "HC1")

summary(zfs)$coefficients["index", 1:2]
ltz_out <- ltz(data = merged_data, Y = Y, D = D, Z = Z, 
    controls = controls, weights = weights, 
    prior = c(0.178, 0.137))
ltz_out
plot_ltz(ltz_out, xlim = c(-0.5, 7))
#plot_ltz(iv_est = ltz_out$iv[1:2], ltz_est = ltz_out$ltz[1:2], 
#         prior = ltz_out$prior)


#install.packages("quantreg")
library(quantreg)

taus <- seq(0.1, 0.9, by = 0.2)
models <- lapply(taus, function(tau) rq(creation ~ autonomy + loggdppc + urbanization + logpop + trend + fiscal_rule , data = merged_data1, tau = tau))
wt_coefs <- sapply(models, function(model) coef(model)["autonomy"])
plot_data <- data.frame(Quantile = taus, Coefficient = wt_coefs)
library(ggplot2)

ggplot(plot_data, aes(x = Quantile, y = Coefficient)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Quantile Regression Coefficients for 'autonomy'",
    x = "Quantile",
    y = "Coefficient"
  ) +
  theme_minimal()
