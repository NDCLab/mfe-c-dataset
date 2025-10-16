# This script will run stats on mfe_c data.
# Author: Kianoosh Hosseini at NDCLab @FIU (https://Kianoosh.info; https://NDClab.com)
# Last Update: 2025-10-16 (YYYY-MM-DD)

library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(psycho)
library(car) # for functions like Anova(); Anova() is different from anova()
library(lme4)
library(ggplot2)
library(emmeans)
library(report)
library(sjPlot)
library(effsize)
library(effectsize)

# Loading mfe_c_face data

proje_wd <- "/Github_Repos/mfe-c-dataset"
setwd(proje_wd)

processed_file_input <- paste(proje_wd, "derivatives", "psychopy", "stat_output", "mfe-c-face", sep ="/", collapse = NULL) # input data directory

face_df <-  read.csv(file = paste(processed_file_input, "processed_data_mfe_c_face_Proj_v1.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))

# Keep the columns that we will need
selected_columns <- face_df[, c("participant_id", "flankEff_meanACC", "congAcc", "incongAcc", "congCorr_meanRT", "incongCorr_meanRT", "committed_errors", "error_proRec", "correct_proRec", "overall_proRec", "proRec_error_minus_correct", "scaared_b_scrdSoc_s1_r1_e1")]
face_df <- selected_columns

# Check the values in every column in main_df and remove the outliers based on +- 3SD.
# Write a function that removes the outliers from an array
remove_outliers <- function(x) {
  mean_x <- mean(as.numeric(x), na.rm = TRUE)
  sd_x <- sd(as.numeric(x), na.rm = TRUE)
  for (xx in 1:length(x)){
    if (!is.na(x[xx])){
      if (x[xx] < (mean_x - 3*sd_x) | x[xx] > (mean_x + 3*sd_x)){
        x[xx] <- NA
      }
    }
  }
  return(x)
}
# apply this outlier removing function to all the columns in the dataframe except for participant ID column.
new_face_df <- face_df
new_face_df[-c(1, ncol(new_face_df))] <- apply(face_df[-c(1, ncol(face_df))], 2, remove_outliers)
face_df <- new_face_df

face_df$group <- 'face' # adding a column that specifies the group (will be needed below)

#Working directory should be the Psychopy experiment directory.

processed_file_input <- paste(proje_wd, "derivatives", "psychopy", "stat_output", "mfe-c-object", sep ="/", collapse = NULL) # input data directory

object_df <-  read.csv(file = paste(processed_file_input, "processed_data_mfe_c_object_Proj_v1.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))
# Keep the columns that we will need
selected_columns <- object_df[, c("participant_id", "flankEff_meanACC", "congAcc", "incongAcc", "congCorr_meanRT", "incongCorr_meanRT", "committed_errors", "error_proRec", "correct_proRec", "overall_proRec", "proRec_error_minus_correct", "scaared_b_scrdSoc_s1_r1_e1")]
object_df <- selected_columns

# apply this outlier removing function to all the columns in the dataframe except for participant ID column.
new_object_df <- object_df
new_object_df[-c(1, ncol(new_object_df))] <- apply(object_df[-c(1, ncol(object_df))], 2, remove_outliers)
object_df <- new_object_df

object_df$group <- 'object' # adding a column that specifies the group (will be needed below)

# binding these two dataframes two create a single main_df
main_df <- rbind(face_df, object_df)

# Save the data
# write the extracted and computed summary scores to disk
temp_dir1 <- "/Github_Repos/mfe-c-dataset/derivatives/psychopy/stat_output"
write.csv(main_df, paste(temp_dir1, "mfe_c_final_data.csv", sep = "/", collapse = NULL), row.names=FALSE)
#########


##### To replicate, this final data is available on github repository (see the directory above within the mfe-c-dataset repo).
##### Load this final data here and then you can run the stats below.
main_df <-  read.csv(file = paste("/Github_Repos/mfe-c-dataset/derivatives/psychopy/stat_output/mfe_c_final_data.csv", sep ="/", collapse = NULL), stringsAsFactors = FALSE, na.strings=c("", "NA"))

###################################### accuracy ~ congruency*group MODEL #############################################
# Reshape to Long Format
long_main_df1 <- main_df %>%
  pivot_longer(
    cols = c(congAcc, incongAcc),  # Columns to pivot
    names_to = "flanker_congruency",       # Name of the new column for variable names
    values_to = "accuracy",        # Name of the new column for values
    names_pattern = "(.*)Acc" # Extract subject name from column name
  )


# Converting categorical variables (group and flanker accuracy) to factors
long_main_df1$group <- as.factor(long_main_df1$group)
long_main_df1$flanker_congruency <- as.factor(long_main_df1$flanker_congruency)


#
# Load the lmerTest package
library(lmerTest)

# I ran the following mixed effects model. However, the model violated normality of residuals and homoscedasticity
# assumptions according to visual inspections.

# Because of this I am gonna run GLMM with a beta distribbution because my outcome (accuracy) is positive and between 0 and 1 and is a proportion.

############## not used for the paper because of assumption violations ##########
# Fit the mixed-effects model with random intercepts
model1 <- lmer(accuracy ~ flanker_congruency * group + (1 | participant_id), data = long_main_df1)

# Display the model summary
summary(model1)
# confidence intervals
confint(model1)

# check_heteroscedasticity() using performance library
library(performance)
check_heteroscedasticity(model1)

qqnorm(resid(model1))
qqline(resid(model1))

hist(resid(model1))
hist(long_main_df1$accuracy)
# Get the fitted values
fitted_values <- fitted(model1)

# Get the residuals
residuals <- residuals(model1)

# Create the plot
plot(fitted_values, residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")

# Transform my dependent variable (accuracy) to better meet normality assumptions:
# Log transformation (if no zeros)
long_main_df1$log_accuracy <- log(long_main_df1$accuracy)
model_log <- lmer(log_accuracy ~ flanker_congruency * group * scaared_b_scrdSoc_s1_r1_e1+ (1 | participant_id), data = long_main_df1)

qqnorm(resid(model_log))
qqline(resid(model_log))

# Square root transformation
long_main_df1$sqrt_accuracy <- sqrt(long_main_df1$accuracy)
model_sqrt <- lmer(sqrt_accuracy ~ flanker_congruency * group * scaared_b_scrdSoc_s1_r1_e1+ (1 | participant_id), data = long_main_df1)

qqnorm(resid(model_sqrt))
qqline(resid(model_sqrt))

# Logit transformation (if accuracy is proportion between 0-1)
# as accuracy can have values of for some of the subjects. I perform some adjustments.
long_main_df1$adjusted_accuracy <- ifelse(long_main_df1$accuracy == 1.0, 0.999999,
                                 ifelse(long_main_df1$accuracy == 0.0, 0.000001,
                                        long_main_df1$accuracy)) # for beta distribution, the values cannot be 0s and 1s. So, I convert them to avoid error.

long_main_df1$logit_accuracy <- car::logit(long_main_df1$adjusted_accuracy)
model_logit <- lmer(logit_accuracy ~ flanker_congruency * group * scaared_b_scrdSoc_s1_r1_e1 + (1 | participant_id), data = long_main_df1)

qqnorm(resid(model_sqrt))
qqline(resid(model_sqrt))


############################################# GLMM #####################################################################


## Running GLMM with a beta distribution #####################
# This option is used:
# - assuming only the rates are available without the counts.
# - Using a binomial model with rates alone (without counts) can lead to incorrect variance estimation.
# - Beta regression handles the continuous proportion data more naturally and doesn't require knowledge of the number of trials.

# glmmTMB package supports beta ditribution. So, I use this library.
library(glmmTMB)
long_main_df1$accuracy <- ifelse(long_main_df1$accuracy == 1.0, 0.999999,
                                 ifelse(long_main_df1$accuracy == 0.0, 0.000001,
                                        long_main_df1$accuracy)) # for beta distribution, the values cannot be 0s and 1s. So, I convert them to avoid error.

model1 <- glmmTMB(
  accuracy ~ flanker_congruency * group + (1 | participant_id),
  data = long_main_df1,
  family = beta_family(link = "logit")
)

# Display the model summary
summary(model1)


# Get the table with standardized coefficients for GLMM
tab_model(
  model1,
  show.std = "refit",       # Standardize using the refit method
  show.se = TRUE,          # Show standard errors
  show.stat = TRUE,         # Show z-statistic
  show.p = TRUE,           # Show p-values
  show.ci = 0.95,          # Show 95% confidence intervals (default)
  transform = NULL,   # Important for Beta regression: Show coefficients on the logit scale
  dv.labels = "Beta Regression (logit link)", # Label for the dependent variable and model type
  digits = 3,       # Set decimal places for most values to 3
  digits.p = 3      # Set decimal places for p-values to 3
)

# Below eta squared and plotting are for CNS 2025 Poster.
# Calculate partial eta squared
aov <- Anova(model1, type=2, test="F")
eta_squared(aov, alternative="two.sided")


my_plot <- ggplot(long_main_df1, aes(x = flanker_congruency, y = accuracy, fill = flanker_congruency)) +
    geom_boxplot(alpha = 0.8, width = 0.6, outlier.shape = 21, outlier.size = 2, outlier.alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
    scale_fill_brewer(palette = "Blues") +
    scale_x_discrete(labels = c("Congruent", "Incongruent")) + # Added labels here
    labs(
      title = "Flanker Accuracy by Congruency",
      x = "Congruency",
      y = "Accuracy (%)",
    ) +
    theme_minimal() +
    theme(
      text = element_text(color = "black", size = 24),  # Base font size increased by 200% and black color
      plot.title = element_text(face = "bold", size = 32, hjust = 0.5, color = "black"),  # 200% increase
      axis.title = element_text(face = "bold", size = 24, color = "black"),  # 200% increase
      axis.text = element_text(size = 20, color = "black"),  # 200% increase
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray80", fill = NA),
      plot.margin = margin(20, 20, 20, 20)
    ) +
    scale_y_continuous(limits = c(NA, 1), labels = scales::percent)

# Save the plot with 600 DPI and specified dimensions
destination_path <- "your_path_here" # Replace with your desired path
filename <- "flanker_acc_plot_600dpi.jpg"
filepath <- file.path(destination_path, filename)

# Check if the directory exists (redundant but good to double-check)
if (!dir.exists(destination_path)) {
  print(paste("Error: Directory does not exist:", destination_path))
} else {
  print(paste("Directory exists:", destination_path))

  tryCatch({
    ggsave(filepath, plot = my_plot, dpi = 600, width = 10, height = 8)
    print(paste("Plot saved to:", filepath))
  }, error = function(e) {
    print(paste("Error saving plot:", e$message))
  })
}
#### END OF CNS 2025 plotting section


##### RULING ALTERNATIVE HYPOTHESIS (Running the same GLMM model as above but this time with including SA)

model1b <- glmmTMB(
  accuracy ~ flanker_congruency * group * scaared_b_scrdSoc_s1_r1_e1 + (1 | participant_id),
  data = long_main_df1,
  family = beta_family(link = "logit")
)

# Display the model summary
summary(model1b)


# Get the table with standardized coefficients for GLMM
tab_model(
  model1b,
  show.std = "refit",       # Standardize using the refit method
  show.se = TRUE,          # Show standard errors
  show.stat = TRUE,         # Show z-statistic
  show.p = TRUE,           # Show p-values
  show.ci = 0.95,          # Show 95% confidence intervals (default)
  transform = NULL,   # Important for Beta regression: Show coefficients on the logit scale
  dv.labels = "Beta Regression (logit link)", # Label for the dependent variable and model type
  digits = 3,       # Set decimal places for most values to 3
  digits.p = 3      # Set decimal places for p-values to 3
)

########################################################################################################################




###################################### RT ~ congruency * group Model #############################################

# Reshape to Long Format
long_main_df2 <- main_df %>%
  pivot_longer(
    cols = c(congCorr_meanRT, incongCorr_meanRT),  # Columns to pivot
    names_to = "flanker_congruency",       # Name of the new column for variable names
    values_to = "RT",        # Name of the new column for values
    names_pattern = "(.*)Corr_meanRT" # Extract subject name from column name
  )


# Converting categorical variables (group and flanker accuracy) to factors
long_main_df2$group <- as.factor(long_main_df2$group)
long_main_df2$flanker_congruency <- as.factor(long_main_df2$flanker_congruency)

#
# Load the lmerTest package
library(lmerTest)



# Fit the mixed-effects model with random intercepts
model2 <- lmer(RT ~ flanker_congruency * group + (1 | participant_id), data = long_main_df2)

# Display the model summary
summary(model2)

# confidence intervals
round(confint(model2),3)

# Get the table with standardized coefficients for LMM; reported in the paper
# standardized coefs from the table below have been reported in the paper.
tab_model(
  model2,
  show.std = TRUE,
  show.se = TRUE,
  show.ci = 0.95,
  show.stat = TRUE,
  digits = 3,       # Set decimal places for most values to 3
  digits.p = 3      # Set decimal places for p-values to 3
)

my_plot <- ggplot(long_main_df2, aes(x = flanker_congruency, y = RT, fill = flanker_congruency)) +
  geom_boxplot(alpha = 0.8, width = 0.6, outlier.shape = 21, outlier.size = 2, outlier.alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.4, size = 2) +
  scale_fill_brewer(palette = "Blues", labels = c("Congruent", "Incongruent")) + # Added labels here
  scale_x_discrete(labels = c("Congruent", "Incongruent")) + # Added labels here
  labs(
    title = "Flanker Reaction Time by Congruency",
    x = "Congruency",
    y = "Reaction Time (seconds)",
  ) +
  theme_minimal() +
  theme(
    text = element_text(color = "black", size = 30),
    plot.title = element_text(face = "bold", size = 30, hjust = 0.5, color = "black"),
    axis.title = element_text(face = "bold", size = 30, color = "black"),
    axis.text = element_text(size = 30, color = "black"),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, " s"))

# Save the plot with 600 DPI and specified dimensions
destination_path <- "/Users/kihossei/Desktop" # Replace with your desired path
filename <- "flanker_rt_plot_600dpi.jpg"
filepath <- file.path(destination_path, filename)

# Check if the directory exists (redundant but good to double-check)
if (!dir.exists(destination_path)) {
  print(paste("Error: Directory does not exist:", destination_path))
} else {
  print(paste("Directory exists:", destination_path))

  tryCatch({
    ggsave(filepath, plot = my_plot, dpi = 600, width = 10, height = 8)
    print(paste("Plot saved to:", filepath))
  }, error = function(e) {
    print(paste("Error saving plot:", e$message))
  })
}


# Calculate partial eta squared
aov2 <- Anova(model2, type=2, test="F")
eta_squared(aov2, alternative="two.sided")

# Standardize the model using different methods
library(sjstats)
std_refit <- standardize(model2, method = "refit")


# check_heteroscedasticity() using performance library
library(performance)
check_heteroscedasticity(model2)

qqnorm(resid(model2))
qqline(resid(model2))

hist(resid(model2))
hist(long_main_df2$RT)
# Get the fitted values
fitted_values <- fitted(model2)

# Get the residuals
residuals <- residuals(model2)

# Create the plot
plot(fitted_values, residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values")

# Visual inspections show that this model is fine in terms of homosscedasticity and normality of residuals.


##### RULING ALTERNATIVE HYPOTHESIS (Running the same model as above but this time with including SA)

# Fit the mixed-effects model with random intercepts
model2b <- lmer(RT ~ flanker_congruency * group * scaared_b_scrdSoc_s1_r1_e1 + (1 | participant_id), data = long_main_df2)

# Display the model summary
summary(model2b)

tab_model(
  model2b,
  show.std = TRUE,
  show.se = TRUE,
  show.ci = 0.95,
  show.stat = TRUE,
  digits = 3,       # Set decimal places for most values to 3
  digits.p = 3      # Set decimal places for p-values to 3
)



########################################################################################################################

######################################## Proportion Recognized ~ accuracy * group MODEL #############################################
# Reshape to Long Format
long_main_df3 <- main_df %>%
  pivot_longer(
    cols = c(error_proRec, correct_proRec),  # Columns to pivot
    names_to = "flanker_accuracy",       # Name of the new column for variable names
    values_to = "proRec",        # Name of the new column for values
    names_pattern = "(.*)_proRec" # Extract subject name from column name
  )


# Converting categorical variables (group and flanker accuracy) to factors
long_main_df3$group <- as.factor(long_main_df3$group)
long_main_df3$flanker_accuracy <- as.factor(long_main_df3$flanker_accuracy)

#
# Load the lmerTest package
library(lmerTest)

# Fit the mixed-effects model with random intercepts
model3 <- lmer(proRec ~ flanker_accuracy * group + (1 | participant_id), data = long_main_df3)

# Display the model summary
summary(model3)
# confidence intervals
round(confint(model3),3)

# Get the table with standardized coefficients for LMM
# the results of this table have been reported in the paper.
tab_model(
  model3,
  show.std = TRUE,
  show.se = TRUE,
  show.ci = 0.95,
  show.stat = TRUE,
  digits = 3,       # Set decimal places for most values to 3
  digits.p = 3      # Set decimal places for p-values to 3
)

# Calculate partial eta squared
aov3 <- Anova(model3, type=2, test="F")
eta_squared(aov3, alternative="two.sided")

# Standardize the model using different methods
library(sjstats)

standardize(model3, method = "refit")

# Visual inspections show that this model is fine in terms of homosscedasticity and normality of residuals.


########################################################################################################################


######################################## Memory Bias for error events ~ scaared SA*group MODEL #############################################

# Regression
main_df$group <- as.factor(main_df$group)

# The following model satisfies the linear regression normality, homoscedasticity, linearity, and independence of errors assumptions.

fit <- lm(proRec_error_minus_correct ~ scaared_b_scrdSoc_s1_r1_e1*group, data = main_df)
summary(fit)
confint(fit, level=0.95)

tab_model(
  fit,
  show.std = TRUE,
  show.se = TRUE,
  show.ci = 0.95,
  show.stat = TRUE,
  digits = 3,       # Set decimal places for most values to 3
  digits.p = 3      # Set decimal places for p-values to 3
)
# Plotting
my_plot <- ggplot(main_df, aes(x=scaared_b_scrdSoc_s1_r1_e1, y=proRec_error_minus_correct, group = group)) +
  geom_point(aes(color = group), size = 4) +
  geom_smooth(method="lm", aes(color = group, fill = group)) +
  scale_color_manual(values = c("face" = "#E69F00", "object" = "#56B4E9")) +
  scale_fill_manual(values = c("face" = "#E69F00", "object" = "#56B4E9")) +
  labs(x = "SA Symptoms\n(SCAARED SA scores)",
       y = "Memory Bias for Error Events\n(Error - Correct Proportion Recognized %)") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        text = element_text(size = 20))



# Save the plot with 600 DPI and specified dimensions
destination_path <- "/Users/kihossei/Desktop"
filename <- "interaction_plot_600dpi.jpg"
filepath <- file.path(destination_path, filename)

# Check if the directory exists (redundant but good to double-check)
if (!dir.exists(destination_path)) {
  print(paste("Error: Directory does not exist:", destination_path))
} else {
  print(paste("Directory exists:", destination_path))

  tryCatch({
    ggsave(filepath, plot = my_plot, dpi = 600, width = 10, height = 8)
    print(paste("Plot saved to:", filepath))
  }, error = function(e) {
    print(paste("Error saving plot:", e$message))
  })
}


# 1. Create Standardized Variables
main_df$scaared_b_scrdSoc_s1_r1_e1_std <- scale(main_df$scaared_b_scrdSoc_s1_r1_e1)
main_df$proRec_error_minus_correct_std <- scale(main_df$proRec_error_minus_correct)

fit2 <- lm(proRec_error_minus_correct_std ~ scaared_b_scrdSoc_s1_r1_e1_std*group, data = main_df)
summary(fit2)

# Simple slopes

library(reghelper)
simple_slopes(fit)
simple_slopes(fit2)
emtrends(fit, ~ group, var="scaared_b_scrdSoc_s1_r1_e1") #gives confidence intervals
emtrends(fit2, ~ group, var="scaared_b_scrdSoc_s1_r1_e1_std") #gives confidence intervals

library(sjstats)

standardize(fit, method = "refit")

########## assumptions check for the regression model above

# 1. Linearity
# Create diagnostic dataframe
diagnostics <- data.frame(
  fitted = fitted(fit),
  residuals = residuals(fit),
  standardized = rstandard(fit)
)

# Residual vs Fitted plot for linearity and homoscedasticity
ggplot(diagnostics, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted values", y = "Residuals")

# 2. Independence of Errors
# Durbin-Watson test
library(car)
durbinWatsonTest(fit)

# 3. Homoscedasticity
check_heteroscedasticity(fit)

# 4. Normality of Errors
# Histogram of residuals
hist(resid(fit))

# Q-Q plot of residuals
plot(fit, which = 2)  # Normal Q-Q

# Shapiro-Wilk test for normality
shapiro.test(resid(fit))

# Inspections show that this model satisfies the assumptions.
######### END of assumptions check


############################################ COMPUTE AGE FOR ALL PARTICIPANTS Before exclusion #########################

##### to compute age based on the month and year of birth.
### Loading RedCap questionnaire data for mfe-c-face
redcapDat_face <- read.csv(file = "Github_Repos/mfe-c-face-dataset/derivatives/redcap/Memoryforerrorcface_SCRD_2024-06-13_1453.csv")
### Loading RedCap questionnaire data for mfe-c-object
redcapDat_object <- read.csv(file = "/Github_Repos/mfe-c-object-dataset/derivatives/redcap/202402v0memoryforerr_SCRD_2024-09-10_1142.csv")

# Keep only the data collection date column
redcapDat_face_dataCollection_date <- redcapDat_face %>% dplyr::select(all_of(c("record_id", "demo_c_s1_r1_e1_timestamp")))
redcapDat_object_dataCollection_date <- redcapDat_object %>% dplyr::select(all_of(c("record_id", "demo_c_s1_r1_e1_timestamp")))

### Loading RedCap questionnaire data for mfe-c-face (this dataframe has only months and years of births)
redcapDat_face_mob_yob <- read.csv(file = "/Desktop/202311v0mfe-c-face-MonthYearOfBirth_DATA_2025-07-29_2141.csv")

# adding the date of data collection to this dataframe
redcapDat_face_mob_yob <- redcapDat_face_mob_yob %>%
  left_join(redcapDat_face_dataCollection_date, by = "record_id")

### Loading RedCap questionnaire data for mfe-c-object
redcapDat_object_mob_yob <- read.csv(file = "Desktop/202402v0-mfe-c-object-MonthYearOfBirth_DATA_2025-07-29_2154.csv")

# adding the date of data collection to this dataframe
redcapDat_object_mob_yob <- redcapDat_object_mob_yob %>%
  left_join(redcapDat_object_dataCollection_date, by = "record_id")

redcapDat <- rbind(redcapDat_face_mob_yob, redcapDat_object_mob_yob)

# remove participants that did not participate although started the redcap
redcapDat_filtered <- redcapDat[redcapDat$record_id != "260009", ]
redcapDat_filtered <- redcapDat_filtered[redcapDat_filtered$record_id != "260073", ] # fully collected data but not included data analysis as this person was beyond the 70 participants we aimed initially
redcapDat <- redcapDat_filtered


# Convert birth od date months and years (mob and yob) columns to a single Date object
library(lubridate)
redcapDat <- redcapDat %>%
  mutate(
    dob = make_date(demo_c_yob_s1_r1_e1, demo_c_mob_s1_r1_e1, 1) # Assume 1st of the month for DOB
  )
# Convert the datetime column to a POSIXct object
redcapDat$dataCollection_datetime <- ymd_hms(redcapDat$demo_c_s1_r1_e1_timestamp)

# Function to calculate age from birth year to data colllection date
redcapDat$age <- as.period(interval(start=redcapDat$dob, end=redcapDat$demo_c_s1_r1_e1_timestamp), unit = "years")

# Function to convert age string to decimal years
convert_age_to_decimal <- function(age_str) {
  # Extract years and months using regular expressions
  years <- as.numeric(gsub("y.*", "", age_str))
  months <- as.numeric(gsub(".* ([0-9]+)m.*", "\\1", age_str))

  # Calculate decimal years
  decimal_years <- years + (months / 12)

  return(decimal_years)
}
# Compute age in decimal
for(i in 1:nrow(redcapDat)){
  redcapDat$decimal_age[i] <- convert_age_to_decimal(redcapDat$age[i])
}

round(mean(redcapDat$decimal_age),2)
round(sd(redcapDat$decimal_age), 2)
############################################ END OF COMPUTING AGE FOR ALL PARTICIPANTS #################################

############################################ Counting the sex at birth FOR ALL PARTICIPANTS  #################################

### Loading RedCap questionnaire data for mfe-c-face
redcapDat_face <- read.csv(file = "/Github_Repos/mfe-c-face-dataset/derivatives/redcap/Memoryforerrorcface_SCRD_2024-06-13_1453.csv")
### Loading RedCap questionnaire data for mfe-c-object
redcapDat_object <- read.csv(file = "/Github_Repos/mfe-c-object-dataset/derivatives/redcap/202402v0memoryforerr_SCRD_2024-09-10_1142.csv")

# Keep only the sex column
redcapDat_face_sex <- redcapDat_face %>% dplyr::select(all_of(c("record_id", "demo_c_sex_s1_r1_e1")))
redcapDat_object_sex <- redcapDat_object %>% dplyr::select(all_of(c("record_id", "demo_c_sex_s1_r1_e1")))

redcapDat <- rbind(redcapDat_face_sex, redcapDat_object_sex)

# remove participants that did not participate although started the redcap
redcapDat_filtered <- redcapDat[redcapDat$record_id != "260009", ]
redcapDat_filtered <- redcapDat_filtered[redcapDat_filtered$record_id != "260060", ] # Not collected
redcapDat_filtered <- redcapDat_filtered[redcapDat_filtered$record_id != "260073", ] # fully collected data but not included data analysis as this person was beyond the 70 participants we aimed initially
redcapDat <- redcapDat_filtered

sex_summary <- redcapDat %>%
  count(demo_c_sex_s1_r1_e1, name = "count") %>%
  arrange(desc(count))

print("Gender counts using dplyr:")
print(sex_summary)

# 1= Male; 2= Female; 3= intersex; 4 = other; 5 = unknown; 6 = prefer not to say.

############################################ END OF Counting the sex FOR ALL PARTICIPANTS  ##########################

############################################ COMPUTE AGE FOR ALL PARTICIPANTS After exclusion ##########################
# Select only the 'ID' and 'decimal_age' columns from dataframe redcapDat
redcapDat_subset <- redcapDat[, c("record_id", "decimal_age")]

# Rename the column 'record_id' to 'participant_id' in dataframe 'redcapDat_subset'
redcapDat_subset <- redcapDat_subset %>% rename(participant_id = record_id)

# Merge the subset of redcapDat_subset with dataframe main_df by the 'participant_id' column
main_df <- merge(main_df, redcapDat_subset, by = "participant_id", all.x = TRUE)

# Compute age mean and SD for the face group
face_temp_df <- filter(main_df, group == "face" )

round(mean(face_temp_df$decimal_age),2)
round(sd(face_temp_df$decimal_age), 2)

# Compute age mean and SD for the object group
object_temp_df <- filter(main_df, group == "object" )

round(mean(object_temp_df$decimal_age),2)
round(sd(object_temp_df$decimal_age), 2)
############################################ END OF COMPUTING AGE For the remaining PARTICIPANTS #######################

############################################ Counting the sex FOR ALL PARTICIPANTS After exclusion  #################

# Select only the 'ID' and 'demo_c_gender_s1_r1_e1' columns from dataframe redcapDat
redcapDat_subset <- redcapDat[, c("record_id", "demo_c_sex_s1_r1_e1")]

# Rename the column 'record_id' to 'participant_id' in dataframe 'redcapDat_subset'
redcapDat_subset <- redcapDat_subset %>% rename(participant_id = record_id)

# Merge the subset of redcapDat_subset with dataframe main_df by the 'participant_id' column
main_df <- merge(main_df, redcapDat_subset, by = "participant_id", all.x = TRUE)

# Count gender for the face group
face_temp_df <- filter(main_df, group == "face" )

sex_summary <- face_temp_df %>%
  count(demo_c_sex_s1_r1_e1, name = "count") %>%
  arrange(desc(count))

print("Sex counts using dplyr for face:")
print(sex_summary)

# Count gender for the object group
object_temp_df <- filter(main_df, group == "object" )

sex_summary <- object_temp_df %>%
  count(demo_c_sex_s1_r1_e1, name = "count") %>%
  arrange(desc(count))

print("Sex counts using dplyr for object:")
print(sex_summary)


# Face: 43 women + 5 men + 1 not reported + 1 trans man + 5 nonbinary +
# Object: 43 Women +  13 men + 2 not reported + 1 nonbinary + 1 unknown cis

############################################ END OF Counting the sex FOR ALL PARTICIPANTS After exclusion  ##########



############################################ COMPUTE cronbach.alpha for SCAARED ########################################
# Compute cronbach.alpha for SCAARED
# Define file paths for your CSV files
redcap_mfe_c_face_scaaredSoc <- "/Github_Repos/mfe-c-face-dataset/derivatives/redcap/Memoryforerrorcface_SCRD_2024-06-13_1453.csv"
redcap_mfe_c_object_scaaredSoc <- "/Github_Repos/mfe-c-object-dataset/derivatives/redcap/202402v0memoryforerr_SCRD_2024-09-10_1142.csv"

# Define the columns to keep
selected_columns <- c("record_id",
                      "scaared_b_i3_s1_r1_e1",
                      "scaared_b_i10_s1_r1_e1",
                      "scaared_b_i27_s1_r1_e1",
                      "scaared_b_i34_s1_r1_e1",
                      "scaared_b_i41_s1_r1_e1",
                      "scaared_b_i42_s1_r1_e1",
                      "scaared_b_i43_s1_r1_e1")

# Function to load and select columns from a CSV file
load_and_select <- function(file_path, columns) {
  tryCatch({
    df <- read_csv(file_path)

    # Check if all specified columns exist in the dataframe
    if (!all(columns %in% names(df))) {
      missing_cols <- setdiff(columns, names(df))
      stop(paste("The following columns are missing in file:", file_path, paste(missing_cols, collapse = ", ")))
    }

    df_selected <- df %>% dplyr::select(all_of(columns))
    return(df_selected)
  }, error = function(e) {
    message(paste("Error processing file:", file_path))
    message(e)
    return(NULL) # Return NULL in case of an error
  })
}

# Load and select columns from both files
df1_selected <- load_and_select(redcap_mfe_c_face_scaaredSoc, selected_columns)
df2_selected <- load_and_select(redcap_mfe_c_object_scaaredSoc, selected_columns)

# Check if both dataframes were loaded successfully before binding
if (!is.null(df1_selected) && !is.null(df2_selected)) {
  # Bind the dataframes using bind_rows (from dplyr)
  combined_df <- bind_rows(df1_selected, df2_selected)
} else {
  message("One or both files could not be loaded. Please check the file paths and column names.")
}

combined_df <- filter(combined_df, record_id != 260009 & record_id != 260060 ) # these two participants had not completed the experiment. So, they had empty surveys.
combined_df <- filter(combined_df, record_id != 260073 ) # This participant was collected after reaching 140 sample size. So, we are not analyzing its data.


# remove the ID column
new_combined_df <- combined_df[, -1]

install.packages("ltm")
library(ltm) # for cronbach.alpha

cronbach.alpha(new_combined_df, standardized = FALSE, na.rm = TRUE) #computes raw alpha value (raw is the value that we report in the paper.

############################################ END of COMPUTING cronbach.alpha for SCAARED ###############################


############################################ COMPUTING the stats of the number of flanker trials removed based on RT ###

# For face
proje_wd <- "/GitHub_Repos/mfe-c-dataset"
flanker_proc_dat_files <- paste(proje_wd, "derivatives", "psychopy", "csv_output", "mfe-c-face", sep ="/", collapse = NULL)

## creating a list of all processed flanker data csv files in the input folder.
flanker_proc_dat_files_list <- c() # an empty list that will be filled in the next "for" loop!
csvSelect <- list.files(flanker_proc_dat_files, pattern = ".csv") # listing only csv files
for (i in 1:length(csvSelect)){
  temp_for_file <- ifelse (str_detect(csvSelect[i], "mfe_c_face_flankerDat", negate = FALSE), 1, 0)
  if (temp_for_file == 1){
    temp_list <- csvSelect[i]
    flanker_proc_dat_files_list <- c(flanker_proc_dat_files_list, temp_list)
  }
}

# Create an empty dataframe to store the results
rt_df <- data.frame(participant_id = character(),
                      num_fast_rts = numeric(),
                      stringsAsFactors = FALSE)

# Loop through each csv file
for (file in flanker_proc_dat_files_list) {
  # Read the csv file
  temp_file <- paste(flanker_proc_dat_files,file, sep = "/", collapse = NULL)
  tempDat <- read.csv(file = temp_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))

  # Get the participant ID
  participant_id <- tempDat$participant_id[1]

  # Count the number of RTs smaller than 0.15 seconds
  num_fast_rts <- sum(tempDat$current_trial_rt < 0.15, na.rm = TRUE)

  # Add the results to the dataframe
  rt_df <- rbind(rt_df, data.frame(participant_id = participant_id,
                                       num_fast_rts = num_fast_rts))
}

# Adding the object data
#
proje_wd <- "/GitHub_Repos/mfe-c-dataset"
flanker_proc_dat_files <- paste(proje_wd, "derivatives", "psychopy", "csv_output", , "mfe-c-object", sep ="/", collapse = NULL)

## creating a list of all processed flanker data csv files in the input folder.
flanker_proc_dat_files_list <- c() # an empty list that will be filled in the next "for" loop!
csvSelect <- list.files(flanker_proc_dat_files, pattern = ".csv") # listing only csv files
for (i in 1:length(csvSelect)){
  temp_for_file <- ifelse (str_detect(csvSelect[i], "mfe_c_object_flankerDat", negate = FALSE), 1, 0)
  if (temp_for_file == 1){
    temp_list <- csvSelect[i]
    flanker_proc_dat_files_list <- c(flanker_proc_dat_files_list, temp_list)
  }
}

# Loop through each csv file
for (file in flanker_proc_dat_files_list) {
  # Read the csv file
  temp_file <- paste(flanker_proc_dat_files,file, sep = "/", collapse = NULL)
  tempDat <- read.csv(file = temp_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))

  # Get the participant ID
  participant_id <- tempDat$participant_id[1]

  # Count the number of RTs smaller than 0.15 seconds
  num_fast_rts <- sum(tempDat$current_trial_rt < 0.15, na.rm = TRUE)

  # Add the results to the dataframe
  rt_df <- rbind(rt_df, data.frame(participant_id = participant_id,
                                   num_fast_rts = num_fast_rts))
}

round(mean(rt_df$num_fast_rts), 2)
round(sd(rt_df$num_fast_rts), 2)

########################################################################################################################



############################################ COMPUTING the stats of the number of surprsie trials removed based on RT ###

# For face
proje_wd <- "/GitHub_Repos/mfe-c-dataset"
surprsie_proc_dat_files <- paste(proje_wd, "derivatives", "psychopy", "csv_output", "mfe-c-face", sep ="/", collapse = NULL)

## creating a list of all processed flanker data csv files in the input folder.
surprsie_proc_dat_files_list <- c() # an empty list that will be filled in the next "for" loop!
csvSelect <- list.files(surprsie_proc_dat_files, pattern = ".csv") # listing only csv files
for (i in 1:length(csvSelect)){
  temp_for_file <- ifelse (str_detect(csvSelect[i], "mfe_c_face_surpriseDat", negate = FALSE), 1, 0)
  if (temp_for_file == 1){
    temp_list <- csvSelect[i]
    surprsie_proc_dat_files_list <- c(surprsie_proc_dat_files_list, temp_list)
  }
}

# Create an empty dataframe to store the results
rt_df <- data.frame(participant_id = character(),
                    num_fast_rts = numeric(),
                    stringsAsFactors = FALSE)

# Loop through each csv file
for (file in surprsie_proc_dat_files_list) {
  # Read the csv file
  temp_file <- paste(surprsie_proc_dat_files,file, sep = "/", collapse = NULL)
  tempDat <- read.csv(file = temp_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))

  # Get the participant ID
  participant_id <- tempDat$participant_id[1]

  # Count the number of RTs smaller than 0.20 seconds
  num_fast_rts <- sum(tempDat$memory_surp_rt < 0.20, na.rm = TRUE)

  # Add the results to the dataframe
  rt_df <- rbind(rt_df, data.frame(participant_id = participant_id,
                                   num_fast_rts = num_fast_rts))
}

# Adding the object data
#
proje_wd <- "/GitHub_Repos/mfe-c-dataset"
surprsie_proc_dat_files <- paste(proje_wd, "derivatives", "psychopy", "csv_output", "mfe-c-object", sep ="/", collapse = NULL)

## creating a list of all processed flanker data csv files in the input folder.
surprsie_proc_dat_files_list <- c() # an empty list that will be filled in the next "for" loop!
csvSelect <- list.files(surprsie_proc_dat_files, pattern = ".csv") # listing only csv files
for (i in 1:length(csvSelect)){
  temp_for_file <- ifelse (str_detect(csvSelect[i], "mfe_c_object_surpriseDat", negate = FALSE), 1, 0)
  if (temp_for_file == 1){
    temp_list <- csvSelect[i]
    surprsie_proc_dat_files_list <- c(surprsie_proc_dat_files_list, temp_list)
  }
}

# Loop through each csv file
for (file in surprsie_proc_dat_files_list) {
  # Read the csv file
  temp_file <- paste(surprsie_proc_dat_files,file, sep = "/", collapse = NULL)
  tempDat <- read.csv(file = temp_file, stringsAsFactors = FALSE, na.strings=c("", "NA"))

  # Get the participant ID
  participant_id <- tempDat$participant_id[1]

  # Count the number of RTs smaller than 0.20 seconds
  num_fast_rts <- sum(tempDat$memory_surp_rt < 0.20, na.rm = TRUE)

  # Add the results to the dataframe
  rt_df <- rbind(rt_df, data.frame(participant_id = participant_id,
                                   num_fast_rts = num_fast_rts))
}

round(mean(rt_df$num_fast_rts), 2)
round(sd(rt_df$num_fast_rts), 2)

########################################################################################################################

############################################ COMPUTE RACE FOR ALL PARTICIPANTS Before exclusion #########################
##### Computing racial/ethnic composition from RedCap demographic data

### Loading RedCap questionnaire data for mfe-c-face
redcapDat_face <- read.csv(file = "/Github_Repos/mfe-c-face-dataset/derivatives/redcap/Memoryforerrorcface_SCRD_2024-06-13_1453.csv")

### Loading RedCap questionnaire data for mfe-c-object
redcapDat_object <- read.csv(file = "/Github_Repos/mfe-c-object-dataset/derivatives/redcap/202402v0memoryforerr_SCRD_2024-09-10_1142.csv")

# Define ethnicity columns
ethnicity_cols <- c(
  "demo_c_ethnic_s1_r1_e1___1",  # American Indian or Alaska Native
  "demo_c_ethnic_s1_r1_e1___2",  # Asian
  "demo_c_ethnic_s1_r1_e1___3",  # Black or African American
  "demo_c_ethnic_s1_r1_e1___4",  # Hispanic, Latino/a/x or Spanish Origin
  "demo_c_ethnic_s1_r1_e1___5",  # Middle Eastern or North African
  "demo_c_ethnic_s1_r1_e1___6",  # Native Hawaiian or Other Pacific Islander
  "demo_c_ethnic_s1_r1_e1___7",  # White
  "demo_c_ethnic_s1_r1_e1___8",  # (Not specified)
  "demo_c_ethnic_s1_r1_e1___9"   # Do not want to disclose
)

# Keep only record_id and ethnicity columns
redcapDat_face_ethnicity <- redcapDat_face %>%
  dplyr::select(all_of(c("record_id", ethnicity_cols)))

redcapDat_object_ethnicity <- redcapDat_object %>%
  dplyr::select(all_of(c("record_id", ethnicity_cols)))

# Combine face and object datasets
redcapDat <- rbind(redcapDat_face_ethnicity, redcapDat_object_ethnicity)


# Remove participants that did not participate although started the redcap
redcapDat_filtered <- redcapDat[redcapDat$record_id != "260009", ] # this participant had not completed the experiment. So, they had empty surveys.
redcapDat_filtered <- redcapDat_filtered[redcapDat_filtered$record_id != "260060", ] # this participant had not completed the experiment. So, they had empty surveys.
redcapDat_filtered <- redcapDat_filtered[redcapDat_filtered$record_id != "260073", ] # This participant was collected after reaching 140 sample size. So, we are not analyzing its data.
redcapDat <- redcapDat_filtered

# Create ethnicity labels
ethnicity_labels <- c(
  "American Indian or Alaska Native",
  "Asian",
  "Black or African American",
  "Hispanic, Latino/a/x or Spanish Origin",
  "Middle Eastern or North African",
  "Native Hawaiian or Other Pacific Islander",
  "White",
  "Other",
  "Prefer not to disclose"
)

# Create a mapping for shorter labels
ethnicity_short <- c(
  "Am. Indian/Alaska Native",
  "Asian",
  "Black/African American",
  "Hispanic/Latino/a/x",
  "Middle Eastern/N. African",
  "Native Hawaiian/Pac. Islander",
  "White",
  "Other",
  "Not disclosed"
)

# Function to get ethnicities for a participant
get_ethnicities <- function(row) {
  selected <- which(row[ethnicity_cols] == 1)
  if (length(selected) == 0) {
    return("None selected")
  }
  return(paste(ethnicity_short[selected], collapse = "; "))
}

# Apply function to create ethnicity string for each participant
redcapDat$ethnicity_identity <- apply(redcapDat, 1, get_ethnicities)

# Count number of ethnicities selected per participant
redcapDat$num_ethnicities <- rowSums(redcapDat[, ethnicity_cols] == 1, na.rm = TRUE)

# Create individual-level summary
individual_summary <- redcapDat %>%
  dplyr::select(record_id, ethnicity_identity, num_ethnicities) %>%
  dplyr::arrange(record_id)

# Count endorsements for each ethnicity category
ethnicity_counts <- data.frame(
  Ethnicity = ethnicity_labels,
  Count = sapply(ethnicity_cols, function(col) sum(redcapDat[[col]] == 1, na.rm = TRUE)),
  Percentage = sapply(ethnicity_cols, function(col)
    round(sum(redcapDat[[col]] == 1, na.rm = TRUE) / nrow(redcapDat) * 100, 2))
)
rownames(ethnicity_counts) <- NULL

# Categorize participants by number of ethnicities selected
ethnicity_category_counts <- table(redcapDat$num_ethnicities)
ethnicity_category_df <- data.frame(
  Number_of_Ethnicities = names(ethnicity_category_counts),
  Count = as.numeric(ethnicity_category_counts),
  Percentage = round(as.numeric(ethnicity_category_counts) / nrow(redcapDat) * 100, 2)
)

# Get unique ethnicity combinations
ethnicity_combinations <- redcapDat %>%
  dplyr::group_by(ethnicity_identity) %>%
  dplyr::summarise(
    Count = n(),
    Percentage = round(n() / nrow(redcapDat) * 100, 2)
  ) %>%
  dplyr::arrange(desc(Count))

# Print results
cat(paste0("=", strrep("=", 79)), "\n")
cat("INDIVIDUAL PARTICIPANT ETHNICITY IDENTITIES\n")
cat(paste0("=", strrep("=", 79)), "\n\n")
print(individual_summary, row.names = FALSE)

cat("\n\n")
cat(paste0("=", strrep("=", 79)), "\n")
cat("ETHNICITY CATEGORY COUNTS (Categories are not mutually exclusive)\n")
cat(paste0("=", strrep("=", 79)), "\n\n")
print(ethnicity_counts, row.names = FALSE)

cat("\n\n")
cat(paste0("=", strrep("=", 79)), "\n")
cat("NUMBER OF ETHNICITIES SELECTED PER PARTICIPANT\n")
cat(paste0("=", strrep("=", 79)), "\n\n")
print(ethnicity_category_df, row.names = FALSE)
cat("\nTotal participants:", nrow(redcapDat), "\n")
cat("Monoracial participants:", sum(redcapDat$num_ethnicities == 1),
    paste0("(", round(sum(redcapDat$num_ethnicities == 1) / nrow(redcapDat) * 100, 2), "%)"), "\n")
cat("Multiracial participants:", sum(redcapDat$num_ethnicities > 1),
    paste0("(", round(sum(redcapDat$num_ethnicities > 1) / nrow(redcapDat) * 100, 2), "%)"), "\n")

cat("\n\n")
cat(paste0("=", strrep("=", 79)), "\n")
cat("UNIQUE ETHNICITY COMBINATIONS\n")
cat(paste0("=", strrep("=", 79)), "\n\n")
print(ethnicity_combinations, row.names = FALSE)

# Optional: Export individual-level data to CSV
# write.csv(individual_summary, "participant_ethnicity_identities.csv", row.names = FALSE)

############################################ END OF COMPUTING Race FOR ALL PARTICIPANTS #################################


############################################ COMPUTE Education level FOR ALL PARTICIPANTS Before exclusion #########################

##### Computing education level distribution from RedCap demographic data

### Loading RedCap questionnaire data for mfe-c-face
redcapDat_face <- read.csv(file = "/Github_Repos/mfe-c-face-dataset/derivatives/redcap/Memoryforerrorcface_SCRD_2024-06-13_1453.csv")

### Loading RedCap questionnaire data for mfe-c-object
redcapDat_object <- read.csv(file = "/Github_Repos/mfe-c-object-dataset/derivatives/redcap/202402v0memoryforerr_SCRD_2024-09-10_1142.csv")

# Define education columns
education_cols <- c(
  "demo_c_edu_s1_r1_e1___1",   # Some high school
  "demo_c_edu_s1_r1_e1___2",   # High school diploma or equivalent
  "demo_c_edu_s1_r1_e1___3",   # Vocational training
  "demo_c_edu_s1_r1_e1___4",   # Some college
  "demo_c_edu_s1_r1_e1___5",   # Associate's degree
  "demo_c_edu_s1_r1_e1___6",   # Bachelor's degree
  "demo_c_edu_s1_r1_e1___7",   # Some post undergraduate work
  "demo_c_edu_s1_r1_e1___8",   # Master's degree
  "demo_c_edu_s1_r1_e1___9",   # Specialist degree
  "demo_c_edu_s1_r1_e1___10",  # Applied or professional doctorate
  "demo_c_edu_s1_r1_e1___11",  # Doctorate degree
  "demo_c_edu_s1_r1_e1___12"   # Other
)

# Keep only record_id and education columns
redcapDat_face_education <- redcapDat_face %>%
  dplyr::select(all_of(c("record_id", education_cols)))

redcapDat_object_education <- redcapDat_object %>%
  dplyr::select(all_of(c("record_id", education_cols)))

# Combine face and object datasets
redcapDat <- rbind(redcapDat_face_education, redcapDat_object_education)

# Remove participants that did not participate although started the redcap
redcapDat_filtered <- redcapDat[redcapDat$record_id != "260009", ]
redcapDat_filtered <- redcapDat_filtered[redcapDat_filtered$record_id != "260060", ]
redcapDat_filtered <- redcapDat_filtered[redcapDat_filtered$record_id != "260073", ]
redcapDat <- redcapDat_filtered

# Create education labels
education_labels <- c(
  "Some high school",
  "High school diploma or equivalent",
  "Vocational training",
  "Some college",
  "Associate's degree (e.g., AA, AE, AFA, AS, ASN)",
  "Bachelor's degree (e.g., BA, BBA BFA, BS)",
  "Some post undergraduate work",
  "Master's degree (e.g., MA, MBA, MFA, MS, MSW)",
  "Specialist degree (e.g., EdS)",
  "Applied or professional doctorate degree (e.g., MD, DDC, DDS, JD, PharmD)",
  "Doctorate degree (e.g., EdD, PhD)",
  "Other"
)

# Create shorter labels for display
education_short <- c(
  "Some high school",
  "High school diploma",
  "Vocational training",
  "Some college",
  "Associate's degree",
  "Bachelor's degree",
  "Some post-undergrad work",
  "Master's degree",
  "Specialist degree",
  "Professional doctorate",
  "Doctorate degree",
  "Other"
)

# Function to get education level for a participant
get_education <- function(row) {
  selected <- which(row[education_cols] == 1)
  if (length(selected) == 0) {
    return("None selected")
  }
  return(paste(education_short[selected], collapse = "; "))
}

# Apply function to create education string for each participant
redcapDat$education_level <- apply(redcapDat, 1, get_education)

# Count number of education levels selected per participant
redcapDat$num_education_levels <- rowSums(redcapDat[, education_cols] == 1, na.rm = TRUE)

# Create individual-level summary
individual_summary <- redcapDat %>%
  dplyr::select(record_id, education_level, num_education_levels) %>%
  dplyr::arrange(record_id)

# Count endorsements for each education category
education_counts <- data.frame(
  Education_Level = education_labels,
  Count = sapply(education_cols, function(col) sum(redcapDat[[col]] == 1, na.rm = TRUE)),
  Percentage = sapply(education_cols, function(col)
    round(sum(redcapDat[[col]] == 1, na.rm = TRUE) / nrow(redcapDat) * 100, 2))
)
rownames(education_counts) <- NULL

# Categorize participants by number of education levels selected
education_category_counts <- table(redcapDat$num_education_levels)
education_category_df <- data.frame(
  Number_of_Levels = names(education_category_counts),
  Count = as.numeric(education_category_counts),
  Percentage = round(as.numeric(education_category_counts) / nrow(redcapDat) * 100, 2)
)

# Get unique education combinations
education_combinations <- redcapDat %>%
  dplyr::group_by(education_level) %>%
  dplyr::summarise(
    Count = n(),
    Percentage = round(n() / nrow(redcapDat) * 100, 2)
  ) %>%
  dplyr::arrange(desc(Count))

# Print results
cat(paste0("=", strrep("=", 79)), "\n")
cat("INDIVIDUAL PARTICIPANT EDUCATION LEVELS\n")
cat(paste0("=", strrep("=", 79)), "\n\n")
print(individual_summary, row.names = FALSE)

cat("\n\n")
cat(paste0("=", strrep("=", 79)), "\n")
cat("EDUCATION LEVEL COUNTS\n")
cat(paste0("=", strrep("=", 79)), "\n\n")
print(education_counts, row.names = FALSE)

cat("\n\n")
cat(paste0("=", strrep("=", 79)), "\n")
cat("NUMBER OF EDUCATION LEVELS SELECTED PER PARTICIPANT\n")
cat(paste0("=", strrep("=", 79)), "\n\n")
print(education_category_df, row.names = FALSE)
cat("\nTotal participants:", nrow(redcapDat), "\n")
cat("Single education level:", sum(redcapDat$num_education_levels == 1),
    paste0("(", round(sum(redcapDat$num_education_levels == 1) / nrow(redcapDat) * 100, 2), "%)"), "\n")
cat("Multiple education levels:", sum(redcapDat$num_education_levels > 1),
    paste0("(", round(sum(redcapDat$num_education_levels > 1) / nrow(redcapDat) * 100, 2), "%)"), "\n")

cat("\n\n")
cat(paste0("=", strrep("=", 79)), "\n")
cat("UNIQUE EDUCATION COMBINATIONS\n")
cat(paste0("=", strrep("=", 79)), "\n\n")
print(education_combinations, row.names = FALSE)

# Optional: Export individual-level data to CSV
# write.csv(individual_summary, "participant_education_levels.csv", row.names = FALSE)

############################################ END OF COMPUTING Education level FOR ALL PARTICIPANTS #################################
