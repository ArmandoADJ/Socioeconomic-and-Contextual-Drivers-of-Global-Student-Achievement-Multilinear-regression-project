#-------------------------------------------------------------------------------
#Import libraries
#-------------------------------------------------------------------------------
library(haven)
library(psych)
library(ggplot2)
library(vtable)
library(skimr)
library(tidyverse)
library(MASS)
library(car)
library(olsrr)
library(lm.beta)
library(patchwork)
#-------------------------------------------------------------------------------
#Import dataset
#-------------------------------------------------------------------------------
path <- "C:\\Users\\Arman\\OneDrive\\Documentos\\R Class\\Project 7130\\Data\\Cleaned_Data.SAV"
data <- read_sav(path)
set.seed(42)

#-------------------------------------------------------------------------------
#Compute average plausible values PV1-PV10 for math, science and reading
#-------------------------------------------------------------------------------
data["avg_pvs_math"] <- rowSums(data[c("PV1MATH", "PV2MATH", "PV3MATH", "PV4MATH",
"PV5MATH", "PV6MATH", "PV7MATH", "PV8MATH", "PV9MATH", "PV10MATH")])/ 10

data["avg_pvs_scie"] <- rowSums(data[c("PV1SCIE", 'PV2SCIE', 'PV3SCIE', 'PV4SCIE', 
'PV5SCIE', 'PV6SCIE', 'PV7SCIE', 'PV8SCIE','PV9SCIE', 'PV10SCIE')])/10

data["avg_pvs_read"]<- rowSums(data[c('PV1READ', 'PV2READ', 'PV3READ', 'PV4READ',
'PV5READ', 'PV6READ', 'PV7READ', 'PV8READ', 'PV9READ', 'PV10READ')])/10

#-------------------------------------------------------------------------------
# Create a new Variable that will not contain the separate PV's
#-------------------------------------------------------------------------------
data1 <- data[c("CNT","CNTSCHID", "ESCS","ST004D01T", "AGE", "REPEAT", "ST255Q01JA",
"STUDYHMW", "TARDYSD", "STRATIO", "SC061Q07TA", "OECD.x","LANGN", "IMMIG", 
"ST322Q02JA","ICTRES", "CONTI", "avg_pvs_math", "avg_pvs_scie", "avg_pvs_read")]

#-------------------------------------------------------------------------------
#Compute descriptive statistic for all the variables
#-------------------------------------------------------------------------------
summary(data1)

#-------------------------------------------------------------------------------
#Remove variables with high Nans and re-compute descriptive statistics for all variables
#-------------------------------------------------------------------------------
data1 <- subset(data1, select= -c(ICTRES, ST322Q02JA, STRATIO, CNTSCHID))
data1 <- na.omit(data1)
summary(data1)


my_skim<- skim_with(numeric = sfl(mean, median, sd, var, min, max), 
  append = FALSE)

my_skim(data1)

#-------------------------------------------------------------------------------

###############################################################################
################### Create Plots for all Variables ############################
###############################################################################

#-------------------------------------------------------------------------------
#Histograms for dependent Variables
#-------------------------------------------------------------------------------
dependentvar <- subset(data1, select =
                         c(avg_pvs_math, avg_pvs_scie, avg_pvs_read))

plotvars <- names(dependentvar)

for (i in 1:length(plotvars)){
  histogram <- ggplot(data = dependentvar, aes(x= get(plotvars[i]))) + 
    geom_histogram(fill = "#999999", color = "black") +
    labs(title = paste("Histogram of", plotvars[i]), x = plotvars[i]) + 
    theme_minimal()
    print(histogram)
}

#-------------------------------------------------------------------------------
#Boxplot with all Dependent Variables
#-------------------------------------------------------------------------------
reshaped <- dependentvar %>%
  pivot_longer(cols = c(avg_pvs_math, avg_pvs_scie, avg_pvs_read), 
  names_to = "Variable", values_to = "Value")

ggplot(data = reshaped, aes(x = Variable, y = Value, fill = Variable)) + 
  geom_boxplot() + 
  labs(title = "Boxplot of Avg plausible Values (Math, Science, Read) ") + 
  theme_minimal() + scale_fill_grey(start = 0.5, end = 0.9 )

#-------------------------------------------------------------------------------
#Bar Charts for Categorical variables
#-------------------------------------------------------------------------------
categoricalvar <- subset(data1, select =
  c(ESCS, ST004D01T, AGE, REPEAT, ST255Q01JA, STUDYHMW, TARDYSD, SC061Q07TA,
    OECD.x, LANGN, IMMIG, CONTI))

plotbarvars <- names(categoricalvar)

for(i in 1:length(plotbarvars)){
  barsplot <- ggplot(data = categoricalvar, aes(x= get(plotbarvars[i]))) +
    geom_bar(fill = "#999999", color = "black") + labs(
      title = paste("Barplot of", plotbarvars[i]), 
      x = plotbarvars[i]) + theme_minimal()
  print(barsplot)
    
}

#-------------------------------------------------------------------------------
#Subset to 5000 samples to assess correlation#
#-------------------------------------------------------------------------------
sample_rows <- sample(nrow(data1), 5000)

sampled_data <- data1[sample_rows,]

#pairs.panels(sampled_data, main = "Panel plot of all Variables", smooth = F, ellipses = F)

divided_variables <- subset(sampled_data, select =  c(CNT, ESCS, ST004D01T, 
  AGE, REPEAT, ST255Q01JA, STUDYHMW, avg_pvs_math, avg_pvs_scie, avg_pvs_read))

divided_variables1 <- subset(sampled_data, select = c(TARDYSD, SC061Q07TA, 
  OECD.x,LANGN, IMMIG, CONTI, avg_pvs_math, avg_pvs_scie, 
  avg_pvs_read))

pairs.panels(divided_variables, main = "Panel plot of all Variables", 
             smooth = F, ellipses = F)

pairs.panels(divided_variables1, main = "Panel plot of all Variables", 
             smooth = F, ellipses = F)

#-------------------------------------------------------------------------------
#Run The Model
#-------------------------------------------------------------------------------
#Math Model
options(scipen = 3)
lm_model_math <- lm(data = data1, avg_pvs_math ~ ESCS + ST004D01T + AGE + REPEAT + 
                 ST255Q01JA + STUDYHMW  +TARDYSD + SC061Q07TA + OECD.x + LANGN +
                 IMMIG + CONTI)

summary(lm_model_math)

#Science Model
lm_model_science <- lm(data = data1, avg_pvs_scie ~ ESCS + ST004D01T + AGE + REPEAT + 
                 ST255Q01JA + STUDYHMW  +TARDYSD + SC061Q07TA + OECD.x + LANGN +
                 IMMIG + CONTI)

summary(lm_model_science)

#Reading model
lm_model_reading <- lm(data = data1, avg_pvs_read ~ ESCS + ST004D01T + AGE + REPEAT + 
                 ST255Q01JA + STUDYHMW  +TARDYSD + SC061Q07TA + OECD.x + LANGN +
                 IMMIG + CONTI)

summary(lm_model_reading)

#-------------------------------------------------------------------------------
################################################################################
############################Check For Assumptions###############################
################################################################################
#-------------------------------------------------------------------------------
                     #Math Model - Get Residuals
dataframe_resids_fitted <- model.frame(lm_model_math)
dataframe_resids_fitted$fitted_values_math <- fitted(lm_model_math)
dataframe_resids_fitted$residuals_math <- rstudent(lm_model_math)


#Sample residuals for visualizations
sample_rows_math <- sample(nrow(dataframe_resids_fitted),5000)
sampled_resid_math <-dataframe_resids_fitted[sample_rows_math,]

#Plot Math Model 
ggplot(data = sampled_resid_math, aes( x= fitted_values_math, y = residuals_math)) + 
  geom_point(alpha = 0.3, color = "blue") + geom_hline(yintercept = 0, 
    linetype = "solid", color = "red") + labs(
  title = "Residuals vs fitted Values Math model", 
  x = "Fitted Values", 
  y = "Residuals"
) + theme_minimal()

ggplot(data = dataframe_resids_fitted, aes(x = residuals_math)) +
  geom_histogram(fill = "#999999", color = "black") + labs(
    title = "Histogram of Residuals Math model",
    x  = "Residuals math model"
  ) + theme_minimal()

ggplot(data = dataframe_resids_fitted, aes( y = residuals_math)) + 
  geom_boxplot(fill = "#999999", color = "black") +
  labs(title = "Boxplot of Residuals Math Model",
       y  ="Residuals"
         ) + theme_minimal()

ggplot(data = dataframe_resids_fitted,aes(sample= residuals_math )) +
  stat_qq() + stat_qq_line(color="red") + labs(
    title = "Normal Q-Q Plot for Residuals Math Model") + theme_minimal()

#Skewness and Kurtosis
summary_Math <- describe(dataframe_resids_fitted$residuals_math)
print(summary_Math)

#Test of normality Shapiro Wilk's test
Shapiro_wilk_math <- shapiro.test(sampled_resid_math$residuals_math)
print(Shapiro_wilk_math)

#Compute VIF for math model
vif_math<- vif(lm_model_math)
print(vif_math)

#Compute Outlier 
outlier_indices <- which(abs(dataframe_resids_fitted$residuals_math) > 3)
potential_outliers <- dataframe_resids_fitted$residuals_math[outlier_indices]
print(potential_outliers)

#Cook's D outliers

cooksd_outliers <- cooks.distance(lm_model_math)
n <- length(cooksd_outliers)
threshold<- 4/ n
influential_points <- which(cooksd_outliers > threshold)
plot(lm_model_math, which = 4)
abline(h= threshold, col = "red", lty = 2)

#Dfbetas Outliers
dfbetas_outliers <- dfbeta(lm_model_math)
n <- length(dfbetas_outliers)
threshold <- 2/sqrt(n)
dfbetas_influential_points <- which(dfbetas_outliers > threshold)
ols_plot_dfbetas(lm_model_math)

#Dffits outliers
dffits_outliers <- dffits(lm_model_math)
ols_plot_dffits(lm_model_math)
print(which(abs(dffits_outliers) > 0.04))

#Remove outliers

data2 <- data1

data2 <- data2[-c(273956, 358831, 495031, 488246),]

new_lm_model_math <- lm(data =data2, avg_pvs_math ~ ESCS + ST004D01T + AGE + REPEAT + 
                          ST255Q01JA + STUDYHMW  +TARDYSD + SC061Q07TA + OECD.x + LANGN +
                          IMMIG + CONTI)

summary(lm_model_math)
summary(new_lm_model_math)

################################################################################
                          #Science Model - Get Residuals

dataframe_resids_fitted_science <- model.frame(lm_model_science)
dataframe_resids_fitted_science$fitted_values_science <- fitted(lm_model_science)
dataframe_resids_fitted_science$residuals_science <- rstudent(lm_model_science)

#Sample residuals for visualizations
sample_rows_science <- sample(nrow(dataframe_resids_fitted_science),5000)
sampled_resid_science <-dataframe_resids_fitted_science[sample_rows_science,]

#Plot Math Model 
ggplot(data = sampled_resid_science, aes( x= fitted_values_science, 
                                          y = residuals_science)) + 
  geom_point(alpha = 0.3, color = "blue") + geom_hline(yintercept = 0, 
  linetype = "solid", color = "red") + labs(
    title = "Residuals vs fitted Values Science model", 
    x = "Fitted Values", y = "Residuals") + theme_minimal()

ggplot(data = dataframe_resids_fitted_science, aes(x = residuals_science)) +
  geom_histogram(fill = "#999999", color = "black") + labs(
    title = "Histogram of Residuals Science model",
    x  = "Residuals Science model"
  ) + theme_minimal()

ggplot(data = dataframe_resids_fitted_science, aes( y = residuals_science)) + 
  geom_boxplot(fill = "#999999", color = "black") +
  labs(title = "Boxplot of Residuals Science Model",
       y  ="Residuals"
  ) + theme_minimal()

ggplot(data = dataframe_resids_fitted_science ,aes(sample= residuals_science )) +
  stat_qq() + stat_qq_line(color="red") + labs(
    title = "Normal Q-Q Plot for Residuals Science Model") + theme_minimal()

#Skewness and Kurtosis
summary_Science <- describe(dataframe_resids_fitted_science$residuals_science)
print(summary_Science)

#Test of normality Shapiro Wilk's test
Shapiro_wilk_science <- shapiro.test(sampled_resid_science$residuals_science)
print(Shapiro_wilk_science)

#Compute VIF for math model
vif_science<- vif(lm_model_science)
print(vif_science)

#Compute Outlier 
outlier_indices_science <- which(abs(dataframe_resids_fitted_science$residuals_science) > 3)
potential_outliers_science <- dataframe_resids_fitted_science$residuals_science[outlier_indices_science]
print(potential_outliers_science)

#Cook's D outliers

cooksd_outliers_science <- cooks.distance(lm_model_science)
n <- length(cooksd_outliers_science)
threshold<- 4/ n
influential_points_science <- which(cooksd_outliers_science > threshold)
plot(lm_model_science, which = 4)

#Dfbetas Outliers
dfbetas_outliers_science <- dfbeta(lm_model_science)
n <- length(dfbetas_outliers_science)
threshold <- 2/sqrt(n)
dfbetas_influential_points_science <- which(dfbetas_outliers_science > threshold)
ols_plot_dfbetas(lm_model_science)

#Dffits outliers
dffits_outliers_science <- dffits(lm_model_science)
ols_plot_dffits(lm_model_science)
print(which(abs(dffits_outliers_science) > 0.04))
#Remove outliers

data2 <- data1

data2 <- data2[-c(302090, 383833),]

new_lm_model_science <- lm(data =data2, avg_pvs_scie ~ ESCS + ST004D01T + AGE + REPEAT + 
                          ST255Q01JA + STUDYHMW  +TARDYSD + SC061Q07TA + OECD.x + LANGN +
                          IMMIG + CONTI)

summary(lm_model_science)
summary(new_lm_model_science)

################################################################################
                     #Reading Model - Get Residuals

dataframe_resids_fitted_reading  <- model.frame(lm_model_reading)
dataframe_resids_fitted_reading $fitted_values_reading <- fitted(lm_model_reading)
dataframe_resids_fitted_reading $residuals_reading <- rstudent(lm_model_reading)

#Sample residuals for visualizations
sample_rows_reading <- sample(nrow(dataframe_resids_fitted_reading ),5000)
sampled_resid_reading <-dataframe_resids_fitted_reading [sample_rows_reading,]

#Plot reading Model 
ggplot(data = sampled_resid_reading, aes( x= fitted_values_reading, 
                                          y = residuals_reading)) + 
  geom_point(alpha = 0.3, color = "blue") + 
  geom_hline(yintercept = 0, linetype = "solid", color = "red") + labs(
  title = "Residuals vs fitted Values Reading model", 
  x = "Fitted Values", y = "Residuals") + theme_minimal()

ggplot(data = dataframe_resids_fitted_reading, aes(x = residuals_reading)) +
  geom_histogram(fill = "#999999", color = "black") + labs(
    title = "Histogram of Residuals Reading model",
    x  = "Residuals Reading model"
  ) + theme_minimal()

ggplot(data = dataframe_resids_fitted_reading, aes( y = residuals_reading)) + 
  geom_boxplot(fill = "#999999", color = "black") +
  labs(title = "Boxplot of Residuals Reading Model",
       y  ="Residuals"
  ) + theme_minimal()

ggplot(data = dataframe_resids_fitted_reading ,aes(sample= residuals_reading )) +
  stat_qq() + stat_qq_line(color="red") + labs(
    title = "Normal Q-Q Plot for Residuals Reading Model") + theme_minimal()

#Skewness and Kurtosis
summary_Reading <- describe(dataframe_resids_fitted_reading$residuals_reading)
print(summary_Reading)

#Test of normality Shapiro Wilk's test
Shapiro_wilk_reading <- shapiro.test(sampled_resid_reading$residuals_reading)
print(Shapiro_wilk_reading)

#Compute VIF for reading model
vif_reading<- vif(lm_model_reading)
print(vif_reading)

#Compute Outlier 
outlier_indices_reading <- which(abs(dataframe_resids_fitted_reading$residuals_reading) > 3)
potential_outliers_reading <- dataframe_resids_fitted_reading$residuals_reading[outlier_indices_reading]
print(potential_outliers_reading)

#Cook's D outliers

cooksd_outliers_reading <- cooks.distance(lm_model_reading)
n <- length(cooksd_outliers_reading)
threshold<- 4/ n
influential_points_reading <- which(cooksd_outliers_reading > threshold)
plot(lm_model_reading, which = 4)

#Dfbetas Outliers
dfbetas_outliers_reading <- dfbeta(lm_model_reading)
n <- length(dfbetas_outliers_reading)
threshold <- 2/sqrt(n)
dfbetas_influential_points_reading <- which(dfbetas_outliers_reading > threshold)
ols_plot_dfbetas(lm_model_reading)

#Dffits outliers
dffits_outliers_reading <- dffits(lm_model_reading)
ols_plot_dffits(lm_model_reading)
print(which(abs(dffits_outliers_reading) > 0.04))
#Remove outliers

data2 <- data1

data2 <- data2[-c(),]

new_lm_model_reading <- lm(data =data2, avg_pvs_scie ~ ESCS + ST004D01T + AGE + REPEAT + 
                             ST255Q01JA + STUDYHMW  +TARDYSD + SC061Q07TA + OECD.x + LANGN +
                             IMMIG + CONTI)

summary(lm_model_science)
summary(new_lm_model_science)


#-------------------------------------------------------------------------------
#Parsimonious model
#------------------------------------------------------------------------------
parsimounious_model_math <- step(lm_model_math, direction = "backward")

summary(parsimounious_model_math)

parsimounious_model_science <- step(lm_model_science, direction = "backward")

summary(parsimounious_model_science)

parsimounious_model_reading <- step(lm_model_science, direction = "backward")

summary(parsimounious_model_reading)

#-------------------------------------------------------------------------------
#Extra Visuals for Report
#-------------------------------------------------------------------------------
#Obtain the standardized betas math model
sb_math <- lm.beta(lm_model_math)$standardized.coefficients

#Retrieve Names and beta values
betas_df <- data.frame(
  term = names(sb_math),
  beta = as.numeric(sb_math),
  row.names = NULL
)

#Remove the intercept from the plot
betas_df <- subset(betas_df, term != "(Intercept)")
betas_df <- na.omit(betas_df)

#Bar plot for math model
ggplot(betas_df, aes(x = reorder(term, beta), y = beta, fill = beta > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.9, guide = "none") +theme_minimal() +
  labs(
    title = "Standardized Coefficients Math model",
    x = "Predictor",
    y = "Standardized Beta"
  )

#Obtain the standardized betas science model
sb_scie<- lm.beta(lm_model_science)$standardized.coefficients

#Retrieve Names and beta values
betas_df <- data.frame(
  term = names(sb_scie),
  beta = as.numeric(sb_scie),
  row.names = NULL
)

#Remove the intercept from the plot
betas_df <- subset(betas_df, term != "(Intercept)")
betas_df <- na.omit(betas_df)

#Bar plot for science model
ggplot(betas_df, aes(x = reorder(term, beta), y = beta, fill = beta > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.9, guide = "none") +theme_minimal() +
  labs(
    title = "Standardized Coefficients Science model",
    x = "Predictor",
    y = "Standardized Beta"
  )

#Obtain the standardized betas reading model
sb_read<- lm.beta(lm_model_reading)$standardized.coefficients

#Retrieve Names and beta values
betas_df <- data.frame(
  term = names(sb_read),
  beta = as.numeric(sb_read),
  row.names = NULL
)

#Remove the intercept from the plot
betas_df <- subset(betas_df, term != "(Intercept)")
betas_df <- na.omit(betas_df)

#Bar plot for reading model
ggplot(betas_df, aes(x = reorder(term, beta), y = beta, fill = beta > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_grey(start = 0.5, end = 0.9, guide = "none") +theme_minimal() +
  labs(
    title = "Standardized Coefficients Reading model",
    x = "Predictor",
    y = "Standardized Beta"
  )

#Histogram plot of RStudentized residuals across models
plot1 <- ggplot(data = dataframe_resids_fitted, aes(x = residuals_math)) +
  geom_histogram(fill = "#7e7e7e", color = "black") + labs(x  = "Math model"
  ) + theme_minimal()

plot2 <-  ggplot(data = dataframe_resids_fitted_science, aes(x = residuals_science)) +
  geom_histogram(fill = "#4f4f4f", color = "black") + labs(x  = "Science model"
  ) + theme_minimal()

plot3 <-  ggplot(data = dataframe_resids_fitted_reading, aes(x = residuals_reading)) +
  geom_histogram(fill = "#2f2f2f", color = "black") + labs(x  = "Reading model"
  ) + theme_minimal()

combined_plot <- plot1 + plot2 + plot3 + plot_layout(ncol= 3) + 
  plot_annotation(title = "Histogram of R Studentized residuals across Models")

combined_plot

library(haven)
library(ggplot2)

data3 <- data1
data3$avg_pvs <- rowSums(data3[c("avg_pvs_math","avg_pvs_read","avg_pvs_scie")], na.rm = TRUE) / 3

# convert haven_labelled -> factor with clear labels
data3$OECD <- factor(
  as.integer(haven::zap_labels(data3$OECD.x)),
  levels = c(0, 1),
  labels = c("Non-OECD", "OECD")
)

ggplot(data3, aes(x = OECD, y = avg_pvs, fill = OECD)) +
  geom_bar(stat = "summary", fun = mean, width = 0.6) +
  labs(
    title = "Average PISA Performance by OECD Membership",
    x = NULL, y = "Average of Math, Reading, Science"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") + scale_fill_grey(start = 0.4, end = 0.6)

sample_rows <- sample(nrow(data3), 5000)

sampled_data <- data3[sample_rows,]

ggplot(data = sampled_data, aes(x = ESCS, y = avg_pvs)) + geom_point() + 
  geom_smooth(method = "lm", color = "#FDBB30") + theme_minimal() + labs(
    title = "Scatterplot ESCS vs avg_pvs", x = "ESCS", y = "avg_pvs"
  )

Route_data <- "C:/Users/Arman/OneDrive/Documentos/R Class/Project 7130/Data/Project_data.csv"
write_csv(data1,Route_data)