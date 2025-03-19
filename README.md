# Data-Codes-Efficiency-Innovation-Bundles-Cambodian-Rice
This repository contains R scripts and data for analyzing m-DSR adoption in Cambodian rice farming using bootstrapped Data Envelopment Analysis (DEA), machine learning (ML), and statistical methods. It evaluates Innovation Bundles (IBs) based on land preparation and harvesting methods, ranking rice varieties by efficiency. 
#Script written and analyzed in R 4.3.1 and R Studio RStudio 2024.12.1+563 
# in Microsoft Windows 10 Pro

##****************************************************************************
## I. Frontier efficiency analysis of Cambodian rice data using bootstrapped DEA
##****************************************************************************

## Step 1: Set Working Directory
setwd("C:/")
getwd()

## Step 2: Load Necessary Libraries
library(Benchmarking)
library(dplyr)
library(officer)
library(flextable)
library(ggplot2)
library(gridExtra)

## Step 3: Load Input-Output Data for Rice Production from Cambodian Farm Households
IO_dat <- read.csv("IO_data_RA.csv")

## Step 4: Check Data Structure
head(IO_dat)
names(IO_dat)

## Step 5: Subset Data for Rice Year 2019-2020 and by Crop Season (FC and SC)
# FC = First Crop, SC = Second Crop
IO_data1 <- subset(IO_dat, RY == "2019-2020")
IO_data1_FC <- subset(IO_data1, CS == "FC")
IO_data1_SC <- subset(IO_data1, CS == "SC")

## Step 6: Define Input and Output Variables for DEA Analysis
input_columns <- c("RA", "SR", "FERTC", "HERBC", "PESTC", "RODC", "LPC", "CEC", 
                   "IFC", "FALC", "PALC", "HALC", "HARVC")
output_column <- "GSI"

## Step 7: Function to Clean Numeric Columns and Handle Missing Values
clean_numeric <- function(x) {
  x <- as.numeric(as.character(x))  # Convert to numeric
  x[!is.finite(x)] <- NA  # Replace Inf, -Inf, NaN with NA
  return(x)
}

## Step 8: Convert Inputs & Outputs to Numeric and Handle Missing Values
IO_data1_FC <- IO_data1_FC %>%
  mutate(across(all_of(input_columns), clean_numeric)) %>%
  mutate(!!output_column := clean_numeric(.data[[output_column]]))

IO_data1_SC <- IO_data1_SC %>%
  mutate(across(all_of(input_columns), clean_numeric)) %>%
  mutate(!!output_column := clean_numeric(.data[[output_column]]))

## Step 9: Remove Rows with Missing Values Before Conversion to Matrices
IO_data1_FC <- IO_data1_FC %>% filter(complete.cases(across(all_of(c(input_columns, output_column)))))
IO_data1_SC <- IO_data1_SC %>% filter(complete.cases(across(all_of(c(input_columns, output_column)))))

## Step 10: Create Input and Output Matrices
input_matrix_FC <- as.matrix(IO_data1_FC[input_columns])
output_matrix_FC <- as.matrix(IO_data1_FC[[output_column]])
input_matrix_SC <- as.matrix(IO_data1_SC[input_columns])
output_matrix_SC <- as.matrix(IO_data1_SC[[output_column]])

## Step 11: Perform DEA with Bootstrapping (if valid rows exist)
n_boot <- 10000  # Number of bootstrap replications

if (nrow(input_matrix_FC) > 1 && all(complete.cases(input_matrix_FC, output_matrix_FC))) {
  boot_result_FC <- dea.boot(X = input_matrix_FC, Y = output_matrix_FC, 
                             NREP = n_boot, RTS = "vrs", ORIENTATION = "in")
  IO_data1_FC <- IO_data1_FC %>%
    mutate(efficiency_bias_corrected = boot_result_FC$eff.bc)
} else {
  stop("Error: Not enough valid data for DEA analysis in FC.")
}

if (nrow(input_matrix_SC) > 1 && all(complete.cases(input_matrix_SC, output_matrix_SC))) {
  boot_result_SC <- dea.boot(X = input_matrix_SC, Y = output_matrix_SC, 
                             NREP = n_boot, RTS = "vrs", ORIENTATION = "in")
  IO_data1_SC <- IO_data1_SC %>%
    mutate(efficiency_bias_corrected = boot_result_SC$eff.bc)
} else {
  stop("Error: Not enough valid data for DEA analysis in SC.")
}

## Step 12: Save Data with Efficiency Scores
write.csv(IO_data1_FC, "IO_data1_FC_bias_corrected.csv", row.names = FALSE)
write.csv(IO_data1_SC, "IO_data1_SC_bias_corrected.csv", row.names = FALSE)

message("DEA efficiency analysis completed successfully, & results saved.")

## Step 13: Visualization of Bias-Corrected Efficiency Scores

# Function to calculate mean efficiency scores for each group (e.g., mDSR use and IBs)
calculate_means <- function(data, group_var, efficiency_var, season) {
  data %>%
    filter(!is.na(.data[[group_var]])) %>%
    group_by(.data[[group_var]]) %>%
    summarise(mean_efficiency = mean(.data[[efficiency_var]], na.rm = TRUE), .groups = "drop") %>%
    mutate(Season = season,
           label = paste0("bar(italic(theta))['", .data[[group_var]], ", ", season, "'] == ", round(mean_efficiency, 2)))
}

# Adjust text position for better readability
text_y_position <- 4.0  

# === First Plot: Density of Efficiency Scores for USE (Adopters vs. Non-Adopters) ===
means_USE_FC <- calculate_means(IO_data1_FC, "USE", "efficiency_bias_corrected", "FC")
means_USE_SC <- calculate_means(IO_data1_SC, "USE", "efficiency_bias_corrected", "SC")
means_USE_combined <- bind_rows(means_USE_FC, means_USE_SC)

density_USE_combined <- ggplot(bind_rows(IO_data1_FC %>% mutate(Season = "FC"), 
                                         IO_data1_SC %>% mutate(Season = "SC")), 
                               aes(x = efficiency_bias_corrected, fill = as.factor(USE))) +
  geom_density(alpha = 0.5) +
  geom_vline(data = means_USE_combined, aes(xintercept = mean_efficiency, group = interaction(USE, Season)),
             linetype = "dotted", color = "black", linewidth = 1) +
  geom_text(data = means_USE_combined, aes(x = mean_efficiency, y = text_y_position, 
                                           label = label, group = interaction(USE, Season)),
            color = "black", angle = 90, vjust = -1, parse = TRUE) +  
  scale_fill_manual(values = c("No" = "blue", "Yes" = "green"), labels = c("Non-Adopters", "Adopters")) +
  facet_grid(rows = vars(Season)) +  
  theme_minimal() +
  labs(title = "Density of Efficiency Scores by Adoption Status (USE) - FC & SC",
       x = "Efficiency Scores",
       y = "Density") +
  theme(legend.position = "top", legend.title = element_blank())

# === Second Plot: Density of Efficiency Scores for IB Groups ===
means_IB_FC <- calculate_means(IO_data1_FC, "IB", "efficiency_bias_corrected", "FC")
means_IB_SC <- calculate_means(IO_data1_SC, "IB", "efficiency_bias_corrected", "SC")
means_IB_combined <- bind_rows(means_IB_FC, means_IB_SC)

density_IB_combined <- ggplot(bind_rows(IO_data1_FC %>% mutate(Season = "FC"), 
                                        IO_data1_SC %>% mutate(Season = "SC")), 
                              aes(x = efficiency_bias_corrected, fill = IB)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = means_IB_combined, aes(xintercept = mean_efficiency, group = interaction(IB, Season)),
             linetype = "dotted", color = "black", linewidth = 1) +
  geom_text(data = means_IB_combined, aes(x = mean_efficiency, y = text_y_position, 
                                          label = label, group = interaction(IB, Season)),
            color = "black", angle = 90, vjust = -1, parse = TRUE) +
  facet_grid(rows = vars(Season)) +  
  theme_minimal() +
  labs(title = "Density of Efficiency Scores by IB - FC & SC",
       x = "Efficiency Scores",
       y = "Density") +
  theme(legend.position = "top", legend.title = element_blank())

# First plot: Adopters vs. Non-Adopters (mDSR USE)
density_USE_combined  

# Second plot: IB Groups
density_IB_combined  

#**********************************************************
# II. Test of Significance of variables between IBs for FC & SC
#***********************************************************
# FC = First Crop, SC = Second Crop
# Step 1: Load necessary libraries
library(dplyr)
library(tidyr)
library(flextable)
library(officer)

# Step 2: Load the dataset
data <- read.csv("IO_data_RA.csv")

# Step 3: Separate data for FC and SC seasons
data_FC <- data %>% filter(CS == "FC")
data_SC <- data %>% filter(CS == "SC")

# Step 4: Identify numeric columns
numeric_columns <- data %>% select(where(is.numeric)) %>% colnames()

# Step 5: --- SHAPIRO-WILK NORMALITY TEST ---
# Step 5.1: Function to perform Shapiro-Wilk normality test
get_shapiro_pvalue <- function(values) {
  if (length(values) > 3) {  # Shapiro-Wilk requires at least 3 values
    return(tryCatch(shapiro.test(values)$p.value, error = function(e) NA))
  } else {
    return(NA)  # Not enough data for normality test
  }
}

# Perform normality test for each IB and variable
normality_results <- data %>%
  pivot_longer(all_of(numeric_columns), names_to = "Variable", values_to = "Value") %>%
  filter(!is.na(Value)) %>%
  group_by(IB, Variable) %>%
  summarise(
    P_Value = get_shapiro_pvalue(Value),
    Normality = case_when(
      is.na(P_Value) ~ "Insufficient Data",
      P_Value < 0.05 ~ "Non-Normal",
      TRUE ~ "Normal"
    ),
    .groups = "drop"
  ) %>%
  mutate(P_Value = round(P_Value, 3))  # Round p-values

# Reshape results for better readability
normality_table <- normality_results %>%
  pivot_wider(names_from = IB, values_from = c(P_Value, Normality), names_sep = "_") %>%
  arrange(Variable)

# Save Normality Test Results to Word
ft_norm <- flextable(normality_table) %>% autofit()
doc_norm <- read_docx() %>% body_add_flextable(ft_norm)
print(doc_norm, target = "Normality_Test_Results_IB.docx")

cat("Normality test results saved to 'Normality_Test_Results_IB.docx'\n")

# Step 6: --- WILCOXON RANK-SUM TEST FOR IB GROUPS ---
# Step 6.1: Define IB comparison pairs
comparison_pairs <- list(
  "IB1_vs_IB4" = c("IB1", "IB4"),
  "IB2_vs_IB4" = c("IB2", "IB4"),  # Fixed duplicate
  "IB3_vs_IB4" = c("IB3", "IB4")
)

# Step 6.2: Function to perform Wilcoxon's rank-sum test
perform_wilcox_test <- function(data, var, group1, group2) {
  values1 <- data %>% filter(IB == group1) %>% pull(!!sym(var))
  values2 <- data %>% filter(IB == group2) %>% pull(!!sym(var))
  
  # Remove NAs correctly using `na.omit()`
  values1 <- na.omit(values1)
  values2 <- na.omit(values2)
  
  if (length(values1) >= 3 & length(values2) >= 3) {  # Ensure enough observations
    test_p_value <- tryCatch(
      wilcox.test(values1, values2, exact = FALSE)$p.value,
      error = function(e) NA
    )
    return(case_when(
      is.na(test_p_value) ~ "",
      test_p_value < 0.001 ~ "***",
      test_p_value < 0.01 ~ "**",
      test_p_value < 0.05 ~ "*",
      TRUE ~ ""
    ))
  } else {
    return("Insufficient Data")
  }
}

# Step 6.3: Function to calculate Wilcoxon tests for all variables
wilcox_test_results <- function(data, numeric_columns) {
  results <- lapply(numeric_columns, function(var) {
    sapply(comparison_pairs, function(pair) {
      perform_wilcox_test(data, var, pair[1], pair[2])
    })
  }) %>%
    do.call(rbind, .) %>%
    as.data.frame()
  
  results <- cbind(Variable = numeric_columns, results)  # Add variable names
  colnames(results) <- c("Variable", "IB1 vs IB4", "IB2 vs IB4", "IB3 vs IB4")
  return(results)
}

# Step 6.4: Perform Wilcoxon tests for FC and SC separately
results_FC <- wilcox_test_results(data_FC, numeric_columns)
results_SC <- wilcox_test_results(data_SC, numeric_columns)

# Step 6.5: Save Wilcoxon test results to Word
ft_FC <- flextable(results_FC) %>% autofit()
doc_FC <- read_docx() %>% 
  body_add_par("Wilcoxon Test Results for FC Season", style = "heading 1") %>%
  body_add_flextable(ft_FC)
print(doc_FC, target = "Wilcoxon_Test_Results_FC.docx")

ft_SC <- flextable(results_SC) %>% autofit()
doc_SC <- read_docx() %>%
  body_add_par("Wilcoxon Test Results for SC Season", style = "heading 1") %>%
  body_add_flextable(ft_SC)
print(doc_SC, target = "Wilcoxon_Test_Results_SC.docx")

# Step 6.6: Save Wilcoxon test results as CSV
write.csv(results_FC, "Wilcoxon_Test_Results_FC.csv", row.names = FALSE)
write.csv(results_SC, "Wilcoxon_Test_Results_SC.csv", row.names = FALSE)

cat("Wilcoxon test results saved for FC and SC in Word & CSV files.\n")

#*****************************************************************************************
# III. Machine Learning integrated truncated regression for bias-corrected DEA efficiency
#*****************************************************************************************

# Step 1: Setting working directory and calling libraries
#*******************************************************
#Step 1.1: Set working directory 
setwd("C:/DATA/CIMMYT/EIA/ANALYSIS_NEW/CLEANED DATA/Analysis with Benchmarking Package/mDSR_Analyses_Final/New folder/mDSR Analysis_Final_Working")
getwd()
#1.2:  Load Required Libraries
library(truncreg)
library(randomForest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(caret)
library(flextable)
library(officer)

# Step 2: Load and Preprocess Data
#*********************************
#Step 2.1: Load the dataset
ML_data <- read.csv("ML_data.csv")

#Step 2.2: Data preprocessing function
process_data <- function(data) {
  data <- data %>%
    mutate(
      efficiency_bias_corrected = as.numeric(efficiency_bias_corrected),
      efficiency_bias_corrected = replace_na(efficiency_bias_corrected, mean(efficiency_bias_corrected, na.rm = TRUE)),
      PROD = as.numeric(PROD),
      PROD = replace_na(PROD, mean(PROD, na.rm = TRUE)),
      Age = as.numeric(Age),
      Edn = as.numeric(Edn),
      RV = as.factor(RV),
      PVN = factor(PVN, levels = c("Takeo", setdiff(unique(PVN), "Takeo"))), # "Takeo" as reference
      dsrExtn = as.factor(dsrExtn),
      dsrTrg = as.factor(dsrTrg),
      USE = as.factor(USE)
    )
  return(data)
}

#Step 2.3: Preprocess data for First Crop (FC) and Second Crop (SC)
data_FC <- process_data(subset(ML_data, CS == "FC"))
data_SC <- process_data(subset(ML_data, CS == "SC"))

# Step 3: Machine Learning: Feature Selection Using Random Forest
#*****************************************************************
#Step 3.1:  Define predictors and target
predictors <- c("RE", "RV", "PVN", "dsrExtn", "dsrTrg", "Age", "Edn")
target <- "efficiency_bias_corrected"

# Step 3.2: Train Random Forest for feature importance (First Crop)
rf_model_FC <- randomForest(as.formula(paste(target, "~", paste(predictors, collapse = " + "))),
                            data = data_FC, ntree = 500, importance = TRUE)

#Step 3.3: Train Random Forest for feature importance (Second Crop)
rf_model_SC <- randomForest(as.formula(paste(target, "~", paste(predictors, collapse = " + "))),
                            data = data_SC, ntree = 500, importance = TRUE)

#Step 3.4: Extract feature importance
feature_importance_FC <- importance(rf_model_FC)
feature_importance_SC <- importance(rf_model_SC)

#Step 3.5: Select top predictors
selected_predictors_FC <- rownames(feature_importance_FC[order(feature_importance_FC[, 1], decreasing = TRUE), ])[1:5]
selected_predictors_SC <- rownames(feature_importance_SC[order(feature_importance_SC[, 1], decreasing = TRUE), ])[1:5]

# Step 3.6: Combine Feature Selection and Regression Results into Tables

# Step 3.7: Summarize feature importance for both crops
summary(feature_importance_FC)
summary(feature_importance_SC)

# Step 3.8: Combine feature importance data into a single table for both crops
feature_table <- bind_rows(
  data.frame(
    Crop = "First Crop",
    Predictor = rownames(feature_importance_FC),
    Importance = feature_importance_FC[, "%IncMSE"],
    IncNodePurity = feature_importance_FC[, "IncNodePurity"]
  ),
  data.frame(
    Crop = "Second Crop",
    Predictor = rownames(feature_importance_SC),
    Importance = feature_importance_SC[, "%IncMSE"],
    IncNodePurity = feature_importance_SC[, "IncNodePurity"]
  )
)

# Step 3.9: Create a flextable for better visualization
library(flextable)
feature_flextable <- feature_table %>%
  arrange(Crop, desc(Importance)) %>%
  flextable() %>%
  set_header_labels(
    Predictor = "Predictor",
    Importance = "% Increase in MSE",
    IncNodePurity = "Increase in Node Purity",
    Crop = "Crop"
  ) %>%
  autofit()

# Step 3.10: Save feature selection table as a Word file
save_as_docx(feature_flextable, path = "Feature_Selection_Results.docx")

# Step 4: Helper Function to Run Truncated Regression for Each MOP
#*****************************************************************
run_truncated_regression_per_mop <- function(data, mops, predictors, target, training_data) {
  results <- lapply(mops, function(mop) {
    subset_data <- data %>% filter(MOP == mop)
    
    # Ensure factor levels match
    for (var in predictors) {
      if (is.factor(subset_data[[var]]) && is.factor(training_data[[var]])) {
        subset_data[[var]] <- factor(subset_data[[var]], levels = levels(training_data[[var]]))
        subset_data[[var]] <- droplevels(subset_data[[var]])
      }
    }
    
    # Run truncated regression
    tryCatch({
      run_truncated_regression(subset_data, predictors, target, training_data)
    }, error = function(e) {
      cat("\nError for MOP:", mop, "\n")
      cat("Error Message:", e$message, "\n")
      return(NULL)
    })
  })
  names(results) <- mops
  return(results)
}

# Truncated Regression Function
run_truncated_regression <- function(data, predictors, target, training_data) {
  # Align levels for categorical variables
  for (var in predictors) {
    if (is.factor(data[[var]]) && is.factor(training_data[[var]])) {
      data[[var]] <- factor(data[[var]], levels = levels(training_data[[var]]))
      data[[var]] <- droplevels(data[[var]])
    }
  }
  
  # Define the formula
  formula <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
  
  # Run truncated regression
  truncated_model <- tryCatch({
    truncreg(formula, data = data, point = 0, direction = "left")
  }, error = function(e) {
    warning("Truncated regression failed: ", e$message)
    return(NULL)
  })
  
  # If model fitting failed, return NULL
  if (is.null(truncated_model)) {
    return(NULL)
  }
  
  #Predict efficiencies
  data$predicted_efficiency <- tryCatch({
    predict(truncated_model, newdata = data)
  }, error = function(e) {
    warning("Prediction failed: ", e$message)
    return(rep(NA, nrow(data)))
  })
  
  # Return model and data
  return(list(model = truncated_model, summary = summary(truncated_model), data = data))
}

# Step 5: Apply Truncated Regression for Each Crop Season (FC & SC)
#******************************************************************
mops_FC <- unique(data_FC$MOP)
mops_SC <- unique(data_SC$MOP)

#Step 5.1: Run truncated regression for First Crop
results_FC <- run_truncated_regression_per_mop(data_FC, mops_FC, selected_predictors_FC, target, data_FC)

#Step 5.2: Run truncated regression for Second Crop
results_SC <- run_truncated_regression_per_mop(data_SC, mops_SC, selected_predictors_SC, target, data_SC)

# Step 6: Display Results with Notes
#***********************************
extract_results_with_note <- function(results, crop_label, mops) {
  note <- "Note: The reference levels (\"IR504\" for RV and \"Irrigated\" for RE) are used as the baseline category in the truncated regression models. Coefficients for the other levels of RV and RE in the regression results represent their impact relative to the reference levels."
  
  for (i in seq_along(results)) {
    if (!is.null(results[[i]])) {
      cat(sprintf("\n--- Regression Summary for %s, MOP: %s ---\n\n", crop_label, mops[i]))
      print(results[[i]]$summary)
      cat("\n", note, "\n")
    } else {
      cat(sprintf("\n--- No Results for %s, MOP: %s ---\n\n", crop_label, mops[i]))
    }
  }
}

#Step 6.1: Display results for First Crop (FC) with notes
cat("\n### Detailed Regression Results with Notes: First Crop (FC) ###\n")
extract_results_with_note(results_FC, "First Crop (FC)", mops_FC)

#Step 6.2: Display results for Second Crop (SC) with notes
cat("\n### Detailed Regression Results with Notes: Second Crop (SC) ###\n")
extract_results_with_note(results_SC, "Second Crop (SC)", mops_SC)

# Step 7: Save Predicted Efficiencies as a csv file
#****************************************************
save_efficiencies <- function(results, crop_label) {
  efficiencies <- lapply(results, function(res) {
    if (!is.null(res)) {
      return(data.frame(MOP = unique(res$data$MOP), 
                        Predicted_Efficiency = mean(res$data$predicted_efficiency, na.rm = TRUE)))
    } else {
      return(NULL)
    }
  })
  efficiencies <- do.call(rbind, efficiencies)
  write.csv(efficiencies, paste0("Predicted_Efficiencies_", crop_label, ".csv"), row.names = FALSE)
}

#Step 7.1: Save efficiencies for FC and SC
save_efficiencies(results_FC, "FC")
save_efficiencies(results_SC, "SC")

predicted_efficiencies_FC <- do.call(rbind, lapply(results_FC, function(res) if (!is.null(res)) res$data))
predicted_efficiencies_SC <- do.call(rbind, lapply(results_SC, function(res) if (!is.null(res)) res$data))
predicted_efficiencies_FC <- do.call(rbind, lapply(results_FC, function(res) if (!is.null(res)) res$data))
predicted_efficiencies_SC <- do.call(rbind, lapply(results_SC, function(res) if (!is.null(res)) res$data))

# Save full datasets with predictions
write.csv(predicted_efficiencies_FC, "Predicted_Efficiencies_FC.csv", row.names = FALSE)
write.csv(predicted_efficiencies_SC, "Predicted_Efficiencies_SC.csv", row.names = FALSE)

# Step 8: Function to run truncated regression & print estimates with additional parameters
#*****************************************************************************************
run_truncated_regression_verbose <- function(data, predictors, target) {
  formula <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
  
  tryCatch({
    model <- truncreg(formula, data = data, point = 0, direction = "left")
    summary_model <- summary(model)
    
    # Extract additional parameters
    r_squared <- 1 - (summary_model$deviance / summary_model$null.deviance)
    aic <- AIC(model)
    bic <- BIC(model)
    log_likelihood <- logLik(model)
    
    # Print estimates
    cat("\nModel Formula:", deparse(formula), "\n")
    cat("\n--- Coefficients ---\n")
    print(coef(summary_model))
    cat("\nR-squared:", round(r_squared, 4))
    cat("\nAIC:", round(aic, 4))
    cat("\nBIC:", round(bic, 4))
    cat("\nLog-Likelihood:", round(log_likelihood, 4))
    cat("\n")
    
    return(list(summary = summary_model, r_squared = r_squared, aic = aic, bic = bic, log_likelihood = log_likelihood))
  }, error = function(e) {
    warning("Truncated regression failed: ", e$message)
    return(NULL)
  })
}

#Step 8.1: Separate data by adopters, non-adopters, and pooled data
adopters_FC <- subset(data_FC, USE == "Yes")
non_adopters_FC <- subset(data_FC, USE == "No")
pooled_FC <- data_FC

adopters_SC <- subset(data_SC, USE == "Yes")
non_adopters_SC <- subset(data_SC, USE == "No")
pooled_SC <- data_SC

#Step 8.2: Run truncated regression for First Crop (FC)
cat("\n### Truncated Regression Results: First Crop (FC) ###\n")

cat("\n--- Adopters ---\n")
results_adopters_FC <- run_truncated_regression_verbose(adopters_FC, selected_predictors_FC, target)

cat("\n--- Non-Adopters ---\n")
results_non_adopters_FC <- run_truncated_regression_verbose(non_adopters_FC, selected_predictors_FC, target)

cat("\n--- Pooled Data ---\n")
results_pooled_FC <- run_truncated_regression_verbose(pooled_FC, selected_predictors_FC, target)

#Step 8.3: Run truncated regression for Second Crop (SC)
cat("\n### Truncated Regression Results: Second Crop (SC) ###\n")

cat("\n--- Adopters ---\n")
results_adopters_SC <- run_truncated_regression_verbose(adopters_SC, selected_predictors_SC, target)

cat("\n--- Non-Adopters ---\n")
results_non_adopters_SC <- run_truncated_regression_verbose(non_adopters_SC, selected_predictors_SC, target)

cat("\n--- Pooled Data ---\n")
results_pooled_SC <- run_truncated_regression_verbose(pooled_SC, selected_predictors_SC, target)

#Step 8.4: Save results into a list for further use
results_list <- list(
  FC = list(
    Adopters = results_adopters_FC,
    NonAdopters = results_non_adopters_FC,
    Pooled = results_pooled_FC
  ),
  SC = list(
    Adopters = results_adopters_SC,
    NonAdopters = results_non_adopters_SC,
    Pooled = results_pooled_SC
  )
)

#Step 8.5: Save detailed results to a text file for documentation
writeLines(capture.output(results_list), "Truncated_Regression_Results_Detailed.txt")

# Step 9: Visualization and saving of efficiency estimates 
#********************************************************
#Step 9.1: Visualization for Second Crop
ggplot(predicted_efficiencies_FC, aes(x = MOP, y = predicted_efficiency, fill = MOP)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Predicted Efficiencies for First Crop", y = "Efficiency", x = "MOP")

#Step 9.2: Visualization for Second Crop
ggplot(predicted_efficiencies_SC, aes(x = MOP, y = predicted_efficiency, fill = MOP)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Predicted Efficiencies for Second Crop", y = "Efficiency", x = "MOP")

#Step 9.3: Calculate averages for First Crop
avg_efficiencies_FC <- predicted_efficiencies_FC %>%
  group_by(MOP) %>%
  summarise(
    Avg_Observed = mean(efficiency_bias_corrected, na.rm = TRUE),
    Avg_Predicted = mean(predicted_efficiency, na.rm = TRUE)
  )

#Step 9.4: Calculate averages for Second Crop
avg_efficiencies_SC <- predicted_efficiencies_SC %>%
  group_by(MOP) %>%
  summarise(
    Avg_Observed = mean(efficiency_bias_corrected, na.rm = TRUE),
    Avg_Predicted = mean(predicted_efficiency, na.rm = TRUE)
  )

#Step 9.5: Save the summaries
write.csv(avg_efficiencies_FC, "Average_Efficiencies_FC.csv", row.names = FALSE)
write.csv(avg_efficiencies_SC, "Average_Efficiencies_SC.csv", row.names = FALSE)

refined_predictors_FC <- selected_predictors_FC
refined_predictors_SC <- selected_predictors_SC

#Step 9.6: Refined results for First Crop
refined_results_FC <- lapply(mops_FC, function(mop) {
  subset_data <- data_FC %>% filter(MOP == mop)
  run_truncated_regression(subset_data, refined_predictors_FC, target, data_FC)
})

#Step 9.7: Refined results for Second Crop
refined_results_SC <- lapply(mops_SC, function(mop) {
  subset_data <- data_SC %>% filter(MOP == mop)
  run_truncated_regression(subset_data, refined_predictors_SC, target, data_SC)
})
mops_FC <- unique(data_FC$MOP)
mops_SC <- unique(data_SC$MOP)

# Step 10: Function to calculate rankings of RVs for each RE and MOP
#******************************************************************
rank_RVs_per_RE <- function(data, crop_label) {
  ranked_data <- data %>%
    group_by(MOP, RE, RV) %>%
    summarise(Average_Efficiency = mean(predicted_efficiency, na.rm = TRUE)) %>%
    arrange(MOP, RE, desc(Average_Efficiency)) %>%
    group_by(MOP, RE) %>%
    mutate(Rank = rank(-Average_Efficiency, ties.method = "min")) %>%
    ungroup()
  
  # Save to CSV
  write.csv(ranked_data, paste0("Ranked_RVs_", crop_label, ".csv"), row.names = FALSE)
  
  return(ranked_data)
}

#Step 10.1: Rank RVs for First Crop (FC)
ranked_RVs_FC <- rank_RVs_per_RE(predicted_efficiencies_FC, "FC")

#Step 10.2: Rank RVs for Second Crop (SC)
ranked_RVs_SC <- rank_RVs_per_RE(predicted_efficiencies_SC, "SC")

#Step 10.3: View Ranked Data for FC and SC
head(ranked_RVs_FC)
head(ranked_RVs_SC)

# Step 11: Visualizations of Efficiencies
#Step 11.1: Combine predicted efficiencies from all subsets in results_FC
predicted_efficiencies_FC <- do.call(rbind, lapply(results_FC, function(res) if (!is.null(res)) res$data))

#Step 11.2: Merge predicted efficiencies into data_FC by HID
data_FC <- merge(data_FC, predicted_efficiencies_FC[, c("HID", "predicted_efficiency")], by = "HID", all.x = TRUE)

#Step 11.3: Verify if predicted_efficiency is appended
head(data_FC)

#Step 11.4: Combine predicted efficiencies from all subsets in results_SC
predicted_efficiencies_SC <- do.call(rbind, lapply(results_SC, function(res) if (!is.null(res)) res$data))

#Step 11.5: Merge predicted efficiencies into data_SC by HID
data_SC <- merge(data_SC, predicted_efficiencies_SC[, c("HID", "predicted_efficiency")], by = "HID", all.x = TRUE)

#Step 11.6: Verify if predicted_efficiency is appended
head(data_SC)

#Step 11.7: Visualization for Predicted Efficiencies: First Crop

ggplot(data_FC, aes(x = MOP, y = predicted_efficiency, fill = MOP)) +
  geom_violin(trim = FALSE, alpha = 0.6, color = "black") +
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Predicted Efficiencies for First Crop",
    y = "Predicted Efficiency",
    x = "Management Operation Practice (MOP)"
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


#Step 11.8: Visualization for Predicted Efficiencies: Second Crop

ggplot(data_SC, aes(x = MOP, y = predicted_efficiency, fill = MOP)) +
  geom_violin(trim = FALSE, alpha = 0.6, color = "black") +
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, color = "black") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Predicted Efficiencies for Second Crop",
    y = "Predicted Efficiency",
    x = "Management Operation Practice (MOP)"
  ) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

#Step 11.9: Prepare data for Ridgeline Plots
data_FC_long <- data_FC %>%
  select(MOP, bias_corrected_efficiency = efficiency_bias_corrected, predicted_efficiency) %>%
  pivot_longer(
    cols = c(bias_corrected_efficiency, predicted_efficiency),
    names_to = "Efficiency_Type", values_to = "Efficiency_Value"
  )

data_SC_long <- data_SC %>%
  select(MOP, bias_corrected_efficiency = efficiency_bias_corrected, predicted_efficiency) %>%
  pivot_longer(
    cols = c(bias_corrected_efficiency, predicted_efficiency),
    names_to = "Efficiency_Type", values_to = "Efficiency_Value"
  )

data_combined <- bind_rows(
  mutate(data_FC_long, Crop = "First Crop"),
  mutate(data_SC_long, Crop = "Second Crop")
)

#Step 11.10 Define custom colors for MOPs
custom_colors <- c(
  "mDSR+CH" = "olivedrab4",
  "mDSR+M" = "yellowgreen",
  "NomDSR+CH" = "salmon2",
  "NomDSR+M" = "tomato3"
)

#Step 11.11: Ridgeline Plot for Efficiencies of MOPs
ggplot(data_combined, aes(x = Efficiency_Value, y = MOP, fill = MOP)) +
  geom_density_ridges_gradient(
    scale = 1.8, rel_min_height = 0.01, gradient_lwd = 0.3
  ) +
  scale_fill_manual(
    values = custom_colors,
    name = "Management Operation Practice (MOP)"
  ) +
  facet_grid(Crop ~ Efficiency_Type, labeller = labeller(
    Crop = c("First Crop" = "First Crop (FC)", "Second Crop" = "Second Crop (SC)"),
    Efficiency_Type = c(
      "bias_corrected_efficiency" = "Bias-Corrected Efficiency",
      "predicted_efficiency" = "Predicted Efficiency"
    )
  )) +
  theme_minimal() +
  labs(
    title = "Ridgeline Plots of Bias-Corrected and Predicted Efficiencies by MOP",
    x = "Efficiency",
    y = "Management Operation Practice (MOP)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12),
    legend.position = "right",
    panel.spacing = unit(1, "lines")  # Increase space between panels
  )

# Step 12: Summarizing efficiency estimates
#*****************************************

#Step 12.1: Function to summarize efficiencies, round values, and save as a Word table
summarize_efficiencies_to_word <- function(data, crop_label, file_name) {
  summary_table <- data %>%
    group_by(MOP) %>%
    summarise(
      Predicted_Mean = round(mean(predicted_efficiency, na.rm = TRUE), 2),
      Predicted_SD = round(sd(predicted_efficiency, na.rm = TRUE), 2),
      Predicted_Min = round(min(predicted_efficiency, na.rm = TRUE), 2),
      Predicted_Max = round(max(predicted_efficiency, na.rm = TRUE), 2),
      Observed_Mean = round(mean(efficiency_bias_corrected, na.rm = TRUE), 2),
      Observed_SD = round(sd(efficiency_bias_corrected, na.rm = TRUE), 2),
      Observed_Min = round(min(efficiency_bias_corrected, na.rm = TRUE), 2),
      Observed_Max = round(max(efficiency_bias_corrected, na.rm = TRUE), 2)
    )
  
  # Create a flextable
  ft <- flextable(summary_table) %>%
    set_header_labels(
      MOP = "Management Operation Practice",
      Predicted_Mean = "Predicted Efficiency (Mean)",
      Predicted_SD = "Predicted Efficiency (SD)",
      Predicted_Min = "Predicted Efficiency (Min)",
      Predicted_Max = "Predicted Efficiency (Max)",
      Observed_Mean = "Observed Efficiency (Mean)",
      Observed_SD = "Observed Efficiency (SD)",
      Observed_Min = "Observed Efficiency (Min)",
      Observed_Max = "Observed Efficiency (Max)"
    ) %>%
    add_header_row(
      values = paste("Efficiency Summary for", crop_label), 
      colwidths = ncol(summary_table)
    ) %>%
    autofit()
  
  # Save the table to a Word file
  save_as_docx(ft, path = file_name)
  
  cat("\nSaved Efficiency Summary for", crop_label, "to", file_name, "\n")
}

# Summarize and save results for First Crop (FC)
summarize_efficiencies_to_word(data_FC, "First Crop (FC)", "Efficiency_Summary_FC.docx")

# Summarize and save results for Second Crop (SC)
summarize_efficiencies_to_word(data_SC, "Second Crop (SC)", "Efficiency_Summary_SC.docx")

# Step 13:Summary stat of ML selected predictors (Missing from the previous steps)
#**********************************************************************************
# Load the data
ML_data <- read.csv("ML_data.csv")

# Select relevant variables
selected_vars <- c("RV", "RE", "dsrTrg", "dsrExtn", "PVN", "Edn", "Age", "CS")

# Filter the dataset for relevant columns
data_selected <- ML_data %>% select(all_of(selected_vars))

# Function to compute summary statistics for categorical variables
categorical_summary <- function(var, data) {
  summary_table <- data %>%
    group_by(CS, .data[[var]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Percentage = round(n / sum(n) * 100, 2))
  
  # Convert to a wide format
  summary_wide <- summary_table %>%
    pivot_wider(names_from = CS, values_from = c(n, Percentage))
  
  return(summary_wide)
}

# Function to compute summary statistics for continuous variables
continuous_summary <- function(var, data) {
  summary_table <- data %>%
    group_by(CS) %>%
    summarise(
      Mean = round(mean(.data[[var]], na.rm = TRUE), 2),
      SD = round(sd(.data[[var]], na.rm = TRUE), 2),
      Median = round(median(.data[[var]], na.rm = TRUE), 2),
      IQR = round(IQR(.data[[var]], na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  return(summary_table)
}

# Perform significance tests between seasons
significance_tests <- function(var, data) {
  if (is.character(data[[var]]) || is.factor(data[[var]])) {
    # Chi-square test for categorical variables
    test_result <- tryCatch({
      test <- chisq.test(table(data[[var]], data$CS))
      p_value <- round(test$p.value, 4)
      test_statistic <- round(test$statistic, 2)
      test_method <- "Chi-square Test"
    }, error = function(e) {
      p_value <- NA
      test_statistic <- NA
      test_method <- "Chi-square Test (Invalid due to small sample)"
    })
  } else {
    # T-test or Wilcoxon test for continuous variables
    if (shapiro.test(data[[var]][data$CS == "FC"])$p.value > 0.05 & 
        shapiro.test(data[[var]][data$CS == "SC"])$p.value > 0.05) {
      test <- t.test(data[[var]] ~ data$CS, var.equal = FALSE)
      test_method <- "T-test"
    } else {
      test <- wilcox.test(data[[var]] ~ data$CS)
      test_method <- "Wilcoxon Test"
    }
    p_value <- round(test$p.value, 4)
    test_statistic <- round(test$statistic, 2)
  }
  
  return(data.frame(Variable = var, Test = test_method, Statistic = test_statistic, p_value = p_value))
}

# Generate summary statistics for all variables
summary_results <- list()
test_results <- list()

for (var in selected_vars[-length(selected_vars)]) {  # Exclude CS
  if (is.character(ML_data[[var]]) || is.factor(ML_data[[var]])) {
    summary_results[[var]] <- categorical_summary(var, ML_data)
  } else {
    summary_results[[var]] <- continuous_summary(var, ML_data)
  }
  test_results[[var]] <- significance_tests(var, ML_data)
}

# Combine test results into a single dataframe
test_results_df <- do.call(rbind, test_results)

# Create a Word document
doc <- read_docx()

# Add summary tables
for (var in names(summary_results)) {
  doc <- doc %>%
    body_add_par(var, style = "heading 2") %>%
    body_add_flextable(flextable(summary_results[[var]])) %>%
    body_add_par("")
}

# Add significance test results
doc <- doc %>%
  body_add_par("Significance Tests Between Crop Seasons", style = "heading 1") %>%
  body_add_flextable(flextable(test_results_df))

# Save the document
save_path <- "Predictor_Summary_Statistics_and_Tests.docx"
print(doc, target = save_path)

# Print completion message
cat("Summary statistics and significance tests saved to", save_path)

# Step 14: Visualization of rice yield and bias-corrected efficiency of IBs for FC & SC
#**************************************************************************************

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)  # For combining plots

# Load the data
ML_data <- read.csv("ML_data.csv")

# Function to remove outliers using IQR
remove_outliers <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  data %>% filter(data[[column]] >= (Q1 - 1.5 * IQR) & data[[column]] <= (Q3 + 1.5 * IQR))
}

# Remove outliers
cleaned_data <- ML_data %>%
  group_by(MOP) %>%
  filter(!is.na(YIELD), !is.na(efficiency_bias_corrected), !is.na(CS)) %>%
  do(remove_outliers(., "YIELD")) %>%
  do(remove_outliers(., "efficiency_bias_corrected"))

# Define mapping of MOP values to IB labels
MOP_mapping <- c("mDSR+CH" = "IB1", "mDSR+M" = "IB2", "NomDSR+CH" = "IB3", "NomDSR+M" = "IB4")

# Apply the mapping
cleaned_data <- cleaned_data %>%
  mutate(MOP_IB = recode(MOP, !!!MOP_mapping))

# Define IB colors
MOP_colors <- c("IB1" = "limegreen", "IB2" = "steelblue1", "IB3" = "hotpink", "IB4" = "orangered3")

# Compute mean values for annotation
mean_values <- cleaned_data %>%
  group_by(CS, MOP_IB) %>%
  summarise(
    avg_YIELD = mean(YIELD, na.rm = TRUE),
    avg_efficiency = mean(efficiency_bias_corrected, na.rm = TRUE),
    .groups = "drop"
  )

# Separate data for FC and SC
data_FC <- cleaned_data %>% filter(CS == "FC")
data_SC <- cleaned_data %>% filter(CS == "SC")
mean_FC <- mean_values %>% filter(CS == "FC")
mean_SC <- mean_values %>% filter(CS == "SC")

# Define common axis limits and breaks for uniform comparison
x_limits <- c(2.5, 6.5)  # Yield axis range
y_limits <- c(0.2, 1.0)   # Efficiency axis range
x_breaks <- seq(2.5, 6.5, by = 0.5)
y_breaks <- seq(0.2, 1.0, by = 0.2)

# Function to create individual plots with uniform axes and border
create_plot <- function(data, mean_values, title) {
  ggplot(data, aes(x = YIELD, y = efficiency_bias_corrected, color = MOP_IB, fill = MOP_IB)) +
    geom_point(alpha = 0.6, size = 2) +  
    stat_ellipse(geom = "polygon", alpha = 0.2, level = 0.8) +  # Cluster circles
    geom_text(data = mean_values, aes(x = avg_YIELD, y = avg_efficiency,
                                      label = paste0("(", round(avg_YIELD, 2), ", ", round(avg_efficiency, 2), ")")),
              color = "black", size = 3, vjust = -0.5) +  
    scale_color_manual(values = MOP_colors) +  
    scale_fill_manual(values = MOP_colors) +  
    scale_x_continuous(limits = x_limits, breaks = x_breaks) +  # Uniform X-axis
    scale_y_continuous(limits = y_limits, breaks = y_breaks) +  # Uniform Y-axis
    labs(
      title = title,
      x = "Rice Yield (t ha⁻¹)",
      y = "Bias-Corrected Efficiency",
      color = "Innovation Bundles",
      fill = "Innovation Bundles"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      
      # Border for each panel
      panel.border = element_rect(color = "black", linewidth = 1.5, fill = NA)
    )
}

# Create plots with uniform axes and borders
plot_FC <- create_plot(data_FC, mean_FC, "A. Dry season rice (2019-2020)")
plot_SC <- create_plot(data_SC, mean_SC, "B. Wet season rice (2019-2020)")

# Combine the two plots with patchwork and shared legend
combined_plot <- plot_FC + plot_SC + 
  plot_layout(ncol = 2, guides = "collect") +  # Shared legend
  plot_annotation(title = "Efficiency vs Yield for Dry & Wet Season")  # Adds title without affecting layout

# Print the combined plot
print(combined_plot)


# Generate summary statistics for YIELD and Efficiency
summary_stats <- cleaned_data %>%
  group_by(CS, IB) %>%
  summarise(
    avg_YIELD = mean(YIELD, na.rm = TRUE),
    sd_YIELD = sd(YIELD, na.rm = TRUE),
    min_YIELD = min(YIELD, na.rm = TRUE),
    max_YIELD = max(YIELD, na.rm = TRUE),
    avg_efficiency = mean(efficiency_bias_corrected, na.rm = TRUE),
    sd_efficiency = sd(efficiency_bias_corrected, na.rm = TRUE),
    min_efficiency = min(efficiency_bias_corrected, na.rm = TRUE),
    max_efficiency = max(efficiency_bias_corrected, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(CS, IB)

# Print the summary statistics to console
print(summary_stats)

# Save summary statistics as CSV
write.csv(summary_stats, "Efficiency_vs_Yield_Summary_Stats.csv", row.names = FALSE)

message("Summary statistics saved as 'Efficiency_vs_Yield_Summary_Stats.csv'.")


