library(caret)
library(e1071)
library(randomForest)
library(mlr)
library(readr)
library(precrec)
library(polycor)
library(dplyr)
library(tibble)
library(terra)
library(sf)
library(tools)

# Path to the input CSV file
input_file <- 'Adaptive_Sampling/Sample_points/df_pb_ratio10_buffer20km.csv'
# Extract file name without extension
file_name <- file_path_sans_ext(basename(input_file))
# Create the output folder with the same name as the file
output_folder <- file.path("Adaptive_Sampling/Results", file_name)
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Loading the presence-background data
df_pb <- read_csv(input_file)
# Convert dataframe in shp
df_pb <- sf::st_as_sf(df_pb, coords = c("x", "y"), crs = 4326)

# Set the path to your raster files 
files_raster <- list.files(path = "Adaptive_Sampling/Inputs_raster_scaled", pattern = "\\.tif$", full.names = TRUE)
# Load rasters into a SpatRaster object
rasters <- rast(files_raster)




# Extract the raster values for the species points as a dataframe
model_data <- terra::extract(rasters, df_pb, df = TRUE, ID = FALSE)
# Adding species column to the dataframe
model_data$occ <- as.factor(df_pb$occ)
# Check NAs
anyNA(model_data)

# Using colMeans()
mean_val <- colMeans(model_data[-ncol(model_data)],na.rm = TRUE)
# Replacing NA with mean value of each column
for(i in colnames(model_data[-ncol(model_data)])) {
  model_data[-ncol(model_data)][,i][is.na(model_data[-ncol(model_data)][,i])] <- mean_val[i]
}

# Separation between predictor (X) and target (y) variables
X <- model_data[, -ncol(model_data)]
y <- model_data[, ncol(model_data)]

# Function to create a list of models
get_models <- function() {
  models <- list()
  # Basic models
  models[[1]] <- makeLearner("classif.logreg", predict.type = "prob")
  models[[2]] <- makeLearner("classif.rpart", predict.type = "prob",minsplit = 10, cp = 0.01)
  models[[3]] <- makeLearner("classif.svm", kernel = "linear", predict.type = "prob", cost = 1)
  models[[4]] <- makeLearner("classif.kknn", predict.type = "prob", k = 10, distance = 2)
  # Advanced models
  models[[5]] <- makeLearner("classif.ada", predict.type = "prob",nu = 0.1, iter = 50)
  models[[6]] <- makeLearner("classif.ranger", predict.type = "prob",num.trees = 100, mtry = 3, min.node.size = 5)
  models[[7]] <- makeLearner("classif.randomForest", ntree = 100, predict.type = "prob",mtry = 3, nodesize = 5)
  models[[8]] <- makeLearner("classif.gbm", predict.type = "prob",n.trees = 100, interaction.depth = 3, shrinkage = 0.1, n.minobsinnode = 10)
  
  return(models)
}

# Function to create super learner
get_super_learner <- function() {
  ensemble <- makeStackedLearner(base.learners = get_models(), 
                                 super.learner = makeLearner("classif.logreg", predict.type = "prob"),
                                 method = "stack.cv", resampling = makeResampleDesc("CV", iters = 50))
  return(ensemble)
}

# Split
set.seed(123) # For reproducibility
train_index <- createDataPartition(y, p = 0.70, list = FALSE)
X_train <- X[train_index, ]
y_train <- as.factor(y[train_index])
# Change the order of the levels to make class 1 as positive
y_train <- relevel(y_train, ref = "1")
X_val <- X[-train_index, ]
y_val <- as.factor(y[-train_index])
# Change the order of the levels to make class 1 as positive
y_val <- relevel(y_val, ref = "1")
cat('Train', dim(X_train), length(y_train), 'Test', dim(X_val), length(y_val), '\n')

# Normalization of covariates (centering and scaling)
preprocess_params <- preProcess(X_train, method = c("center", "scale"))
# Save the preprocessing parameters
saveRDS(preprocess_params, file = file.path(output_folder, "preprocess_params.rds"))
# Applying the transformation to the training data
X_train_std <- predict(preprocess_params, X_train)
# Application of the transformation to the validation data
X_val_std <- predict(preprocess_params, X_val)
# Save the standardized validation data
saveRDS(X_val_std, file = file.path(output_folder, "X_val_std.rds"))

# Creating the dataframe to save metrics
model_metrics <- tibble(Model = character(), ROC_AUC = numeric(), PR_AUC = numeric(), Accuracy = numeric())

# Function to calculate performance metrics
calculate_metrics <- function(predictions, true_labels) {
  
  scores <- as.numeric(predictions[, "response"])
  
  # Calculation of ROC and Precision-Recall AUC
  eval <- evalmod(scores = scores, labels = true_labels)
  roc_auc <- auc(eval)$aucs[1] # ROC AUC
  pr_auc <- auc(eval)$aucs[2]  # Precision-Recall AUC
  
  # Calculation of accuracy
  predicted_classes <- as.numeric(predictions[, "response"])
  accuracy <- mean(predicted_classes == as.numeric(true_labels)) * 100
  
  return(list(ROC_AUC = roc_auc, PR_AUC = pr_auc, Accuracy = accuracy))
}

# Creation of training tasks 
train_task <- makeClassifTask(data = data.frame(X_train_std, y = y_train), target = "y")
# Base models
base_models <- get_models()
# Loops on base models
for (model in base_models) {
  model_name <- model$id
  # Model training
  trained_model <- train(model, train_task)
  # Predictions about the validation set
  yhat <- predict(trained_model, newdata = data.frame(X_val_std))
  # Calculation of metrics
  metrics <- calculate_metrics(yhat$data, y_val)
  
  # Calculation of sensitivity and specificity
  conf_matrix <- table(Predicted = yhat$data$response, Actual = y_val)
  
  
  # Saving in the dataframe
  model_metrics <- model_metrics %>% add_row(
    Model = model_name,
    ROC_AUC = metrics$ROC_AUC,
    PR_AUC = metrics$PR_AUC,
    Accuracy = metrics$Accuracy
  )
}

# Super Learner
super_learner <- get_super_learner()
trained_super_learner <- train(super_learner, train_task)
yhat_super <- predict(trained_super_learner, newdata = data.frame(X_val_std))
# Save the Super Learner
saveRDS(trained_super_learner, file = file.path(output_folder, "trained_super_learner.rds"))

# Calculation of metrics for the Super Learner
metrics_super <- calculate_metrics(yhat_super$data, y_val)
# Adding Super Learner metrics to the dataframe
model_metrics <- model_metrics %>% add_row(
  Model = "Super Learner",
  ROC_AUC = metrics_super$ROC_AUC,
  PR_AUC = metrics_super$PR_AUC,
  Accuracy = metrics_super$Accuracy
)

# Display model metrics
print(model_metrics)
# Output folder path
metrics_file <- file.path(output_folder, "model_metrics.txt")
# Save model metrics
write.table(model_metrics, file = metrics_file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)


# Output folder path
conf_matrix_file <- file.path(output_folder, "confusion_matrix.txt")
# Calculation of confusion matrix
conf_matrix <- confusionMatrix(as.factor(yhat_super$data$response), as.factor(y_val))
# Save the confusion matrix to a text file
sink(conf_matrix_file) # Redirect output to file
# Print the confusion matrix
print(conf_matrix)
sink() # Restore output to console


# Plot of ROC and PR curves for the Super Learner
precrec_obj <- evalmod(scores = as.numeric(yhat_super$data$response), labels = y_val)
roc_pr_plot <- autoplot(precrec_obj)
# Save the plot as an image
plot_file <- file.path(output_folder, "ROC_PR_curves.png")
ggsave(plot_file, roc_pr_plot, width = 8, height = 6)

# Predictor Importance for Random Forest Model (or any model that supports feature importance)
if ("classif.randomForest" %in% sapply(get_models(), function(x) x$id)) {
  rf_model <- get_models()[[which(sapply(get_models(), function(x) x$id) == "classif.randomForest")]]
  trained_rf <- train(rf_model, train_task)
  importance_rf <- varImp(trained_rf$learner.model)
  importance_rf <- tibble::rownames_to_column(importance_rf, "Variable") 
  #importance_rf$Variable <- row.names(importance_rf) 
  varImpPlot(trained_rf$learner.model)
  print("Importance of predictors for Random Forest:")
  print(importance_rf)
}

# Output folder path
importance_file <- file.path(output_folder, "variable_importance.txt")
# Save predictor importance file
write.table(importance_rf, file = importance_file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)



# Code to map probabilities into a raster
env_values <- as.data.frame(raster::as.data.frame(rasters, xy = TRUE, na.rm = TRUE))
env_values[is.na(env_values)] <- mean_val[colnames(env_values)]
# Extract coordinates
coords <- env_values[,c('x','y')]
# Selects all columns except coordinates
env_values <- env_values[3:16]
# Application of the transformation to the raster data
env_values_std <- predict(preprocess_params, env_values)
# Predict the probability of occurrence (occ = 1)
predictions <- predict(trained_super_learner, newdata = env_values_std)$data$prob.1
# Create dataframe with the predictions
predictions <- data.frame(X = coords$x, 
                          Y = coords$y, 
                          Prob = predictions)
# Save predictions                        
saveRDS(predictions, file = file.path(output_folder, "predictions.rds"))



