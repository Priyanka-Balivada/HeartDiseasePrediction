library(e1071)   # for svm()
library(randomForest)   # for randomForest()
library(rpart)   # for decision tree classifier
library(ada)  # for Ada boost classifier

# Load the data
heart_data <- read.csv("heart_updated.csv")

heart_data$HeartDisease <- as.factor(heart_data$HeartDisease) #write with as or without as no change in output
summary(heart_data)

head(heart_data)

levels(heart_data$HeartDisease);
View(heart_data);
# Split the data into training and validation datasets
set.seed(123)   # for reproducibility
View(heart_data);
train_index <- sample(nrow(heart_data), 0.8 * nrow(heart_data))
train_index;
train_data <- heart_data[train_index, ]
head(train_data)
View(train_data)
val_data <- heart_data[-train_index, ]
head(val_data)

# Create the SVM model
svm_model <- svm(HeartDisease ~ ., data = train_data, kernel = "linear", cost = 1, scale = TRUE)

# Create the random forest model
rf_model <- randomForest(HeartDisease ~ ., data = train_data, ntree = 500, mtry = sqrt(ncol(train_data)), type = "classification")

# Create the random decision tree
dt_model <- rpart(HeartDisease ~ ., data = train_data, method = "class")

ada_model <- ada(x = as.data.frame(train_data[, -ncol(train_data)]),y = train_data$HeartDisease, iter = 200)

# Evaluate the accuracy of the models on the validation dataset
svm_pred <- predict(svm_model, newdata = val_data)
svm_acc <- mean(svm_pred == val_data$HeartDisease)

rf_pred <- predict(rf_model, newdata = val_data)
rf_acc <- mean(rf_pred == val_data$HeartDisease)

dt_pred <- predict(dt_model, newdata = val_data, type = "class")
dt_acc <- mean(dt_pred == val_data$HeartDisease)

ada_pred <- predict(ada_model, as.data.frame(train_data[, -ncol(train_data)]))
ada_acc <- mean(ada_pred == train_data$HeartDisease)
cat("AdaBoost Accuracy:", ada_acc, "\n")

# Select the top 3 models based on accuracy
if (svm_acc > rf_acc & svm_acc > dt_acc & svm_acc > ada_acc) {
  best_model1 <- svm_model
  if (rf_acc > dt_acc & rf_acc > ada_acc) {
    best_model2 <- rf_model
    if (dt_acc > ada_acc) {
      best_model3 <- dt_model
    } else {
      best_model3 <- ada_model
    }
  } else if (dt_acc > ada_acc) {
    best_model2 <- dt_model
    best_model3 <- rf_model
  } else {
    best_model2 <- ada_model
    best_model3 <- rf_model
  }
} else if (rf_acc > svm_acc & rf_acc > dt_acc & rf_acc > ada_acc) {
  best_model1 <- rf_model
  if (svm_acc > dt_acc & svm_acc > ada_acc) {
    best_model2 <- svm_model
    if (dt_acc > ada_acc) {
      best_model3 <- dt_model
    } else {
      best_model3 <- ada_model
    }
  } else if (dt_acc > ada_acc) {
    best_model2 <- dt_model
    best_model3 <- svm_model
  } else {
    best_model2 <- ada_model
    best_model3 <- svm_model
  }
} else if (dt_acc > svm_acc & dt_acc > rf_acc & dt_acc > ada_acc) {
  best_model1 <- dt_model
  if (svm_acc > rf_acc & svm_acc > ada_acc) {
    best_model2 <- svm_model
    if (rf_acc > ada_acc) {
      best_model3 <- rf_model
    } else {
      best_model3 <- ada_model
    }
  } else if (rf_acc > ada_acc) {
    best_model2 <- rf_model
    best_model3 <- svm_model
  } else {
    best_model2 <- ada_model
    best_model3 <- svm_model
  }
} else {
  best_model1 <- ada_model
  if (svm_acc > rf_acc & svm_acc > dt_acc) {
    best_model2 <- svm_model
    if (rf_acc > dt_acc) {
      best_model3 <- rf_model
    } else {
      best_model3 <- dt_model
    }
  } else if (rf_acc > dt_acc) {
    best_model2 <- rf_model
    best_model3 <- svm_model
  } else {
    best_model2 <- dt_model
    best_model3 <- svm_model
  }
}

print(best_model1)
print(best_model2)
print(best_model3)

# Create the ensemble model by combining the three best models
ensemble_pred <- predict(best_model1, newdata = val_data)
for (i in 1:length(ensemble_pred)) {
  if (ensemble_pred[i] == 0) {
    ensemble_pred[i] <- predict(best_model2, newdata = val_data[i, ])
    if (ensemble_pred[i] == 0) {
      ensemble_pred[i] <- predict(best_model3, newdata = val_data[i, ])
    }
  }
}

# Evaluate the accuracy of the ensemble model
ensemble_acc <- mean(ensemble_pred == val_data$HeartDisease)

# Print the accuracy of the models and the ensemble model
cat("SVM accuracy:", svm_acc, "\n")
cat("Random Forest accuracy:", rf_acc, "\n")
cat("Decision Tree accuracy:", dt_acc, "\n")
cat("Ada Boost accuracy:", ada_acc, "\n")
cat("Ensemble accuracy:", ensemble_acc, "\n")

new_data <- data.frame(Age = 63, Sex = 1, ChestPainType = 0, RestingBP = 140, Cholesterol = 260, FastingBS = 0, RestingECG = 2, MaxHR = 112, ExerciseAngina = 1, Oldpeak=3,ST_Slope = 1)

# Predict output using each model
svm_pred <- predict(svm_model, newdata = new_data)
rf_pred <- predict(rf_model, newdata = new_data)
dt_pred <- predict(dt_model, newdata = new_data)
ada_pred <- predict(ada_model, newdata = new_data)

ensemble_pred <- predict(best_model1, newdata = new_data)
for (i in 1:length(ensemble_pred)) {
  if (ensemble_pred[i] == 0) {
    ensemble_pred[i] <- predict(best_model2, newdata = new_data[i, ])
    if (ensemble_pred[i] == 0) {
      ensemble_pred[i] <- predict(best_model3, newdata = new_data[i, ])
    }
  }
}

# Print the predicted output from each model
cat("SVM predicted output:", svm_pred, "\n")
cat("Random Forest predicted output:", rf_pred, "\n")
cat("Decision Tree predicted output:", dt_pred, "\n")
cat("Ada predicted output:", ada_pred, "\n")
cat("Ensembling classifier predicted output:", ensemble_pred, "\n")
