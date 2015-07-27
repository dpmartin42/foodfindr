############################
# Author: Daniel Martin
# EDA and model building for
# menupages data
############################

rm(list = ls())

library(dplyr)
library(randomForest)
library(glmnet)
library(ROCR)
library(ggplot2)

set.seed(2348) # set seed for reproducibility

#################
# Read in data

restaurant_data <- read.csv("Data/restaurant_info.csv") %>%
  mutate(tags = gsub(".*'cuisine', |);\n", "", as.character(tags)))

# Set healthy labels based on tags:
# vegan, vegetarian, local/organic, health food, and sandwiches (but not pizza)

restaurant_data$isHealthy <- NA
restaurant_data$isHealthy[grep("vegan|vegetarian|local|salads|health-food|sandwiches", restaurant_data$tags)] <- 1
restaurant_data$isHealthy[grep("pizza", restaurant_data$tags)] <- 0
restaurant_data$isHealthy[is.na(restaurant_data$isHealthy)] <- 0
restaurant_data$isHealthy <- factor(restaurant_data$isHealthy)

summary(restaurant_data$isHealthy) # 28% "healthy"

menu_data <- read.csv("Data/menu_words.csv")

##############################
# EDA on word counts
# (python only kept top 5000)

plot(sort(colSums(menu_data[, -1]), decreasing = TRUE))

# Keep only the top 1000 words

menu_data <- menu_data[, -1]
menu_sub <- menu_data[, names(menu_data) %in% names(sort(colSums(menu_data), decreasing = TRUE))[1:1000]]
menu_sub$isHealthy <- restaurant_data$isHealthy

##########################################
# Compare a random forest and LASSO using 
# either straight frequencies or tf-idf 
# weighting and estimate AUC using 5-fold 
# cross-validation on the training set

# split into training and testing (70/30)

train_ids <- sample(1:nrow(menu_sub), floor(nrow(menu_sub) * .70), replace = FALSE)

training <- menu_sub[train_ids,]
testing <- menu_sub[-train_ids,]

conditions <- expand.grid(mtry = c(10, 31, 60),
                          ntree = c(100, 500, 800),
                          auc_freq = NA,
                          auc_idf = NA)

training$fold <- sample(x = 1:5, size = nrow(training), replace = TRUE)

# calculate tf-idf

tf <- training[, -1001]
idf <- log(nrow(training[, -1001])/colSums(training[, -1001]))
tfidf <- training[, -1001]

for(word in names(idf)){
  tfidf[,word] <- tf[,word] * idf[word]
}

tfidf$isHealthy <- training$isHealthy
tfidf$fold <- training$fold

# Run the random forest across different values for both hyperparameters (ntree and mtry)

for(num_cond in 1:nrow(conditions)){
  
  df <- conditions[num_cond, ]
  auc_freq <- c(NA)
  auc_idf <- c(NA)
  
  for(i in 1:5){
    
    rf_mod_freq <- randomForest(x = training[training$fold != i, 1:1000],
                                y = training[training$fold != i, 1001],
                                ntree = df$ntree, mtry = df$mtry)
    
    rf_mod_idf <- randomForest(x = tfidf[tfidf$fold != i, 1:1000],
                               y = tfidf[tfidf$fold != i, 1002],
                               ntree = df$ntree, mtry = df$mtry)
    
    auc_freq[i] <- predict(rf_mod_freq, type = "prob", newdata = training[training$fold == i, ])[, 2] %>%
      prediction(training[training$fold == i, 1001]) %>%
      performance("auc") %>%
      slot("y.values") %>%
      unlist()
    
    auc_idf[i] <- predict(rf_mod_idf, type = "prob", newdata = tfidf[tfidf$fold == i, ])[, 2] %>%
      prediction(tfidf[tfidf$fold == i, 1002]) %>%
      performance("auc") %>%
      slot("y.values") %>%
      unlist()
    
    print(i)
    
  }
  
  conditions$auc_freq[num_cond] <- mean(auc_freq)
  conditions$auc_idf[num_cond] <- mean(auc_idf)
  
  print(paste("Number", num_cond, "of", nrow(conditions), "found"))
  
}

conditions

# Fairly close, but ntree = 500 and mtry = sqrt(predictors) seems to work best (0.887)

# Run a LASSO model for both frequency counts and tf-idf weighting

auc_freq <- c(NA)
auc_idf <- c(NA)

for(i in 1:5){
  
  lambda_freq <- cv.glmnet(x = as.matrix(training[training$fold != i, 1:1000]),
                          y = training[training$fold != i, 1001],
                          alpha = 1, family = 'binomial')
  
  lasso_freq <- glmnet(x = as.matrix(training[training$fold != i, 1:1000]),
                      y = training[training$fold !=i, 1001],
                      alpha = 1, family = 'binomial',
                      lambda = lambda_freq$lambda.1se)
  
  auc_freq <- predict(lasso_freq,
                      type = "response",
                      s = 'lambda.min',
                      newx = as.matrix(training[training$fold == i, 1:1000])) %>%
    prediction(training[training$fold == i, 1001]) %>%
    performance("auc") %>%
    slot("y.values") %>%
    unlist()

  lambda_idf <- cv.glmnet(x = as.matrix(tfidf[tfidf$fold != i, 1:1000]),
                          y = tfidf[tfidf$fold != i, 1002],
                          alpha = 1, family = 'binomial')
  
  lasso_idf <- glmnet(x = as.matrix(tfidf[tfidf$fold != i, 1:1000]),
                      y = tfidf[tfidf$fold !=i, 1002],
                      alpha = 1, family = 'binomial',
                      lambda = lambda_idf$lambda.1se)
  
  auc_idf[i] <- predict(lasso_idf,
                    type = "response",
                    s = 'lambda.min',
                    newx = as.matrix(tfidf[tfidf$fold == i, 1:1000])) %>%
    prediction(tfidf[tfidf$fold == i, 1002]) %>%
    performance("auc") %>%
    slot("y.values") %>%
    unlist()
  
  print(i)
  
}

mean(auc_freq)
mean(auc_idf)

# LASSO model with counts rather than tf-idf works better.

##############################
# Perform final model
# validation for randomForest
# with standard defaults
# and a LASSO

rf_mod <- randomForest(x = training[, 1:1000],
                       y = training[, 1001])

rf_auc <- predict(rf_mod, type = "prob", newdata = testing[, 1:1000])[, 2] %>%
  prediction(testing[, 1001]) %>%
  performance("auc") %>%
  slot("y.values") %>%
  unlist()

# AUC for random forest with traditional defaults is 86

lasso_lambda <- cv.glmnet(x = as.matrix(training[, 1:1000]),
                          y = training[, 1001],
                          alpha = 1, family = 'binomial')

lasso_mod <- glmnet(x = as.matrix(training[, 1:1000]),
                    y = training[, 1001],
                    alpha = 1, family = 'binomial',
                    lambda = lasso_lambda$lambda.min)

lasso_auc <- predict(lasso_mod,
                     type = "response",
                     s = 'lambda.min',
                     newx = as.matrix(testing[, 1:1000])) %>%
  prediction(testing[, 1001]) %>%
  performance("auc") %>%
  slot("y.values") %>%
  unlist()

# AUC for lasso is 0.82

#########################
# Interpret final rf
# model, create plots,
# and calculate predicted
# probabilities to create
# health ratings

# Variable importance

rf_importance <- as.data.frame(rf_mod$importance) %>%
  mutate(food = row.names(.)) %>%
  arrange(desc(MeanDecreaseGini)) %>%
  head(n = 10)

names(rf_importance)[1] <- "Importance"
rf_importance$food <- factor(rf_importance$food, levels = rev(rf_importance$food))

pdf("Figure/rf_imp.pdf", width = 5, height = 5)
ggplot(aes(x = food, y = Importance), data = rf_importance) + 
  geom_point(size = 3) + 
  scale_y_continuous(breaks = 4:10) + 
  labs(x = "Food\n", y = "\nImportance") +
  coord_flip() + 
  theme_bw() + 
  ggtitle("Variable Importance\n") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        title = element_text(face = "bold", size = 16))
dev.off()

# Plot ROC curve

rf_roc <- predict(rf_mod, type = "prob", newdata = testing[, 1:1000])[, 2] %>%
  prediction(testing[, 1001]) %>%
  performance("tpr", "fpr")

pdf("Figure/auc_rf.pdf", width = 5, height = 5)
plot(rf_roc, main = "ROC Curve for Random Forest (Bag-of-Words)", col = 2, lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")
text(x = 0.1, y = 0.85, labels = paste("AUC =", round(rf_auc, 2)))
dev.off()

restaurant_data$new_pred <- predict(rf_mod, type = "prob", newdata = menu_sub)[, 2]

select(restaurant_data, name, new_pred) %>%
  arrange(desc(new_pred)) %>%
  head(n = 50)

select(restaurant_data, name, new_pred) %>%
  arrange(new_pred) %>%
  head(n = 50)

plot(density(restaurant_data$new_pred))
summary(restaurant_data$new_pred)

restaurant_data$health_color = as.character(cut(restaurant_data$new_pred,
                                    breaks = quantile(restaurant_data$new_pred, probs = c(0, 0.25, 0.75, 1.00)),
                                    include.lowest = TRUE,
                                    labels = c("red", "yellow", "green")))

# Include vegan, vegetarian-friendly, and gluten-free

restaurant_data$isVegan <- ifelse(grepl("vegan", restaurant_data$tags), 1, 0)
restaurant_data$isVegetarian <- ifelse(grepl("vegetarian", restaurant_data$tags), 1, 0)

select(restaurant_data, name, address, latitude, longitude, link,
       price, health_color, isVegan, isVegetarian) %>%
  write.csv("Data/restaurant_ratings.csv", row.names = FALSE)

#########################
# Validation using local
# blogs that include
# places that were on
# menupages

restaurant_data[2022, ]

# Ariana Restaurant: 97 - YELLOW (0.04)
# Root: 1578 - GREEN (0.752) 
# Lucy Ethiopian Cafe: 1139 - YELLOW (.08)
# Snappy Sushi: 1705 - YELLOW (.12)
# Trident: 2016 - GREEN (0.312)
# Erbaluce: 720 - YELLOW (0.056)
# Elephant Walk: 703 - YELLOW (.15)
# EVOO: 727 - GREEN (0.71)
# Life Alive: 1107 - GREEN (0.8)
# Oleana: 1322 - YELLOW (0.08)
# Blu: 267 - YELLOW (0.09)
# Post 390: 1478 - GREEN (0.24)
# Ten Tables x 2: 1842, 1843 - GREENx2 (0.69, 0.68)
# Vee Vee: 2053 - GREEN (0.68)
# True Bistro: 2022 - GREEN (0.72)
# Legal Harborside: 1099 - YELLOW (0.06)
# Stephi's in Southie: 1745 - YELLOW (0.05)
# Myer's + Chang: 1269 - GREEN (0.31)
# Teranga: 1845 - GREEN (0.23)

# 11 green
# 9 yellow

# Beat Hotel: 212 - YELLOW (0.20)
# b.good: 157 - GREEN (0.99)
# Life Alive: 1107 - GREEN (0.8)
# Mother Juice: 1258 - GREEN (0.61)
# Root: 1578 - GREEN (0.75) 
# True Bistro: 2022 - GREEN (0.72)

# 5 green, 1 yellow

# Healthiest chains

# ABP: 118 - GREEN (1.0)
# Brueggers: 363 - GREEN (.98)
# Cosi: 573 - GREEN (.99)
# Panera: 1358 - GREEN (.98)

# 4 green

# Total: 20 green, 10 yellow


