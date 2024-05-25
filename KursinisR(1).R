library(ggplot2)
library(survival)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(regclass)
library(QuantPsyc)
library(caret)
library(sandwich)
library(lmtest)
library(car)
library(pROC)
library(survMisc)
Kortele$issilavinimas[Kortele$issilavinimas == "duomenų nepateikia"] <- NA
Kortele$uzimtumas[Kortele$uzimtumas == "duomenų nepateikia"] <- NA
Kortele$israsytas[is.na(Kortele$israsytas)] <- "išrašytas nebuvo"
Kortele$israsytas[Kortele$israsytas == "iÅkeltas ÄÆ kitas stacionarines asmens sveikatos prieÅ¾iÅ«ros, reabilitacijos ÄÆstaigas"] <- "iškeltas į kitas stacionarines asmens sveikatos priežiūros, reabilitacijos įstaigas"
Kortele$israsytas[Kortele$israsytas == "perkeltas ÄÆ psichosocialinio profilio stacionarinius skyrius RPLC"] <- "perkeltas į psichosocialinio profilio stacionarinius skyrius RPLC"
df <- Kortele[complete.cases(Kortele$sveikatos_draudimas) &
                complete.cases(Kortele$uzimtumas) &
                complete.cases(Kortele$amzius_gr) &
                complete.cases(Kortele$gyvenamoji_vieta) &
                complete.cases(Kortele$issilavinimas) &
                complete.cases(Kortele$vizitas) &
                complete.cases(Kortele$israsyto_paciento_bukle), ]
df$israsyto_paciento_bukle <- ifelse(df$israsyto_paciento_bukle == "sėkmingai baigė gydymą", 1, 0)
df$lytis <- ifelse(df$lytis == "vyras", 1, 0)
df$uzimtumas <- ifelse(df$uzimtumas == "dirba", 1, 0)
df$vizitas <- ifelse(df$vizitas == "pirminis", 0, 1)
df$sutrikimas <- substr(df$sutrikimas, 1, 3)

table(df$lytis)
table(df$sveikatos_draudimas)
table(df$vizitas)
table(df$israsyto_paciento_bukle)

ggplot(df, aes(x = sutrikimas)) + geom_bar(fill = "blue") + geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
labs(title = "Pacientų priklausomybių dažnis", x = "Sutrikimas", y = "Dažnis")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = amzius_gr)) + geom_bar(fill = "blue") + geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Pacientų amžiaus grupių pasiskirstymas", x = "Amžiaus grupė", y = "Dažnis")+
  theme(plot.title = element_text(hjust = 0.5))

df$ataskaitiniai_metai <- factor(df$ataskaitiniai_metai, levels = 2016:2022)
ggplot(df, aes(x = ataskaitiniai_metai)) +
  geom_bar(fill = "blue") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Pacientų ataskaitinių metų pasiskirstymas", x = "Metai", y = "Dažnis") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = gyvenamoji_vieta)) + 
  geom_bar(fill = "blue") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Pacientų gyvenamosios vietos pasiskirstymas", x = "Gyvenamoji vieta", y = "Dažnis") +
  theme(plot.title = element_text(hjust = 0.5))  
  
ggplot(df, aes(x = issilavinimas)) + 
  geom_bar(fill = "blue") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Pacientų įgyto išsilavinimo pasiskirstymas", x = "Išsilavinimas", y = "Dažnis") +
  theme(plot.title = element_text(hjust = 0.5))   

ggplot(df, aes(x = taikytas_gydymas)) + 
  geom_bar(fill = "blue") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Pacientui taikyto gydymo pasiskirstymas", x = "Taikytas gydymas", y = "Dažnis") +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(df, aes(x = uzimtumas)) + 
  geom_bar(fill = "blue") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Pacientų užimtumo pasiskirstymas", x = "Užimtumas", y = "Dažnis") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = israsytas)) + 
  geom_bar(fill = "blue") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Pacientų išrašymo pasiskirstymas", x = "išrašymo vieta", y = "Dažnis") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df, aes(x = sutrikimas)) + 
  geom_bar(fill = "blue") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Pacientų priklausomybių dažnis", x = "Sutrikimas", y = "Dažnis") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


library(ggplot2)
library(dplyr)



# Create a ggplot object
p <- ggplot(df, aes(x = amzius_gr)) + 
  geom_bar(fill = "blue") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Pacientų amžiaus grupių pasiskirstymas", x = "Amžiaus grupė", y = "Dažnis") +
  theme(plot.title = element_text(hjust = 0.5))

# Add a line representing normal distribution
p + stat_function(fun = dnorm, args = list(mean = mean(df$amzius_gr), sd = mean(df$amzius_gr)), color = "red", size = 1)
df$amzius_gr <- ifelse(grepl("20-24", df$amzius_gr), 1,
                       ifelse(grepl("25-29", df$amzius_gr), 2,
                              ifelse(grepl("30-34", df$amzius_gr), 3,
                                     ifelse(grepl("35-39", df$amzius_gr), 4,
                                            ifelse(grepl("40-44", df$amzius_gr), 5,
                                                   ifelse(grepl("45-49", df$amzius_gr), 6,
                                                          ifelse(grepl("50-54", df$amzius_gr), 7,
                                                                 ifelse(grepl("55-59", df$amzius_gr), 8,
                                                                        ifelse(grepl("60-64", df$amzius_gr), 9,
                                                                               ifelse(grepl("65-69", df$amzius_gr), 10,
                                                                                      11  # 70+
                                                                               )
                                                                        )
                                                                 )
                                                          )
                                                   )
                                            )
                                     )
                              )
                       )
)

df$issilavinimas <- ifelse(grepl("pradinis \\(1-4kl\\.\\)", df$issilavinimas), 1,
                           ifelse(grepl("pagrindinis \\(9-10 kl\\.\\)", df$issilavinimas), 2,
                                  ifelse(grepl("spec\\. vidurinis \\(profesinis\\)", df$issilavinimas), 3,
                                         ifelse(grepl("vidurinis \\(12 kl\\.\\)", df$issilavinimas), 4,
                                                ifelse(grepl("aukštesnysis", df$issilavinimas), 5,
                                                       6
                                                )
                                         )
                                  )
                           )
)

logistic_model <- glm(israsyto_paciento_bukle ~ ataskaitiniai_metai + lytis + amzius_gr + gyvenamoji_vieta + 
                        issilavinimas + sveikatos_draudimas + uzimtumas + sutrikimas + vizitas + taikytas_gydymas, 
                      data = df, 
                      family = binomial)


# Summarize the model
summary(logistic_model)


##############logit#################
set.seed(246)
train_ratio <- 0.8
train_indices <- createDataPartition(df$israsyto_paciento_bukle, p = train_ratio, list = FALSE)
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

logistic_model1 <- glm(israsyto_paciento_bukle ~ ataskaitiniai_metai + lytis + 
                         amzius_gr + gyvenamoji_vieta + 
                         issilavinimas + sveikatos_draudimas + 
                         uzimtumas + sutrikimas + vizitas + taikytas_gydymas, 
                       data = train_data, 
                       family = binomial)
summary(logistic_model1)

model_logit_rez_final <- glm(israsyto_paciento_bukle ~ amzius_gr +
                               uzimtumas + sutrikimas
                              ,
                             family = binomial(logit), data = train_data)

summary(model_logit_rez_final)

wald_result_logit <- waldtest(model_logit_rez_final, vcov = vcovHC(model_logit_rez_final, type = "HC1"))
wald_result_logit

cooksd <- cooks.distance(model_logit_rez_final)
plot(cooksd, pch="*", xlab = "Paciento indekas", ylab = "Kuko atstumas", cex=2, main="Kuko matas")
outliers <- cooksd[cooksd>1]
outliers

mdl.reduced <-glm(israsyto_paciento_bukle~1, family=binomial,data=train_data)
anova(mdl.reduced, model_logit_rez_final, test="Chisq")
rkvadrat<-1-model_logit_rez_final$deviance/model_logit_rez_final$null.deviance
rkvadrat
library(QuantPsyc)
ClassLog(model_logit_rez_final, train_data$israsyto_paciento_bukle)

#############probit#################
#Saveika tarp gydymo ir issilavinimo
model_probit <- glm(formula = israsyto_paciento_bukle ~.,
                    family = binomial(probit), data = train_data)
summary(model_probit)

model_probit_rez_final <- glm(israsyto_paciento_bukle ~ amzius_gr + 
                               uzimtumas + sutrikimas, 
                             family = binomial(probit), data = train_data)

summary(model_probit_rez_final)
wald_result_probit <- waldtest(model_probit_rez_final, vcov = vcovHC(model_probit_rez_final, type = "HC1"))
wald_result_probit

cooksd <- cooks.distance(model_probit_rez_final)
plot(cooksd, pch="*", xlab = "Paciento indeksas", ylab = "Kuko atstumas", cex=2, main="Kuko matas")
outliers <- cooksd[cooksd>1]
outliers

mdl.reduced <-glm(israsyto_paciento_bukle~1, family=binomial,data=train_data)
anova(mdl.reduced, model_probit_rez_final, test="Chisq")

library(QuantPsyc)
ClassLog(model_probit_rez_final, train_data$israsyto_paciento_bukle)

###################testai#################
# Exponentiated coefficients ("odds ratios")
exp(coef(model_logit_rez_final)) 

# Intervals
exp(confint.default(model_logit_rez_final, level=0.9))

# ROC
lr_prediction <- predict(model_logit_rez_final,
                         train_data, type = "response")

ROC_lr <- roc(train_data$israsyto_paciento_bukle, lr_prediction)
ROC_lr_auc <- auc(ROC_lr)
plot(ROC_lr, col = "green", main = "Logistinės regresijos ROC kreivė", xlab = "Specifiškumas", ylab = "Jautrumas")

text(0.4, 0.6, paste("AUC =", round(auc(ROC_lr), 2)), col = "blue")


# TESTAI
predicted_probabilities <- predict(model_logit_rez_final, type = "response", newdata = test_data)

predicted_labels <- ifelse(predicted_probabilities >= 0.5, 1, 0)

test_data$predicted_probabilities <- predicted_probabilities
test_data$predicted_labels <- predicted_labels

confusion_matrix <- table(test_data$israsyto_paciento_bukle, test_data$predicted_labels)

rownames(confusion_matrix) <- c("pasveiko", "nepasveiko")
colnames(confusion_matrix) <- c("spėta kad pasveiko", "spėta kad nepasveiko")

print(confusion_matrix)





################weighted#########
weights <- ifelse(train_data$israsyto_paciento_bukle == 1, 
                  sum(train_data$israsyto_paciento_bukle == 0) / sum(train_data$israsyto_paciento_bukle == 1), 
                  1)


weighted_logistic_model <- glm(israsyto_paciento_bukle ~ uzimtumas + sutrikimas +
  amzius_gr, data = train_data,  family = binomial(logit), weights = weights)

summary(weighted_logistic_model)
ClassLog(weighted_logistic_model, train_data$israsyto_paciento_bukle)

wald_result_probit <- waldtest(weighted_logistic_model, vcov = vcovHC(weighted_logistic_model, type = "HC1"))
wald_result_probit

cooksd <- cooks.distance(weighted_logistic_model)
plot(cooksd, pch="*", xlab = "Paciento indeksas", ylab = "Kuko atstumas", cex=2, main="Kuko matas")
outliers <- cooksd[cooksd>1]
outliers

mdl.reduced <-glm(israsyto_paciento_bukle~1, family=binomial, data=train_data)
anova(mdl.reduced, weighted_logistic_model, test="Chisq")



weighted_probit_model <- glm(israsyto_paciento_bukle ~ uzimtumas + sutrikimas +
                                 amzius_gr
                               , 
                               data = train_data, 
                               family = binomial(probit), 
                               weights = weights)

summary(weighted_probit_model)

wald_result_probit <- waldtest(weighted_probit_model, vcov = vcovHC(weighted_probit_model, type = "HC1"))
wald_result_probit

cooksd <- cooks.distance(weighted_probit_model)
plot(cooksd, pch="*", xlab = "Paciento indeksas", ylab = "Kuko atstumas", cex=2, main="Kuko matas")
outliers <- cooksd[cooksd>1]
outliers

mdl.reduced <-glm(israsyto_paciento_bukle~1, family=binomial, data=train_data)
anova(mdl.reduced, weighted_probit_model, test="Chisq")

ClassLog(weighted_probit_model, train_data$israsyto_paciento_bukle)
#####################################

# Exponentiated coefficients ("odds ratios")
exp(coef(weighted_logistic_model)) 

# Intervals
exp(confint.default(weighted_logistic_model, level=0.9))

# ROC
lr_prediction <- predict(weighted_logistic_model,
                         train_data, type = "response")

ROC_lr <- roc(train_data$israsyto_paciento_bukle, lr_prediction)
ROC_lr_auc <- auc(ROC_lr)
plot(ROC_lr, col = "green", main = "Logistinės regresijos ROC kreivė", xlab = "Specifiškumas", ylab = "Jautrumas")

text(0.4, 0.6, paste("AUC =", round(auc(ROC_lr), 2)), col = "blue")


# TESTAI
predicted_probabilities <- predict(weighted_logistic_model, type = "response", newdata = test_data)

predicted_labels <- ifelse(predicted_probabilities >= 0.5, 1, 0)

test_data$predicted_probabilities <- predicted_probabilities
test_data$predicted_labels <- predicted_labels

confusion_matrix <- table(test_data$israsyto_paciento_bukle, test_data$predicted_labels)

rownames(confusion_matrix) <- c("pasveiko", "nepasveiko")
colnames(confusion_matrix) <- c("spėta kad pasveiko", "spėta kad nepasveiko")

print(confusion_matrix)

#Su svoriais ir saveikomis

weighted_logistic_model_s <- glm(israsyto_paciento_bukle ~ uzimtumas + sutrikimas +
                                 amzius_gr + taikytas_gydymas:issilavinimas, data = train_data,  family = binomial(logit), weights = weights)

summary(weighted_logistic_model_s)
ClassLog(weighted_logistic_model_s, train_data$israsyto_paciento_bukle)

wald_result_probit <- waldtest(weighted_logistic_model_s, vcov = vcovHC(weighted_logistic_model_s, type = "HC1"))
wald_result_probit

cooksd <- cooks.distance(weighted_logistic_model_s)
plot(cooksd, pch="*", xlab = "Paciento indeksas", ylab = "Kuko atstumas", cex=2, main="Kuko matas")
outliers <- cooksd[cooksd>1]
outliers

mdl.reduced <-glm(israsyto_paciento_bukle~1, family=binomial, data=train_data)
anova(mdl.reduced, weighted_logistic_model_s, test="Chisq")


#Probit su svoriais ir saveikomis
weighted_probit_model_s <- glm(israsyto_paciento_bukle ~ uzimtumas + sutrikimas +
                               amzius_gr + taikytas_gydymas:issilavinimas
                             , 
                             data = train_data, 
                             family = binomial(probit), 
                             weights = weights)

summary(weighted_probit_model_s)

wald_result_probit <- waldtest(weighted_probit_model_s, vcov = vcovHC(weighted_probit_model_s, type = "HC1"))
wald_result_probit

cooksd <- cooks.distance(weighted_probit_model_s)
plot(cooksd, pch="*", xlab = "Paciento indeksas", ylab = "Kuko atstumas", cex=2, main="Kuko matas")
outliers <- cooksd[cooksd>1]
outliers

mdl.reduced <-glm(israsyto_paciento_bukle~1, family=binomial, data=train_data)
anova(mdl.reduced, weighted_probit_model_s, test="Chisq")

ClassLog(weighted_probit_model_s, train_data$israsyto_paciento_bukle)
#####################################

# Exponentiated coefficients ("odds ratios")
exp(coef(weighted_logistic_model_s)) 

# Intervals
exp(confint.default(weighted_logistic_model_s, level=0.9))

# ROC
lr_prediction <- predict(weighted_logistic_model_s,
                         train_data, type = "response")

ROC_lr <- roc(train_data$israsyto_paciento_bukle, lr_prediction)
ROC_lr_auc <- auc(ROC_lr)
plot(ROC_lr, col = "green", main = "Logistinės regresijos ROC kreivė", xlab = "Specifiškumas", ylab = "Jautrumas")

text(0.4, 0.6, paste("AUC =", round(auc(ROC_lr), 2)), col = "blue")


# TESTAI
predicted_probabilities <- predict(weighted_logistic_model_s, type = "response", newdata = test_data)

predicted_labels <- ifelse(predicted_probabilities >= 0.5, 1, 0)

test_data$predicted_probabilities <- predicted_probabilities
test_data$predicted_labels <- predicted_labels

confusion_matrix <- table(test_data$israsyto_paciento_bukle, test_data$predicted_labels)

rownames(confusion_matrix) <- c("pasveiko", "nepasveiko")
colnames(confusion_matrix) <- c("spėta kad pasveiko", "spėta kad nepasveiko")

print(confusion_matrix)





















# Load necessary libraries
library(xgboost)
library(Matrix)
library(pROC)

# One-hot encode categorical variables
train_matrix <- model.matrix(israsyto_paciento_bukle ~ .- 1, data = train_data)

# Ensure the dependent variable is numeric
train_labels <- as.numeric(train_data$israsyto_paciento_bukle)

# Create xgb.DMatrix
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)

# Calculate scale_pos_weight
scale_pos_weight <- sum(train_data$israsyto_paciento_bukle == 0) / sum(train_data$israsyto_paciento_bukle == 1)

# Set parameters
params <- list(
  objective = "binary:logistic",
  scale_pos_weight = scale_pos_weight,
  eval_metric = "auc"
)

# Train the model
xgb_model <- xgb.train(params, dtrain, nrounds = 100, verbose = 1)

# Make predictions
pred <- predict(xgb_model, dtrain)

# Convert predictions to binary output
pred_binary <- ifelse(pred > 0.5, 1, 0)

# Calculate confusion matrix
confusion_matrix <- table(Predicted = pred_binary, Actual = train_labels)

# Print confusion matrix
print(confusion_matrix)

# Calculate AUC
roc_obj <- roc(train_labels, pred)
auc_value <- auc(roc_obj)
print(auc_value)
plot(roc_obj, col = "green", main = "XGBoost modelio ROC kreivė", xlab = "Specifiškumas", ylab = "Jautrumas")

# Get and print feature importance
importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model)
print(importance_matrix)

# Plot feature importance (optional)
xgb.plot.importance(importance_matrix)

library(SHAPforxgboost)
library(DiagrammeR)
xgb.plot.tree(model = xgb_model, trees = 0)





#Dokumentacijos pasibandymas
set.seed(246)
train_ratio <- 0.8
train_indices <- createDataPartition(df$israsyto_paciento_bukle, p = train_ratio, list = FALSE)
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]
train_matrix <- model.matrix(israsyto_paciento_bukle ~ .- 1, data = train_data)

# Ensure the dependent variable is numeric
train_labels <- as.numeric(train_data$israsyto_paciento_bukle)

# Create xgb.DMatrix
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)

# Calculate scale_pos_weight
scale_pos_weight <- sum(train_data$israsyto_paciento_bukle == 0) / sum(train_data$israsyto_paciento_bukle == 1)

# Set parameters
params <- list(
  objective = "binary:logistic",
  scale_pos_weight = scale_pos_weight,
  eval_metric = "auc"
)

# Train the model
xgb_model <- xgb.train(params, dtrain, nrounds = 100, verbose = 1)

# Make predictions
pred <- predict(xgb_model, dtrain)

# Convert predictions to binary output
pred_binary <- ifelse(pred > 0.5, 1, 0)

# Calculate confusion matrix
confusion_matrix <- table(Predicted = pred_binary, Actual = train_labels)

# Print confusion matrix
print(confusion_matrix)

# Calculate AUC
roc_obj <- roc(train_labels, pred)
auc_value <- auc(roc_obj)
print(auc_value)
plot(roc_obj, col = "green", main = "XGBoost modelio ROC kreivė", xlab = "Specifiškumas", ylab = "Jautrumas")

# Get and print feature importance
importance_matrix <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model)
print(importance_matrix)

# Plot feature importance (optional)
xgb.plot.importance(importance_matrix)

#One-hot encode categorical variables for test data
test_matrix <- model.matrix(israsyto_paciento_bukle ~ .- 1, data = test_data)


#Ensure the dependent variable is numeric
test_labels <- as.numeric(test_data$israsyto_paciento_bukle)

#Create xgb.DMatrix for test data
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)


#Make predictions on the test data
test_preds <- predict(xgb_model, dtest)

#Convert probabilities to binary predictions
test_preds_binary <- ifelse(test_preds > 0.5, 1, 0)

#If you know the actual labels of the test data
test_labels <- as.numeric(test_data$israsyto_paciento_bukle)

#Calculate confusion matrix
test_confusion_matrix <- table(Predicted = test_preds_binary, Actual = test_labels)
print(test_confusion_matrix)

roc_obj1 <- roc(test_labels, test_preds)
auc_value1 <- auc(roc_obj1)
print(auc_value1)
plot(roc_obj1, col = "green", main = "XGBoost modelio ROC kreivė", xlab = "Specifiškumas", ylab = "Jautrumas")

