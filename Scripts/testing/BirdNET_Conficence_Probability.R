# BirdNET Confidence score transformation
# from:
# Symes, L., Sugai, L. S., Gottesman, B., Pitzrick, M., & Wood, C. (2023). Acoustic analysis with BirdNET and (almost) no coding: practical instructions (Version 0.9). Zenodo. https://doi.org/10.5281/zenodo.8357176


#### load BirdNET selection table ####
table <- file.choose()
data <- data.frame(read.table(table,
                   sep ="\t",
                   header = T))

# convert the FP test and TP test annotations to binomial (0/1) in a new column
data$Eval <- ifelse(data$"TP.test.FP.test" == "TP test", 1, 0)

#### logistic regression (linear) ####
model <- glm(Eval ~ Confidence,
             family = binomial(link = "logit"),
             data = data)

prediction_values <- seq(0.9, 1, 0.001)
predictions <- predict(model, list(Confidence = prediction_values), type = "r")

# plot predictions
plot(Eval~Confidence, data=data, xlim=c(0.9,1), pch=16,col=rgb(0,0,0,.3))
lines(predictions~prediction_values, lwd=4, col=rgb(1,0,0,.5))
# add fitted probabilities
fitted.probs <- predict(model, type="response")
lines(sort(data$Confidence), fitted.probs[order(data$Confidence)], col="red")

# check model estimates
summary(model)  # see coefficient estimates and significance
summary(data$Confidence)  # check range of confidence scores

# get Confidence score for desired probability of TP
p <- 0.9 # change as needed for desired TP probability
score_threshold <- (log(p/(1-p))-model$coefficients[1]) / model$coefficients[2]
score_threshold

# calculate probability of a TP for a specified confidence score
confidence_score <- 0.98
prob = 1/(1 + exp(-(model$coefficients[1] + model$coefficients[2]*confidence_score)))
prob

# Calculate model performance metrics
library(pROC)
roc_curve <- roc(data$Eval, fitted.probs)
auc(roc_curve)  # Area under ROC curve

#### logarithmic transformation ####
# Add a small value to avoid log(1)
epsilon <- 1e-10
data$conf_adj <- pmin(data$Confidence, 1 - epsilon)

# Create models with different transformations
model1 <- glm(Eval ~ Confidence, family=binomial(link="logit"), data=data)  # original
model2 <- glm(Eval ~ log(conf_adj), family=binomial(link="logit"), data=data)
model3 <- glm(Eval ~ log(-log(conf_adj)), family=binomial(link="logit"), data=data)

# Compare models
AIC(model1, model2, model3)

# Visual comparison
par(mfrow=c(1,3))

# Plot for original model
fitted1 <- predict(model1, type="response")
plot(data$Confidence, data$Eval, main="Original")
lines(sort(data$Confidence), fitted1[order(data$Confidence)], col="red")

# Plot for log model
fitted2 <- predict(model2, type="response")
plot(log(data$conf_adj), data$Eval, main="Log transform")
lines(sort(log(data$conf_adj)), fitted2[order(data$conf_adj)], col="red")

# Plot for log-log model
fitted3 <- predict(model3, type="response")
plot(log(-log(data$conf_adj)), data$Eval, main="Log-log transform")
lines(sort(log(-log(data$conf_adj))), fitted3[order(data$conf_adj)], col="red")

#### update model here with best fitting model ####
best <- model3

# check model estimates
summary(best)  # see coefficient estimates and significance
summary(data$Confidence)  # check range of confidence scores

# get Confidence score for desired probability of TP
p <- 0.9 # change as needed for desired TP probability
score_threshold <- (log(p/(1-p))-best$coefficients[1]) / best$coefficients[2]
score_threshold <- exp(-exp((log(p/(1-p)) - best$coefficients[1])/best$coefficients[2])) # for log-log
score_threshold

# calculate probability of a TP for a specified confidence score (linear)
confidence_score <- 0.98
prob = 1/(1 + exp(-(best$coefficients[1] + best$coefficients[2]*confidence_score)))
prob <- 1/(1 + exp(-(best$coefficients[1] + best$coefficients[2]*log(-log(confidence_score))))) # for log-log
prob



# Calculate model performance metrics
library(pROC)
roc_curve <- roc(data$Eval, fitted.probs)
plot(roc_curve)
auc(roc_curve)  # Area under ROC curve


# Original model
conf_seq <- seq(0.9, 0.9999, 0.0001)
pred_probs_orig <- 1/(1 + exp(-(model1$coefficients[1] +
                                  model1$coefficients[2]*conf_seq)))

# Log-log model
pred_probs_loglog <- 1/(1 + exp(-(best$coefficients[1] +
                                    best$coefficients[2]*log(-log(conf_seq)))))

# Create comparison plot
plot(conf_seq, pred_probs_orig, type='l', col='blue',
     xlab='Confidence Score',
     ylab='Predicted Probability of True Positive',
     main='Model Comparison')
lines(conf_seq, pred_probs_loglog, col='red')
legend("topleft",
       legend=c("Original Model", "Log-log Model"),
       col=c("blue", "red"),
       lty=1)

# Print max probabilities from each model
cat("Max probability (original model):", max(pred_probs_orig), "\n")
cat("Max probability (log-log model):", max(pred_probs_loglog), "\n")

# Find confidence thresholds for different probability levels
thresh_df <- data.frame(
  confidence = conf_seq,
  pred_prob_loglog = pred_probs_loglog
)

# Look at thresholds for different probability levels
print("Confidence scores needed for:")
print("5% probability:")
print(min(thresh_df$confidence[thresh_df$pred_prob_loglog > 0.05]))
print("7.5% probability:")
print(min(thresh_df$confidence[thresh_df$pred_prob_loglog > 0.075]))
print("10% probability:")
print(min(thresh_df$confidence[thresh_df$pred_prob_loglog > 0.10]))

