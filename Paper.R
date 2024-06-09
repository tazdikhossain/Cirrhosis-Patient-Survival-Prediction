cirrhosis_data<-read.csv("C:/cirrhosis.csv", header=TRUE, sep=",")
cirrhosis_data

View(cirrhosis_data)

colSums(is.na(cirrhosis_data))

cirrhosis_data$Albumin <- as.numeric(as.character(cirrhosis_data$Albumin))
hist(cirrhosis_data$Albumin, main = "Histogram of Albumin", xlab = "Albumin", ylab = "Frequency")

cirrhosis_data$Copper <- as.numeric(as.character(cirrhosis_data$Copper))
hist(cirrhosis_data$Copper, main = "Histogram of Copper", xlab = "Copper", ylab = "Frequency")


cirrhosis_data$Alk_Phos <- as.numeric(as.character(cirrhosis_data$Alk_Phos))
hist(cirrhosis_data$Alk_Phos, main = "Histogram of Alk_Phos", xlab = "Alk_Phos", ylab = "Frequency")

cirrhosis_data$SGOT <- as.numeric(as.character(cirrhosis_data$SGOT))
hist(cirrhosis_data$SGOT, main = "Histogram of SGOT", xlab = "SGOT", ylab = "Frequency")

cirrhosis_data$Tryglicerides <- as.numeric(as.character(cirrhosis_data$Tryglicerides))
hist(cirrhosis_data$Tryglicerides, main = "Histogram of Tryglicerides", xlab = "Tryglicerides", ylab = "Frequency")

cirrhosis_data$Platelets <- as.numeric(as.character(cirrhosis_data$Platelets))
hist(cirrhosis_data$Platelets, main = "Histogram of Platelets", xlab = "Platelets", ylab = "Frequency")

cirrhosis_data$Prothrombin <- as.numeric(as.character(cirrhosis_data$Prothrombin))
hist(cirrhosis_data$Prothrombin, main = "Histogram of Prothrombin", xlab = "Prothrombin", ylab = "Frequency")

install.packages("ggplot2")
library(ggplot2)

# Assuming your data frame is named 'cirrhosis_data'

# Create the bar plot
ggplot(cirrhosis_data, aes(x = Drug)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Drug Attribute", x = "Drug", y = "Count") +
  theme_minimal()

ggplot(cirrhosis_data, aes(x = Sex)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Sex Attribute", x = "Sex", y = "Count") +
  theme_minimal()

ggplot(cirrhosis_data, aes(x = Status)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Status Attribute", x = "Status", y = "Count") +
  theme_minimal()

ggplot(cirrhosis_data, aes(x = Ascites)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Ascites Attribute", x = "Ascites", y = "Count") +
  theme_minimal()

ggplot(cirrhosis_data, aes(x = Hepatomegaly)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Hepatomegaly", x = "Hepatomegaly", y = "Count") +
  theme_minimal()

ggplot(cirrhosis_data, aes(x = Spiders)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Spiders", x = "Spiders", y = "Count") +
  theme_minimal()

ggplot(cirrhosis_data, aes(x = Spiders)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Spiders", x = "Spiders", y = "Count") +
  theme_minimal()

ggplot(cirrhosis_data, aes(x = Edema)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Edema", x = "Edema", y = "Count") +
  theme_minimal()

ggplot(cirrhosis_data, aes(x = Stage)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Stage", x = "Stage", y = "Count") +
  theme_minimal()

boxplot(cirrhosis_data$N_Days, main = "boxplot of N days")
# Create a boxplot of N_Days


boxplot(cirrhosis_data$Bilirubin, main = "Boxplot of Bilirubin ")

boxplot(cirrhosis_data$Cholesterol, main = "Boxplot of Cholesterol ")

boxplot(cirrhosis_data$Albumin, main = "Boxplot of Albumin ")

boxplot(cirrhosis_data$Copper, main = "Boxplot of Copper ")

boxplot(cirrhosis_data$Alk_Phos, main = "Boxplot of Alk_Phos ")

boxplot(cirrhosis_data$SGOT, main = "Boxplot of SGOT ")

boxplot(cirrhosis_data$Tryglicerides, main = "Boxplot of Tryglicerides ")

boxplot(cirrhosis_data$Platelets, main = "Boxplot of Platelets ")

boxplot(cirrhosis_data$Prothrombin, main = "Boxplot of Prothrombin")

boxplot(cirrhosis_data$Stage, main = "Boxplot of Stage")




library(DescTools)
mode_value <- Mode(cirrhosis_data$Drug, na.rm = TRUE)
cirrhosis_data$Drug[is.na(cirrhosis_data$Drug)] <- mode_value
print(mode_value)


colSums(is.na(cirrhosis_data))


library(stats)
cirrhosis_data$Status <- factor(cirrhosis_data$Status, levels = c("D", "C", "CL"))
contingency_table <- table(cirrhosis_data$Drug, cirrhosis_data$Status)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)



mode_value <- Mode(cirrhosis_data$Ascites, na.rm = TRUE)
cirrhosis_data$Ascites[is.na(cirrhosis_data$Ascites)] <- mode_value
print(mode_value)

colSums(is.na(cirrhosis_data))


library(stats)
cirrhosis_data$Status <- factor(cirrhosis_data$Status, levels = c("D", "C", "CL"))
contingency_table <- table(cirrhosis_data$Ascites, cirrhosis_data$Status)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)



mode_value <- Mode(cirrhosis_data$Hepatomegaly, na.rm = TRUE)
cirrhosis_data$Hepatomegaly[is.na(cirrhosis_data$Hepatomegaly)] <- mode_value
print(mode_value)

colSums(is.na(cirrhosis_data))


library(stats)
cirrhosis_data$Status <- factor(cirrhosis_data$Status, levels = c("D", "C", "CL"))
contingency_table <- table(cirrhosis_data$Hepatomegaly, cirrhosis_data$Status)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)



mode_value <- Mode(cirrhosis_data$Spiders, na.rm = TRUE)
cirrhosis_data$Spiders[is.na(cirrhosis_data$Spiders)] <- mode_value
print(mode_value)

colSums(is.na(cirrhosis_data))


library(stats)
cirrhosis_data$Status <- factor(cirrhosis_data$Status, levels = c("D", "C", "CL"))
contingency_table <- table(cirrhosis_data$Spiders, cirrhosis_data$Status)
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)





mean_value <- mean(cirrhosis_data$Cholesterol, na.rm = TRUE)
cirrhosis_data$Cholesterol[is.na(cirrhosis_data$Cholesterol)] <- mean_value
print(mean_value)

colSums(is.na(cirrhosis_data))


library(stats)
anova_result <- aov(Cholesterol ~ Status, data = cirrhosis_data)
print(summary(anova_result))




mean_value <- mean(cirrhosis_data$Copper, na.rm = TRUE)
cirrhosis_data$Copper[is.na(cirrhosis_data$Copper)] <- mean_value
print(mean_value)


boxplot(cirrhosis_data$Copper, main = "Boxplot of Copper", ylab = "Copper")




library(ggplot2)
ggplot(cirrhosis_data, aes(x = factor(Status), y = Copper, fill = factor(Status))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count", trim = FALSE) +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  labs(title = "Violin Plot of Copper by Status", x = "Status", y = "Copper") +
  theme_minimal()


median_value <- median(cirrhosis_data$Copper, na.rm = TRUE)
cirrhosis_data$Copper[cirrhosis_data$Copper > quantile(cirrhosis_data$Copper, 0.75) + 1.5 * IQR(cirrhosis_data$Copper) | cirrhosis_data$Copper < quantile(cirrhosis_data$Copper, 0.25) - 1.5 * IQR(cirrhosis_data$Copper)] <- median_value
boxplot(cirrhosis_data$Copper, main = "Boxplot of Copper (Outliers Replaced by Median)", ylab = "Copper")

colSums(is.na(cirrhosis_data))


library(stats)
anova_result <- aov(Copper ~ Status, data = cirrhosis_data)
print(summary(anova_result))





median_value <- mean(cirrhosis_data$Alk_Phos, na.rm = TRUE)
cirrhosis_data$Alk_Phos[is.na(cirrhosis_data$Alk_Phos)] <- median_value
print(median_value)


boxplot(cirrhosis_data$Alk_Phos, main = "Boxplot of Alk_Phos", ylab = "Alk_Phos")


library(ggplot2)
ggplot(cirrhosis_data, aes(x = factor(Status), y = Alk_Phos, fill = factor(Status))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count", trim = FALSE) +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  labs(title = "Violin Plot of Alk_Phos by Status", x = "Status", y = "Alk_Phos") +
  theme_minimal()


median_value <- median(cirrhosis_data$Alk_Phos, na.rm = TRUE)
cirrhosis_data$Alk_Phos[cirrhosis_data$Alk_Phos > quantile(cirrhosis_data$Alk_Phos, 0.75) + 1.5 * IQR(cirrhosis_data$Alk_Phos) | cirrhosis_data$Alk_Phos < quantile(cirrhosis_data$Alk_Phos, 0.25) - 1.5 * IQR(cirrhosis_data$Alk_Phos)] <- median_value
boxplot(cirrhosis_data$Alk_Phos, main = "Boxplot of Alk_Phos (Outliers Replaced by Median)", ylab = "Alk_Phos")

colSums(is.na(cirrhosis_data))


library(stats)
anova_result <- aov(Alk_Phos ~ Status, data = cirrhosis_data)
print(summary(anova_result))





mean_value <- mean(cirrhosis_data$SGOT, na.rm = TRUE)
cirrhosis_data$SGOT[is.na(cirrhosis_data$SGOT)] <- mean_value


boxplot(cirrhosis_data$SGOT, main = "Boxplot of SGOT", ylab = "SGOT")



library(ggplot2)
ggplot(cirrhosis_data, aes(x = factor(Status), y = SGOT, fill = factor(Status))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count", trim = FALSE) +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  labs(title = "Violin Plot of SGOT by Status", x = "Status", y = "SGOT") +
  theme_minimal()


mean_value <- mean(cirrhosis_data$SGOT, na.rm = TRUE)
cirrhosis_data$SGOT[cirrhosis_data$SGOT > quantile(cirrhosis_data$SGOT, 0.75) + 1.5 * IQR(cirrhosis_data$SGOT) | cirrhosis_data$SGOT < quantile(cirrhosis_data$SGOT, 0.25) - 1.5 * IQR(cirrhosis_data$SGOT)] <- mean_value
boxplot(cirrhosis_data$SGOT, main = "Boxplot of SGOT (Outliers Replaced by Mean)", ylab = "SGOT")

colSums(is.na(cirrhosis_data))


library(stats)
anova_result <- aov(SGOT ~ Status, data = cirrhosis_data)
print(summary(anova_result))



mean_value <- mean(cirrhosis_data$Tryglicerides, na.rm = TRUE)
cirrhosis_data$Tryglicerides[is.na(cirrhosis_data$Tryglicerides)] <- mean_value


boxplot(cirrhosis_data$Tryglicerides, main = "Boxplot of Tryglicerides", ylab = "Tryglicerides")



library(ggplot2)
ggplot(cirrhosis_data, aes(x = factor(Status), y = Tryglicerides, fill = factor(Status))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count", trim = FALSE) +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  labs(title = "Violin Plot of Tryglicerides by Status", x = "Status", y = "Tryglicerides") +
  theme_minimal()


median_value <- median(cirrhosis_data$Tryglicerides, na.rm = TRUE)
cirrhosis_data$Tryglicerides[cirrhosis_data$Tryglicerides > quantile(cirrhosis_data$Tryglicerides, 0.75) + 1.5 * IQR(cirrhosis_data$Tryglicerides) | cirrhosis_data$Tryglicerides < quantile(cirrhosis_data$Tryglicerides, 0.25) - 1.5 * IQR(cirrhosis_data$Tryglicerides)] <- median_value
boxplot(cirrhosis_data$Tryglicerides, main = "Boxplot of Tryglicerides (Outliers Replaced by Median)", ylab = "Tryglicerides")

colSums(is.na(cirrhosis_data))


library(stats)
anova_result <- aov(Tryglicerides ~ Status, data = cirrhosis_data)
print(summary(anova_result))



median_value <- median(cirrhosis_data$Platelets, na.rm = TRUE)
cirrhosis_data$Platelets[is.na(cirrhosis_data$Platelets)] <- median_value
print(median_value)

boxplot(cirrhosis_data$Platelets, main = "Boxplot of Platelets", ylab = "Platelets")


library(ggplot2)
ggplot(cirrhosis_data, aes(x = factor(Status), y = Platelets, fill = factor(Status))) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "count", trim = FALSE) +
  geom_boxplot(width=0.1, fill="white", outlier.shape = NA) +
  labs(title = "Violin Plot of Platelets by Status", x = "Status", y = "Platelets") +
  theme_minimal()


median_value <- median(cirrhosis_data$Platelets, na.rm = TRUE)
cirrhosis_data$Platelets[cirrhosis_data$Platelets > quantile(cirrhosis_data$Platelets, 0.75) + 1.5 * IQR(cirrhosis_data$Platelets) | cirrhosis_data$Platelets < quantile(cirrhosis_data$Platelets, 0.25) - 1.5 * IQR(cirrhosis_data$Platelets)] <- median_value
boxplot(cirrhosis_data$Platelets, main = "Boxplot of Platelets (Outliers Replaced by Median)", ylab = "Platelets")

colSums(is.na(cirrhosis_data))


library(stats)
anova_result <- aov(Platelets ~ Status, data = cirrhosis_data)
print(summary(anova_result))



median_value <- median(cirrhosis_data$Prothrombin, na.rm = TRUE)
cirrhosis_data$Prothrombin[is.na(cirrhosis_data$Prothrombin)] <- median_value
print(median_value)


boxplot(cirrhosis_data$Prothrombin, main = "Boxplot of Prothrombin", ylab = "Prothrombin")


median_value <- median(cirrhosis_data$Prothrombin, na.rm = TRUE)
cirrhosis_data$Prothrombin[cirrhosis_data$Prothrombin > quantile(cirrhosis_data$Prothrombin, 0.75) + 1.5 * IQR(cirrhosis_data$Prothrombin) | cirrhosis_data$Prothrombin < quantile(cirrhosis_data$Prothrombin, 0.25) - 1.5 * IQR(cirrhosis_data$Prothrombin)] <- median_value
boxplot(cirrhosis_data$Prothrombin, main = "Boxplot of Prothrombin (Outliers Replaced by Median)", ylab = "Prothrombin")

colSums(is.na(cirrhosis_data))


library(stats)
anova_result <- aov(Prothrombin ~ Status, data = cirrhosis_data)
print(summary(anova_result))


mode_value <- Mode(cirrhosis_data$Stage, na.rm = TRUE)
cirrhosis_data$Stage[is.na(cirrhosis_data$Stage)] <- mode_value

colSums(is.na(cirrhosis_data))



library(ggplot2)
filtered_data <- na.omit(cirrhosis_data[, c('Age', 'Bilirubin', 'Status')])
ggplot(filtered_data, aes(x = Age, y = Bilirubin, color = factor(Status))) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("blue", "green", "red"), 
                     labels = c("Censored", "Censored due to liver tx", "Death")) +
  labs(title = "Scatter Plot of Age vs Serum Bilirubin",
       x = "Age",
       y = "Serum Bilirubin (mg/dl)",
       color = "Status") +
  theme_minimal()





filtered_data <- na.omit(cirrhosis_data[, c('Cholesterol', 'Albumin', 'Status')])

ggplot(filtered_data, aes(x = Cholesterol, y = Albumin, color = factor(Status))) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("blue", "green", "red"), 
                     labels = c("Censored", "Censored due to liver tx", "Death")) +
  labs(title = "Scatter Plot of Cholesterol vs Albumin",
       x = "Cholesterol",
       y = "Albumin",
       color = "Status") +
  theme_minimal()




library(ggplot2)

filtered_data <- na.omit(cirrhosis_data[, c('Copper', 'Alk_Phos', 'Status')])

# Scatter plot for Copper vs Alk_Phos
ggplot(filtered_data, aes(x = Copper, y = Alk_Phos, color = factor(Status))) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("blue", "green", "red"), 
                     labels = c("Censored", "Censored due to liver tx", "Death")) +
  labs(title = "Scatter Plot of Copper vs Alk_Phos",
       x = "Copper",
       y = "Alk_Phos",
       color = "Status") +
  theme_minimal()



library(ggplot2)

filtered_data <- na.omit(cirrhosis_data[, c('Tryglicerides', 'Platelets', 'Status')])

# Scatter plot for Tryglicerides vs Platelets
ggplot(filtered_data, aes(x = Tryglicerides, y = Platelets, color = factor(Status))) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("blue", "green", "red"), 
                     labels = c("Censored", "Censored due to liver tx", "Death")) +
  labs(title = "Scatter Plot of Tryglicerides vs Platelets",
       x = "Tryglicerides",
       y = "Platelets",
       color = "Status") +
  theme_minimal()






library(ggplot2)
upper_attributes <- c('Age', 'Bilirubin', 'Cholesterol', 'Albumin', 'Status')
upper_data <- cirrhosis_data[, upper_attributes]
pairs(upper_data, col = cirrhosis_data$Status)



library(ggplot2)
upper_attributes <- c('Copper', 'Alk_Phos', 'SGOT', 'Tryglicerides', 'Platelets', 'Status')
upper_data <- cirrhosis_data[, upper_attributes]
pairs(upper_data, col = cirrhosis_data$Status)



library(ggplot2)
library(dplyr)

filtered_data <- na.omit(cirrhosis_data)
radar_vars <- c('Age', 'Bilirubin', 'Cholesterol', 'Albumin', 'Copper')
scaled_data <- filtered_data %>%
  mutate(across(all_of(radar_vars), scales::rescale))

mean_values <- colMeans(scaled_data[radar_vars])
plot_data <- data.frame(
  variable = radar_vars,
  value = mean_values
)

# Create radar chart
ggplot(plot_data, aes(x = variable, y = value)) +
  geom_polygon(fill = "blue", color = "black", alpha = 0.5) +
  geom_line() +
  geom_point(size = 3, color = "red") +
  coord_polar() +
  theme_minimal() +
  labs(title = "Radar Chart of Mean Values",
       x = NULL, y = NULL)



# Load required libraries
library(ggplot2)
library(dplyr)

filtered_data <- na.omit(cirrhosis_data)
radar_vars <- c('Bilirubin', 'Albumin', 'Prothrombin', 'Stage')
scaled_data <- filtered_data %>%
  mutate(across(all_of(radar_vars), scales::rescale))

mean_values <- colMeans(scaled_data[radar_vars])

plot_data <- data.frame(
  variable = radar_vars,
  value = mean_values
)

ggplot(plot_data, aes(x = variable, y = value)) +
  geom_polygon(fill = "blue", color = "black", alpha = 0.5) +
  geom_line() +
  geom_point(size = 3, color = "red") +
  coord_polar() +
  theme_minimal() +
  labs(title = "Radar Chart of Mean Values",
       x = NULL, y = NULL)


colSums(is.na(cirrhosis_data))




install.packages("glmnet")
library(glmnet)
set.seed(123) # for reproducibility
train_index <- sample(1:nrow(cirrhosis_data), 0.7 * nrow(cirrhosis_data))
train_data <- cirrhosis_data[train_index, ]
test_data <- cirrhosis_data[-train_index, ]
logistic_model <- glm(Status ~ ., data = train_data, family = binomial)
predictions <- predict(logistic_model, newdata = test_data, type = "response")
accuracy <- mean((predictions > 0.5) == (test_data$Status == "D"))
print(paste("Accuracy:", accuracy))






library(rpart)
tree_model <- rpart(Status ~ ., data = train_data, method = "class")
predictions <- predict(tree_model, newdata = test_data, type = "class")
accuracy <- mean(predictions == test_data$Status)
print(paste("Accuracy:", accuracy))





library(randomForest)
rf_model <- randomForest(Status ~ ., data = train_data)
predictions <- predict(rf_model, newdata = test_data)
accuracy <- mean(predictions == test_data$Status)
print(paste("Accuracy:", accuracy))




library(e1071)
svm_model <- svm(Status ~ ., data = train_data, kernel = "radial")
predictions <- predict(svm_model, newdata = test_data)
accuracy <- mean(predictions == test_data$Status)
print(paste("Accuracy:", accuracy))






install.packages("gbm")
library(gbm)
factor_vars <- c("Drug", "Sex", "Ascites", "Hepatomegaly", "Spiders", "Edema")
train_data[factor_vars] <- lapply(train_data[factor_vars], factor)
test_data[factor_vars] <- lapply(test_data[factor_vars], factor)
train_data$Status_binary <- ifelse(train_data$Status == "D", 1, 0)
test_data$Status_binary <- ifelse(test_data$Status == "D", 1, 0)
gbm_model <- gbm(Status_binary ~ ., data = train_data, distribution = "bernoulli", n.trees = 100, interaction.depth = 4)
predictions <- predict.gbm(gbm_model, newdata = test_data, n.trees = 100, type = "response")

# Convert probabilities to class predictions
predictions <- ifelse(predictions > 0.5, "D", "CL")

# Evaluate the model
accuracy <- mean(predictions == test_data$Status)
print(paste("Accuracy:", accuracy))













# Create a vector containing the relationship results
relationship_results <- c(
  "Drug type and cirrhosis status: p = 0.9879 (no significant association)",
  "Ascites and cirrhosis status: p < 0.001 (significant association)",
  "Hepatomegaly and cirrhosis status: p < 0.001 (significant relationship)",
  "Spider nevi and cirrhosis status: p = 0.0001 (significant association)",
  "Cholesterol levels and cirrhosis status: p = 0.00301 (significant relationship)",
  "Copper levels and cirrhosis status: p < 0.001 (significant relationship)",
  "Alk_Phos levels and cirrhosis status: p = 0.00362 (significant relationship)",
  "SGOT levels and cirrhosis status: p = 1.57e-06 (significant relationship)",
  "Tryglicerides and cirrhosis status: p = 0.176 (no significant relationship)",
  "Platelets and cirrhosis status: p = 0.00131 (significant relationship)",
  "Prothrombin and cirrhosis status: p < 0.001 (significant relationship)"
)

# Create a box with the relationship results
relationship_box <- paste(relationship_results, collapse = "\n")

# Print the box
cat("Relationship Results:\n")
cat("=========================\n")
cat(relationship_box)






