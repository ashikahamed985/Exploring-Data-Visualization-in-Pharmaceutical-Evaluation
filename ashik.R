drug_data=read.csv("F:/FP/DRUG_clean.csv" , header= TRUE , sep=",")
View(drug_data)

missing_values <- colSums(is.na(drug_data))
print(missing_values)


numeric_columns <- drug_data[, c("EaseOfUse", "Effective", "Price", "Reviews", "Satisfaction")]
View(numeric_columns)


boxplot(numeric_columns,
        main = "Boxplot of Numeric Columns",
        xlab = "Variables",
        ylab = "Values")


numeric_columns <- drug_data[, c("EaseOfUse", "Effective", "Price", "Reviews", "Satisfaction")]
par(mfrow=c(2, 3))
for (col in colnames(numeric_columns)) {hist(numeric_columns[[col]], main = col, xlab = col)}



categorical_columns <- drug_data[, c("Condition", "Drug", "Form", "Indication", "Type")]
par(mfrow=c(3, 2))  
for (col in colnames(categorical_columns)) {
  category_counts <- table(categorical_columns[[col]])
  barplot(category_counts, main = col, xlab = "Category", ylab = "Frequency")
}


numeric_columns <- drug_data[, c("EaseOfUse", "Effective", "Price", "Reviews", "Satisfaction")]
pairs(numeric_columns)


satisfaction_means <- tapply(drug_data$EaseOfUse, drug_data$Satisfaction, mean)
plot(as.numeric(names(satisfaction_means)), satisfaction_means, type = "o",
     xlab = "Satisfaction", ylab = "Mean EaseOfUse",
     main = "Mean EaseOfUse Across Satisfaction Levels")


library(corrplot)
numeric_columns <- drug_data[, c("EaseOfUse", "Effective", "Price", "Reviews", "Satisfaction")]
correlation_matrix <- cor(numeric_columns)
corrplot(correlation_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black",
         title = "Correlation Matrix Heatmap")



library(ggplot2)
ggplot(drug_data, aes(x = Type, y = Satisfaction, fill = Type)) +
  geom_violin() +
  labs(title = "Satisfaction by Drug Type", x = "Type", y = "Satisfaction") +
  theme_minimal()


categorical_columns <- drug_data[, c("Condition", "Drug", "Form", "Indication", "Type")]

par(mfrow=c(3, 2))  # Arrange plots in a 3x2 grid
for (col in colnames(categorical_columns)) {
 
  category_counts <- table(categorical_columns[[col]])
  
 
  pie(category_counts, main = col)
}


library(ggplot2)

ggplot(drug_data, aes(x = EaseOfUse, y = Satisfaction)) +
  geom_point() +
  facet_wrap(~ Condition) +
  labs(title = "Scatter Plot of Reviews vs. Satisfaction Faceted by Condition",
       x = "Reviews", y = "Satisfaction")


ggplot(drug_data, aes(x = Price, y = Reviews, size = Effective, color = Effective)) +
  geom_point(alpha = 0.5) +
  scale_size_continuous(range = c(1, 10)) +
  labs(title = "Bubble Chart of Price vs. Reviews by Effectiveness", x = "Price", y = "Reviews") +
  theme_minimal()
  

library(dplyr)
library(ggplot2)
condition_form_count <- drug_data %>%
  group_by(Condition, Form) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

ggplot(condition_form_count, aes(x = reorder(Condition, Count), y = Count, fill = Form)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Medication Forms by Condition", x = "Condition", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





