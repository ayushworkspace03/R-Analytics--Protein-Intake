# Install and load necessary packages
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")

library(ggplot2)
library(readxl)

# Load the dataset
data <- read_excel("C:/Users/AYUSH AGGARWAL/OneDrive/Documents/Indian Liver Patient Dataset.xlsx")

# Ensure column names are consistent
colnames(data) # Verify column names in the dataset

# 1. Distribution of Patient Ages
ggplot(data, aes(x = AGE)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Patient Ages", x = "Age", y = "Frequency")

# 2. Patients Diagnosed with Liver Disease
patient_count <- table(data$IS_PATIENT)
ggplot(as.data.frame(patient_count), aes(x = factor(Var1), y = Freq)) +
  geom_bar(stat = "identity", fill = c("red", "green")) +
  labs(title = "Number of Patients Diagnosed with Liver Disease", 
       x = "Diagnosis (0=No, 1=Yes)", y = "Number of Patients")

# 3. Gender Distribution
gender_distribution <- table(data$GENDER)
ggplot(as.data.frame(gender_distribution), aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  labs(title = "Gender Distribution Among Patients", x = "", y = "")

# 4. Correlation Between Age and Total Bilirubin Levels
ggplot(data, aes(x = AGE, y = TOT_BILIRIUM)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Correlation Between Age and Total Bilirubin Levels", 
       x = "Age", y = "Total Bilirubin")

# 5. Average Albumin Level by Liver Disease Status
ggplot(data, aes(x = factor(IS_PATIENT), y = ALBUMIN)) +
  geom_boxplot(fill = c("lightblue", "lightgreen")) +
  labs(title = "Average Albumin Level by Liver Disease Status", 
       x = "Diagnosis (0=No, 1=Yes)", y = "Albumin Level")

# 6. SGPT Levels by Gender
sgpt_by_gender <- aggregate(SGPT ~ GENDER, data, mean, na.rm = TRUE)
ggplot(sgpt_by_gender, aes(x = GENDER, y = SGPT)) +
  geom_bar(stat = "identity", fill = c("orange", "purple")) +
  labs(title = "Average SGPT Levels by Gender", x = "Gender", y = "Average SGPT Level")

# 7. Percentage of Patients with High ALKPHOS Levels (> 100)
high_alkphos_percentage <- sum(data$ALKPHOS > 100, na.rm = TRUE) / nrow(data) * 100
ggplot() +
  geom_bar(aes(x = "", y = high_alkphos_percentage), stat = "identity", fill = "blue") +
  labs(title = "Percentage of Patients with High ALKPHOS Levels (>100)", 
       x = "", y = "Percentage (%)") + 
  coord_flip()

# 8. AG Ratio by Liver Disease Status
ggplot(data, aes(x = factor(IS_PATIENT), y = AG_RATIO)) +
  geom_boxplot(fill = c("pink", "lightyellow")) +
  labs(title = "AG Ratio by Liver Disease Status", 
       x = "Diagnosis (0=No, 1=Yes)", y = "AG Ratio")

# 9. Maximum SGOT Level Recorded
max_sgot <- max(data$SGOT, na.rm = TRUE)
ggplot() +
  geom_bar(aes(x = "", y = max_sgot), stat = "identity", fill = "red") +
  labs(title = "Maximum SGOT Level Recorded in the Dataset", 
       x = "", y = "Maximum SGOT Level")

# 10. Correlation Between Total Protein and Direct Bilirubin Levels
ggplot(data, aes(x = TOT_PROTIENS, y = DIRECT_BILIRIUM)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Correlation Between Total Protein and Direct Bilirubin Levels", 
       x = "Total Protein Level", y = "Direct Bilirubin Level")
