# Load the required packages
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)

# Read the Excel file into a data frame
dataset <- read_excel("D:\\DATA ANALYTICS\\Dataset main.xlsx")

# 1. Political Party Distribution (Pie Chart)
custom_colors <- c("orange","blue","green","red")

# Create the party pie chart with custom colors
party_pie_chart <- ggplot(dataset, aes(x = "", fill = Party)) +
  geom_bar(width = 1) +
  coord_polar("y") +
  labs(title = "Party Vote Distribution") +
  theme_void() +
  scale_fill_manual(values = custom_colors)

# Display the party pie chart
print(party_pie_chart)

# 2. Income Distribution by Gender (Box Plot)
income_gender_boxplot <- ggplot(dataset, aes(x = Gender, y = Income, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Income Distribution by Gender", x = "Gender", y = "Income", fill = "Gender") +
  theme(legend.position = "none")

# Display the income distribution by gender box plot
print(income_gender_boxplot)

# 3. Education Level vs. Voting Preference (Stacked Bar Chart)
education_voting_chart <- ggplot(dataset, aes(x = Education, fill = Voting_Preference)) +
  geom_bar() +
  labs(title = "Education Level vs. Voting Preference", x = "Education Level", y = "Count", fill = "Voting Preference")

# Display the education level vs. voting preference chart
print(education_voting_chart)

# 4. Age Distribution by Political Party (Density Plot)
age_party_density <- ggplot(dataset, aes(x = Age, color = Party, fill = Party)) +
  geom_density(alpha = 0.5) +
  labs(title = "Age Distribution by Political Party", x = "Age", y = "Density", fill = "Party", color = "Party") +
  theme(legend.position = "bottom")

# Display the age distribution by political party density plot
print(age_party_density)

#print(income_education_scatter)
# 5. Interactive Bar Chart - Age Distribution
age_distribution_barchart <- plot_ly(dataset, x = ~Age, type = "histogram", histnorm = "count", marker = list(color = "red")) %>%
  layout(title = "Age Distribution", xaxis = list(title = "Age"), yaxis = list(title = "Count"))

# Display the interactive age distribution bar chart
print(age_distribution_barchart)

# 6. Bar Graph - Party-wise Average Income
party_income_bar <- ggplot(dataset, aes(x = Party, y = Income)) +
  geom_bar(stat = "summary", fun = "mean", fill = "black", color = "red") +
  labs(title = "Party-wise Average Income", x = "Party", y = "Average Income")

# Display the party-wise average income bar graph
print(party_income_bar)

# Perform analysis with respect to religion

# 1. Religion-wise Party Distribution (Bar Chart)
religion_party_chart <- dataset %>%
  group_by(Religion, Party) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Religion, y = Count, fill = Party)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Religion-wise Party Distribution", x = "Religion", y = "Count", fill = "Party") +
  theme(legend.position = "top")

# Display the religion-wise party distribution chart
print(religion_party_chart)

# 2. Religion-wise Education Level Analysis (Box Plot)
religion_education_boxplot <- dataset %>%
  group_by(Religion, Education) %>%
  summarise(Average_Income = mean(Income)) %>%
  ggplot(aes(x = Religion, y = Average_Income, fill = Education)) +
  geom_boxplot() +
  labs(title = "Religion-wise Education Level Analysis", x = "Religion", y = "Average Income", fill = "Education") +
  theme(legend.position = "bottom")

# Display the religion-wise education level analysis box plot
print(religion_education_boxplot)

# 3. Religion-wise Age Distribution (Histogram)
religion_age_histogram <- dataset %>%
  ggplot(aes(x = Age, fill = Religion)) +
  geom_histogram(binwidth = 5, color = "white", position = "identity") +
  labs(title = "Religion-wise Age Distribution", x = "Age", y = "Count", fill = "Religion") +
  theme(legend.position = "top")

# Display the religion-wise age distribution histogram
print(religion_age_histogram)

# Perform age-related analysis

# 1. Age Distribution (Histogram)
age_histogram <- ggplot(dataset, aes(x = Age)) +
  geom_histogram(binwidth = 5, color = "white", fill = "steelblue") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal()

# Display the age distribution histogram
print(age_histogram)

# 2. Age vs. Income (Scatter Plot)
age_income_scatter <- ggplot(dataset, aes(x = Age, y = Income)) +
  geom_point(color = "steelblue", alpha = 0.5) +
  labs(title = "Age vs. Income", x = "Age", y = "Income") +
  theme_minimal()

# Display the age vs. income scatter plot
print(age_income_scatter)

# 3. Age by Gender (Box Plot)
age_gender_boxplot <- ggplot(dataset, aes(x = Gender, y = Age, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Age by Gender", x = "Gender", y = "Age", fill = "Gender") +
  theme_minimal()

# Display the age by gender box plot
print(age_gender_boxplot)

# 4. Age by Education Level (Violin Plot)
age_education_violin <- ggplot(dataset, aes(x = Education, y = Age, fill = Education)) +
  geom_violin() +
  labs(title = "Age by Education Level", x = "Education Level", y = "Age", fill = "Education") +
  theme_minimal()

# Display the age by education level violin plot
print(age_education_violin)

# Perform region-based analysis

# 1. Region-wise Party Distribution (Bar Chart)
region_party_chart <- dataset %>%
  group_by(Region, Party) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Region, y = Count, fill = Party)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Region-wise Party Distribution", x = "Region", y = "Count", fill = "Party") +
  theme(legend.position = "top")

# Display the region-wise party distribution chart
print(region_party_chart)

# 2. Region-wise Education Level Analysis (Box Plot)
region_education_boxplot <- dataset %>%
  group_by(Region, Education) %>%
  summarise(Average_Income = mean(Income)) %>%
  ggplot(aes(x = Region, y = Average_Income, fill = Education)) +
  geom_boxplot() +
  labs(title = "Region-wise Education Level Analysis", x = "Region", y = "Average Income", fill = "Education") +
  theme(legend.position = "bottom")

# Display the region-wise education level analysis box plot
print(region_education_boxplot)

# 3. Region-wise Age Distribution (Histogram)
region_age_histogram <- dataset %>%
  ggplot(aes(x = Age, fill = Region)) +
  geom_histogram(binwidth = 5, color = "white", position = "identity") +
  labs(title = "Region-wise Age Distribution", x = "Age", y = "Count", fill = "Region") +
  theme(legend.position = "top")

# Display the region-wise age distribution histogram
print(region_age_histogram)