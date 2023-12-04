

# Install and load necessary libraries
install.packages("readxl")
library(readxl)
library(tidyverse)
library(ggplot2)

# Replace 'your_excel_file.xlsx' with the actual path to your Excel file
Walmart_report <- read_excel("C:/Users/dhruv pujari/OneDrive/Desktop/python project/Walmart report.xlsx")



# Transpose the data to make years as rows and financial metrics as columns
transposed_data <- t(Walmart_report)
colnames(transposed_df)[colnames(transposed_df) == 'Operating_ selling_general_administrative expenses'] <- 'Operating_selling_general_administrative_expenses'
colnames(transposed_df)[colnames(transposed_df) == 'Consolidated net income'] <- 'Consolidated_Net_income'
print(transposed_df)
# Convert the transposed data to a data frame
transposed_df <- as.data.frame(transposed_data)

# Set the first row as column names
colnames(transposed_df) <- transposed_df[1, ]

# Remove the first row (which contained the original column names)
transposed_df <- transposed_df[-1, ]

numeric_columns <- sapply(transposed_df, is.numeric)

# Convert numeric columns to appropriate data types
transposed_df[, numeric_columns] <- transposed_df[, numeric_columns] %>%
  lapply(function(x) as.numeric(as.character(x)))



# Add a column for years based on row names
transposed_df$Year <- rownames(transposed_df)
print(transposed_df$Net_sales)

# Bar plot for Net Sales over the years
ggplot(transposed_df, aes(x = Year, y = Net_sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = scales::comma(Net_sales)), vjust = -0.5) +  # Add text labels with commas
  labs(title = "Net Sales Over the Years",
       x = "Year",
       y = "Net Sales") +
  scale_y_continuous(labels = scales::comma)
#Bar plot for the Operating Income Over The Years
ggplot(transposed_df, aes(x = Year, y = Operating_income)) +
  geom_bar(stat = "identity", fill = "coral") +
  geom_text(aes(label = scales::comma(Operating_income)), vjust = -0.5) +
  labs(title = "Operating Income Over the Years",
       x = "Year",
       y = "Operating Income") +
  scale_y_continuous(labels = scales::comma)

# Pie chart for the composition of Total Revenues with lines between sections
total_revenues_composition <- transposed_df %>%
  select(Year, Total_revenues) %>%
  pivot_longer(-Year, names_to = "Category", values_to = "Value")


ggplot(total_revenues_composition, aes(x = "", y = Value, fill = interaction(Year, Category))) +
  geom_bar(stat = "identity", width = 1, color = "Black") +
  geom_text(aes(label = paste(scales::comma(Value))),
            position = position_stack(vjust = 0.7),
            size=5) +
  coord_polar("y") +
  labs(title = "Composition of Total Revenues",
       fill = "Category",
       x = NULL,
       y = NULL) +
  theme_void()

# Convert Consolidated_net_income to numeric
transposed_df$Consolidated_net_income <- as.numeric(as.character(transposed_df$Consolidated_net_income))

ggplot(transposed_df, aes(x = Year, y = Consolidated_net_income)) + 
  geom_bar(stat = "identity", fill = "lightgreen") +
  geom_text(aes(label = scales::comma(Consolidated_net_income)), vjust = -0.5) +
  labs(title = "Consolidated Net Income Over the Years",
       x = "Year",
       y = "Consolidated Net Income") +
  scale_y_continuous(labels = scales::comma)
  
#Bar Plot which shows the Revenue Over the Years
revenues_components <- transposed_df %>%
  select(Year, Cost_of_sales, Membership_and_other_income) %>%
  pivot_longer(-Year, names_to = "Category", values_to = "Value")
revenues_components$Value <- as.numeric(revenues_components$Value)

ggplot(revenues_components, aes(x = Year, y = Value, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(Value), group = Category),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 5,angle=270, hjust = 1) +
  labs(title = "Revenues Components Over the Years",
       x = "Year",
       y = "Value") +
  scale_y_discrete(labels = scales::comma) +
  scale_fill_manual(values = c("steelblue", "Red"))
summary(transposed_df$Operating_income)

#Operating Incomes over the Years
ggplot(transposed_df, aes(x = Year, y = Operating_income, fill = as.factor(Year))) +
  geom_col(alpha = 0.7) +
  geom_text(aes(label = scales::comma(Operating_income), group = as.factor(Year)),
            position = position_stack(vjust = 0.5, reverse = TRUE),
            color = "black",
            size = 3) +
  labs(title = "Operating Income Over the Years",
       x = "Year",
       y = "Operating Income") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral"))

#Box plot of the Membership and other Income
ggplot(transposed_df, aes(x = as.factor(Year), y = Membership_and_other_income, fill = as.factor(Year))) +
  geom_boxplot(width = 0.8, position = position_dodge(width = 1)) +
  labs(title = "Distribution of Membership and Other Income",
       x = "Year",
       y = "Membership and Other Income") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "lightcoral")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 0.75)

