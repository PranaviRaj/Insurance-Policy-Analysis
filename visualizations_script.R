
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure data is loaded (replace 'data' with your actual data frame name)
# data <- read.csv("your_data_file.csv")

# 1. Count of Policies by Start Year and Status
# Ensure start_year and status columns are correctly formatted
data$start_year <- as.factor(data$start_year)
data$status <- as.factor(data$status)

# Plot count of policies by start year and status
ggplot(data, aes(x = start_year, fill = status)) +
  geom_bar(position = "dodge") +
  labs(title = "Count of Policies by Start Year and Status", x = "Start Year", y = "Policy Count") +
  theme_minimal()

# 2. Top Selling Agent
top_agents <- data %>%
  group_by(agent_name) %>%  # Replace 'agent_name' with your actual column
  summarise(total_policies = n()) %>%
  arrange(desc(total_policies)) %>%
  slice(1)  # Get the top agent

ggplot(top_agents, aes(x = reorder(agent_name, -total_policies), y = total_policies)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top Selling Agent", x = "Agent Name", y = "Total Policies Sold") +
  theme_minimal()

# 3. Top 5 States by Premium Collected
top_states <- data %>%
  group_by(state) %>%  # Replace 'state' with your actual column
  summarise(total_premium = sum(premium, na.rm = TRUE)) %>%
  arrange(desc(total_premium)) %>%
  slice(1:5)  # Get top 5 states

ggplot(top_states, aes(x = reorder(state, -total_premium), y = total_premium)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "Top 5 States by Premium Collected", x = "State", y = "Total Premium Collected") +
  theme_minimal()

# 4. Overall Loss Ratio (Percentage)
overall_loss_ratio <- sum(data$claims_paid, na.rm = TRUE) / sum(data$premium, na.rm = TRUE) * 100  # Replace with actual column names
overall_loss_ratio <- round(overall_loss_ratio, 2)  # Round to 2 decimals

# Display result
print(paste("Overall Loss Ratio: ", overall_loss_ratio, "%"))

# 5. Average Duration (in years) of Active Policies
average_duration <- data %>%
  filter(status == "Active") %>%  # Replace 'status' with your column name
  mutate(duration = as.numeric(Sys.Date() - start_date) / 365.25) %>%  # Replace 'start_date' with your column
  summarise(avg_duration_years = mean(duration, na.rm = TRUE)) %>%
  pull(avg_duration_years)

print(paste("Average Duration of Active Policies:", round(average_duration, 2), "years"))

# 6. Breakdown of Loss Ratio by Category 1 and 2
loss_ratio_by_category <- data %>%
  group_by(category_1, category_2) %>%  # Replace with actual column names
  summarise(
    total_claims = sum(claims_paid, na.rm = TRUE),
    total_premium = sum(premium, na.rm = TRUE)
  ) %>%
  mutate(loss_ratio = round((total_claims / total_premium) * 100, 2))

ggplot(loss_ratio_by_category, aes(x = category_1, y = loss_ratio, fill = category_2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loss Ratio by Category 1 and 2", x = "Category 1", y = "Loss Ratio (%)") +
  theme_minimal()
