library(caTools)
library(ggplot2)
library(mlbench)
library(dplyr)
library(forecast)
library(zoo)

d<- read.csv("C:/Users/HP/Downloads/relay_events.csv")
colSums(is.na(d))
d$date <- as.Date(d$date, format = "%Y-%m-%d")
d$season <- as.numeric(d$season)

# Keep only men & women 4x100m relay
relay <- d %>%
  filter(normalized_discipline == "4x100-metres-relay", sex %in% c("male", "female"))
View(relay)
# Aggregate yearly best times
relay_yearly <- relay %>%
  group_by(season, sex) %>%
  summarise(best_time = ifelse(all(is.na(mark_numeric)), NA, 
                               min(mark_numeric, na.rm = TRUE)), 
            .groups = "drop") %>%
  filter(!is.na(best_time), !is.infinite(best_time))%>%
  arrange(season)

# Split datasets
men <- relay_yearly %>% filter(sex == "male")
women <- relay_yearly %>% filter(sex == "female")

# 2. Exploratory Data Analysis

# Moving Averages (3-year window)
men$ma3 <- rollmean(men$best_time, k = 3, fill = NA, align = "right")
women$ma3 <- rollmean(women$best_time, k = 3, fill = NA, align = "right")

men_plot <- men %>% select(season, best_time, ma3) %>% mutate(sex = "Men")
women_plot <- women %>% select(season, best_time, ma3) %>% mutate(sex = "Women")

# Combine datasets
combined <- bind_rows(men_plot, women_plot)

# Plot
ggplot(combined, aes(x = season, group = sex)) +
  geom_line(aes(y = best_time, color = sex), size = 0.8) +
  geom_line(aes(y = ma3, color = sex), linetype = "dashed", size = 1) +
  labs(title = "3-Year Moving Average: Men vs Women 4x100m Relay",
       x = "Year",
       y = "Best Time (seconds)",
       color = "Category") +
  theme_minimal()

# 4. Forecasting 
# ------------------------------
men_ts <- ts(men$best_time, start = min(men$season),end=max(men$season), frequency = 1) 
women_ts <- ts(women$best_time, start = min(women$season),end=max(women$season),frequency = 1) 
print(men_ts)
print(women_ts)
# Fit ARIMA 
men_arima <- auto.arima(men_ts) 
women_arima <- auto.arima(women_ts) 
# Fit ETS 
men_ets <- ets(men_ts) 
women_ets <- ets(women_ts) 


# Forecasting 
fc_men_arima <- forecast(men_arima, h = 8) 
fc_men_ets <- forecast(men_ets, h = 8) 

fc_women_arima <- forecast(women_arima, h = 8) 
fc_women_ets <- forecast(women_ets, h = 8) 

# Forecast plots 
autoplot(men_ts) + autolayer(fc_men_arima$mean, series = "ARIMA", color = "blue") + 
  autolayer(fc_men_ets$mean, series = "ETS", color = "red") + 
  labs(title = "Men 4x100m Relay Forecasts (ARIMA vs ETS)", 
       x = "Year", y = "Best Time (seconds)") + theme_minimal() 

autoplot(women_ts) + autolayer(fc_women_arima$mean, series = "ARIMA", color = "green") + 
  autolayer(fc_women_ets$mean, series = "ETS", color = "purple") +
  labs(title = "Women 4x100m Relay Forecasts (ARIMA vs ETS)",
       x = "Year", y = "Best Time (seconds)") + theme_minimal() 

train_men <- head(men_ts, -5) 
test_men <- tail(men_ts, 5) 
train_women <- head(women_ts, -5) 
test_women <- tail(women_ts, 5) 

# Refit models on training data 
fit_men_arima <- auto.arima(train_men) 
fit_men_ets <- ets(train_men) 
fit_women_arima <- auto.arima(train_women) 
fit_women_ets <- ets(train_women) 

# Forecast on test data 
fc_men_test_arima <- forecast(fit_men_arima, h=8) 
fc_men_test_ets <- forecast(fit_men_ets, h=8) 
fc_women_test_arima <- forecast(fit_women_arima, h=8) 
fc_women_test_ets <- forecast(fit_women_ets, h=8) 

# Accuracy comparison 
acc_men <- rbind( ARIMA = accuracy(fc_men_test_arima, test_men), 
                  ETS = accuracy(fc_men_test_ets, test_men) ) 

acc_women <- rbind( ARIMA = accuracy(fc_women_test_arima, test_women), 
                    ETS = accuracy(fc_women_test_ets, test_women) ) 

print("Men 4x100m Forecast Accuracy:") 
print(acc_men) 
print("Women 4x100m Forecast Accuracy:") 
print(acc_women) 

# Define years for the table
years <- 2021:2027

# Men: combine actuals and forecasts
men_forecasts <- data.frame(
  Season = years,
  Actual = c(men$best_time[men$season >= 2021], rep(NA, 2)),
  ARIMA  = c(men$best_time[men$season >= 2021], as.numeric(fc_men_arima$mean[1:2])),
  ETS    = c(men$best_time[men$season >= 2021], as.numeric(fc_men_ets$mean[1:2]))
)

# Women: combine actuals and forecasts
women_forecasts <- data.frame(
  Season = years,
  Actual = c(women$best_time[women$season >= 2021], rep(NA, 2)),
  ARIMA  = c(women$best_time[women$season >= 2021], as.numeric(fc_women_arima$mean[1:2])),
  ETS    = c(women$best_time[women$season >= 2021], as.numeric(fc_women_ets$mean[1:2]))
)

# Print clean tables
print("Men Forecasts (2021–2027):")
print(men_forecasts)
print("Women Forecasts (2021–2027):")
print(women_forecasts)

#Visualization
# -------------
finals <- relay %>%
  filter(position %in% c("1", "2", "3", "1f1", "2f1", "3f1")) %>%
  mutate(medal = case_when(
    position %in% c("1", "1f1") ~ "Gold",
    position %in% c("2", "2f1") ~ "Silver",
    position %in% c("3", "3f1") ~ "Bronze"
  ))

medal_table <- finals %>%
  filter(sex == "male", age_cat == "senior", position %in% c("1","2","3","1f1","2f1","3f1")) %>%
  mutate(position_clean = case_when(
    position %in% c("1", "1f1") ~ "1",
    position %in% c("2", "2f1") ~ "2",
    position %in% c("3", "3f1") ~ "3",
    TRUE ~ position
  )) %>%
  group_by(date) %>%
  filter(all(c("1","2","3") %in% position_clean)) %>%  
  arrange(date, position_clean) %>%
  ungroup() %>%
  select(date, season, sex, position = position_clean, medal, nationality, mark_numeric)

#View(medal_table)   # check year-wise medal winners
country_medals <- medal_table %>%
  group_by(sex , nationality, medal) %>%
  summarise(total = n(), .groups = "drop") %>%
  arrange(sex, medal, desc(total))

ggplot(medal_table, aes(x = season, y = mark_numeric, color = medal)) +
  geom_line() + geom_point() +
  facet_wrap(~sex, scales = "free_y") +
  labs(title = "Evolution of 1st, 2nd, 3rd Place Times in 4x100m Relay",
       x = "Year", y = "Time (seconds)")


medal_table1 <- finals %>%
  filter(sex == "female", age_cat == "senior", position %in% c("1","2","3","1f1","2f1","3f1")) %>%
  mutate(position_clean = case_when(
    position %in% c("1", "1f1") ~ "1",
    position %in% c("2", "2f1") ~ "2",
    position %in% c("3", "3f1") ~ "3",
    TRUE ~ position
  )) %>%
  group_by(date) %>%
  filter(all(c("1","2","3") %in% position_clean)) %>%  
  arrange(date, position_clean) %>%
  ungroup() %>%
  select(date, season, sex, position = position_clean, medal, nationality, mark_numeric)

#View(medal_table1)   # check year-wise medal winners
country_medals1 <- medal_table1 %>%
  group_by(sex , nationality, medal) %>%
  summarise(total = n(), .groups = "drop") %>%
  arrange(sex, medal, desc(total))
ggplot(medal_table1, aes(x = season, y = mark_numeric, color = medal)) +
  geom_line() + geom_point() +
  facet_wrap(~sex, scales = "free_y") +
  labs(title = "Evolution of 1st, 2nd, 3rd Place Times in 4x100m Relay",
       x = "Year", y = "Time (seconds)")

top5_countries <- medal_table %>%
  bind_rows(medal_table1) %>%
  count(nationality, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(nationality)
ggplot(medal_table %>% bind_rows(medal_table1) %>% 
         filter(nationality %in% top5_countries),
       aes(x = nationality, fill = medal)) +
  geom_bar(position = "dodge") +
  facet_wrap(~sex) +
  labs(title = "Top 5 Countries by Medal Count (Men vs Women)",
       x = "Country", y = "Medal Count") 

#Boxplot
ggplot(medal_table, aes(x = position, y = mark_numeric, fill = position)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribution of Relay Times by Medal Position",
       x = "Medal Position", y = "Time (seconds)") +
  theme_minimal()

#Time Gap
gap <- medal_table %>%
  group_by(date) %>%
  summarise(gap = diff(range(mark_numeric[position %in% c("1","2")])),
            .groups="drop")
ggplot(gap, aes(x = date, y = gap)) +
  geom_line(color = "red") +
  labs(title = "Time Gap Between Gold and Silver Over Time",
       x = "Year", y = "Gap (seconds)")









