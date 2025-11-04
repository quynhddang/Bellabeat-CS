# Case Study: Analyzing Smart Device Fitness Data

## Introduction

Bellabeat is a high-tech company that manufactures health-focused smart products that informs and inspires women around the world. The company has developed technology that collects data on health metrics such as activity, sleep, stress, and reproductive health, empowering women with more knowledge about their own health and habits. Sando Mur and Urška Sršen, co-founder and Chief Creative Officer of Bellabeat, believes that analyzing smart device fitness data could help to unlock new growth opportunities for the company.

## Business Task

The primary objective of this case study is to identify opportunities for Bellabeat to expand its market reach and enhance its marketing strategy. By analyzing patterns in smart device usage, we can understand how individuals integrate health-tracking technology into their lives. In doing so, we can discover correlations between users and device features allowing us to generate recommendations for Bellabeat's marketing strategy.

## About the Data

The dataset being used for this case study is the [FitBit Fitness Tracker Dataset](https://www.kaggle.com/datasets/arashnic/fitbit) available on Kaggle through Mobius and licensed as a Public Domain. This Kaggle set contains personal fitness tracker data from thirty Fitbit users that have consented to the submission of personal tracker data. The data includes information about user's daily physical activity, heart rate, and sleep with some limitations. The sample size only includes 30 users which does not represent all Fitbit users or Bellabeat's customer base. Furthermore, the data is from 2016, so it does not reflect the current usage trends. Because data was collected from participants who volunteered, there may be bias towards users who are interested in fitness. 

## Importing datasets

The dataset was downloaded from Kaggle and then imported to R Studio.
```{r Importing Dataset, results= 'hide'}
# Install Packages

install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("ggplot2")

# Load packages

library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)

# Import dataset

daily_activity <- read_csv("dailyActivity_merged.csv")
daily_sleep <- read_csv("minuteSleep_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")
hourly_calories <- read_csv("hourlyCalories_merged.csv")
hourly_intensities <- read_csv("hourlyIntensities_merged.csv")
```

## Processing the Data

The data was cleaned and processed using R programming. 

First, the column names for all files used were standardized to _snake_case_ format. The data was then inspected for any missing (NA) values and duplicate rows. No missing (NA) values were found, but 525 duplicate rows were found in the minuteSleep_merged dataset. This is because the sleep data is recorded at the minute level, meaning that individuals (with the same ID) can have multiple entries for the same minute or have overlapping time periods. Because these duplicated rows represent multiple sleep logs per day for the same user rather than identical data errors, they were kept to avoid any loss of relevent information.

Next, all dates and time columns for all datasets were then formatted from character format to date format. 

Finally, because the minuteSleep_merged dataset contains data that is measured in minutes, a new sleep_summary dataset was created by aggregating total minutes asleep per user per day to allow for daily level analysis and finding patterns in user's sleep behavior.
```{r Processing Data, results='hide'}
# Clean column names

daily_activity <- clean_names(daily_activity)
daily_sleep <- clean_names(daily_sleep)
hourly_steps <- clean_names(hourly_steps)
hourly_calories <- clean_names(hourly_calories)
hourly_intensities <- clean_names(hourly_intensities)

# Checking for missing (NA) values

colSums(is.na(daily_activity))
colSums(is.na(hourly_steps))
colSums(is.na(daily_sleep))
colSums(is.na(hourly_calories))
colSums(is.na(hourly_intensities))

# Checking for duplicates 

sum(duplicated(daily_activity))
sum(duplicated(hourly_steps))
sum(duplicated(daily_sleep))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_intensities))

# Format date and time columns 

daily_activity$activity_date <- as.Date(daily_activity$activity_date, format = "%m/%d/%Y")
hourly_steps$activity_hour <- mdy_hms(hourly_steps$activity_hour)
hourly_calories$activity_hour <- mdy_hms(hourly_calories$activity_hour)
hourly_intensities$activity_hour <- mdy_hms(hourly_intensities$activity_hour)

# Create summarized sleep dataset (daily sleep per user)

sleep_summary <- daily_sleep %>%
  mutate(date = mdy_hms(date))%>%
  mutate(date = as.Date(date))%>%
  group_by(id, date)%>%
  summarise(total_minutes_asleep = sum(value, na.rm = TRUE), .groups = "drop") %>%
  clean_names()

# Check for duplicates

sum(duplicated(sleep_summary))
```

## Analyzing the Data 

Now, it is time to analyze the data and explore potential relationships between key variables in order to understand general user behaviors.
First, I determined the **average activity of users throughout the day and the week**.
```{r Average Daily Steps per day and week, results='hide'}
# Determining average daily steps per week

weekly_daily_steps <- daily_activity %>%
  mutate(day_of_week = wday(activity_date, label = TRUE, week_start =1))
  
summary_weekly <- weekly_daily_steps %>%
  group_by(day_of_week) %>%
  summarise(
    avg_steps = mean(total_steps),
    avg_sedentary_minutes = mean(sedentary_minutes),
    avg_calories = mean (calories)
  )
```
```{r Weekly Activity Visualization}
# Visualization 

ggplot(data = summary_weekly, aes(x = day_of_week, y = avg_steps, fill = avg_steps, width = 0.7))+ 
  geom_col()+
  geom_text(aes(label = round(avg_steps, 1), vjust = -0.2))+
  labs(title = "Average Steps Per Days of the Week",
    x= "Days of the Week",
    y= "Average Steps"
    )
```
![image alt](https://github.com/quynhddang/Bellabeat-CS/blob/134177807d0b6657ed4689e65341688a85939e36/Visualization/weekly_activity.png)
```{r Hourly Activity, results='hide'}
# Determining hourly activity

hour_activity <- hourly_steps %>% 
  left_join(hourly_calories, by = c("id", "activity_hour")) %>% 
  left_join(hourly_intensities, by = c("id", "activity_hour")) %>%
  mutate(date = as.Date(activity_hour)) %>%
  mutate(day_of_week = wday(date, label = TRUE, week_start = 1)) %>%
  mutate(hour = hour(activity_hour))

peak_data <- hour_activity %>%
  group_by(hour) %>%
  summarise(max_points = max(step_total, na.rm = TRUE))
```
```{r Hourly Activity Visualization}
#Visualization

ggplot(data = hour_activity)+
  geom_point(mapping = aes(x= hour, y= step_total, colour = step_total))+
  geom_line(data = peak_data, aes(x = hour, y = max_points), colour = "red")+
  labs(title = "Activity Throughtout the Day",,
  x= "Hours",
  y= "Total Steps")
```
![image alt](https://github.com/quynhddang/Bellabeat-CS/blob/134177807d0b6657ed4689e65341688a85939e36/Visualization/hourly_activity.png)

Next, I checked the impact of activity on energy expenditure by analyzing the correlation between **total steps** and **calories burned** in the daily_activity dataset.
```{r Correlation between activity and calories}
cor_steps_calories <- cor(daily_activity$total_steps, 
                          daily_activity$calories, 
                          use = "complete.obs")
                          
print(paste("Correlation between Total Steps and Calories:", cor_steps_calories))

# Visualization

ggplot(data = daily_activity, aes(x = total_steps, y = calories))+
  geom_point(alpha = 0.6, colour = "darkgoldenrod1")+
  geom_smooth(method = "lm", colour = "red")+
  labs(title = "Steps vs Calories Burned",
  x = "Total Steps",
  y = "Calories Burned")
```
![image alt](https://github.com/quynhddang/Bellabeat-CS/blob/134177807d0b6657ed4689e65341688a85939e36/Visualization/Steps_vs_calories.png)

Finally, I checked to see if there is any correlation between **daily activity** and **sleep**.
```{r Daily Sleep Data, results='hide'}
# Merging daily activity and sleep summary 

daily_sleep_combined <- left_join (daily_activity, sleep_summary, 
  by = c("id" = "id", "activity_date" = "date"))
```
```{r Correlation between activity and sleep}
cor_sleep_step <- cor(daily_sleep_combined$total_minutes_asleep,
                      daily_sleep_combined$total_steps,
                      use = "complete.obs")
                      
print(paste("Correlation between sleep and activity:", cor_sleep_step))

# Visualization

ggplot(data = daily_sleep_combined, aes(x = total_steps, y = total_minutes_asleep))+
  geom_point(alpha = 0.6, colour = "blue")+
  geom_smooth(method = "lm", se = FALSE, colour = "red")+
  labs(title = "Sleep vs Daily Steps",
      x = "Total Steps",
      y = "Minutes Asleep")
```
![image alt](https://github.com/quynhddang/Bellabeat-CS/blob/134177807d0b6657ed4689e65341688a85939e36/Visualization/Sleep_vs_activity.png)

## Conclusions and Recommendations 

### Activity throughout the week and the day

Based on the first bar graph, user activity seems to be consistent throughout the week with users being most active on Wednesdays and least active on Tuesdays. However, the recommended steps per day for an adult is 10000, and on no days did the average step count reach that goal. 

The **Activity Throughout the Day** chart shows three activity peaks indicating that users tend to be active around lunchtime (11 AM -12PM), early afternoon (2-4 PM), and early nightime (6-7 PM). The rest of the day is characterized by low activity.

From these charts, I find that users spend most of the day inactive with average daily step counts that are below the recommended 10000. This indicates that although users are using the Fitbit device regularly to track their activity, most do not meet their daily goals. 

**Recommendations**:

**Create Weekly Challenges**: Bellabeat should focus on creating weekly challenges on their app platform specifically for days with lower activity such as Tuesdays. These challenges can be personal challenges with rewards within the app. They can also be a competition amongst friends to see who can reach a certain amount of steps first or who can reach the most daily step. These challenges can provide motivation and/or a fun method to exercise with friends turning a low activity day into one with peak engagements.

**Small Daily Goals**: Instead of promoting a daily 10000 step goals like other companies, Bellabeat should focus on small daily goals through timed notifications within existing daily activity peaks. Notifications such as "12 PM: Take a walk after lunch to burn calories and improve digestion" or "5 PM: It's the end of the workday. Take a walk to relieve stress and reach your daily goal!" are relevant because it leverages the user's natural behavior making them more likely to be followed.

### Correlation between activity and calories burned

There was a moderate positive correlation (**r = 0.581**) between activity and calories indicating that users who took more steps daily and exercised more generally burned more calories. This is an expected outcome which can validate how accurately the device tracks physical activities. 

**Recommendations**: Since there is positive correlation between activity and calories burned, Bellabeat should focus on creating personalized visuals or dashboards that showcases a user's daily activity and how much calories have they burned. 

### Correlation between activity and sleep

Here, I examined whether there was a relationship between daily steps taken and extended sleep duration. While it is tempting to assume that regular exercise can directly lead to an increase in sleep duration and quality, the weak negative correlation (**r = -0.139**) suggests the connection is not so direct and that there are other factors involved. These factors can include but are not limited to stress levels, caffeine intake, noise levels, and medical conditions, factors which were not explored in this dataset. 

**Recommendations**: Since there a negative correlation between physical activity and sleep, Bellabeat should incorporate more holistic wellness apps or tools instead of just focusing on physical activity and tracking calories. This allows for more functionality with Bellabeat technology and can be more appealing to users who want to track overall health instead of just physical activity.


## Final Summary

Although the data was limited, this analysis can provide many insights for Bellabeat. With these findings, the company should focus on developing technology targeted at general health and wellness instead of just another fitness tracker. This new technology should focus on identifying personal trends for users which will allow users to be able to make more informed lifestyle decisions allowing for Bellabeat to differentiate and stand out from their competitors. 
