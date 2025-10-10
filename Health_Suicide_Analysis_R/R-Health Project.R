library(dplyr)
install.packages("ggplot2")
library(ggplot2)
setwd("C:/Users/Admin/OneDrive/Desktop/DATA ANALYTICS PORTFOLIO/")

#loading dataset
mental_health<-read.csv("C:/Users/Admin/OneDrive/Desktop/DATA ANALYTICS PORTFOLIO/master.csv")
View(mental_health)

#check structure 
str(mental_health)

#check few fisrt rows
head(mental_health)

#checking for missing values
colSums(is.na(mental_health))

#checking for duplicate rows
sum(duplicated(mental_health))

#Renaming columns
mental_health <- mental_health %>%
  rename(
    country = country,
    year = year,
    gender = sex,
    age_group = age,
    no_of_suicides = suicides_no,
    population = population,
    suicides_per_100k = suicides.100k.pop,
    gdp_per_capita = gdp_per_capita....,
    generation = generation
  )
View(mental_health)


#Basic summary of dataset
#1.no. of rows and columns
dim(mental_health)

summary(mental_health)

#3. viewing uniqueness in data
unique(mental_health$country)
unique(mental_health$gender)
View(mental_health)

#Mutation
mental_health<-mental_health %>%
  mutate(Suicide_rate_per_pop=(no_of_suicides/population)*100000)

#ANALYSIS
#1What is the rate of suicide per year?
suicides_per_year<-mental_health %>%
  group_by(year) %>% 
  summarise(total_suicides_per_year=sum(no_of_suicides))
suicides_per_year

# Plot the trend
ggplot(suicides_per_year, aes(x = year, y = total_suicides_per_year)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred") +
  labs(title = "Global Suicides per Year (1985–2016)",
       x = "Year",
       y = "Number of Suicides")

#Suicide by gender
# Summarize total suicides by gender
suicides_by_gender <- mental_health %>%
  group_by(gender) %>%
  summarise(total_suicides_by_gender = sum(no_of_suicides,na.rm = T))

# Bar chart
ggplot(suicides_by_gender, aes(x = gender, y = total_suicides_by_gender, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Suicides by Gender (1985–2016)",
       x = "Gender",
       y = "Number of Suicides") +
  theme_minimal()


#3 Top 10 countries by total suicides
top_countries <- mental_health %>%
  group_by(country) %>%
  summarise(total_suicides_by_country= sum(no_of_suicides, na.rm = TRUE)) %>%
  arrange(desc(total_suicides_by_country)) %>%
  head(10)

# Plot
ggplot(top_countries, aes(x = reorder(country, total_suicides_by_country)
                          , y = total_suicides_by_country, fill = country)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Countries by Total Suicides (1985–2016)",
       x = "Country",
       y = "Number of Suicides") +
  theme_minimal()

#4 Average suicide rate by gender
avg_rate_by_gender <- mental_health %>%
  group_by(gender) %>%
  summarise(avg_rate = mean(suicides_per_100k, na.rm = TRUE))

# Plot
ggplot(avg_rate_by_gender, aes(x = gender, y = avg_rate, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Suicide Rate (per 100k) by Gender",
       x = "Gender",
       y = "Suicide Rate per 100k Population") +
  theme_minimal()

#5Suicides by age
suicides_by_age <- mental_health %>%
  group_by(age_group) %>%
  summarise(total_suicides = sum(no_of_suicides, na.rm = TRUE)) %>%
  arrange(desc(total_suicides))

# View first few rows
head(suicides_by_age)

# Visualization
ggplot(suicides_by_age, aes(x = reorder(age_group, total_suicides), y = total_suicides)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Total Suicides by Age Group (1985–2016)",
       x = "Age Group",
       y = "Total Number of Suicides") +
  theme_minimal()

#6 Make sure GDP and suicide rate columns are numeric
mental_health$gdp_per_capita <- as.numeric(mental_health$gdp_per_capita)
mental_health$suicides_per_100k <- as.numeric(mental_health$suicides_per_100k)

# Scatter plot
ggplot(mental_health, aes(x = gdp_per_capita, y = suicides_per_100k)) +
  geom_point(alpha = 0.5, color = "darkred") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "GDP per Capita vs Suicide Rate (per 100k)",
       x = "GDP per Capita (USD)",
       y = "Suicides per 100k Population") +
  theme_minimal()


