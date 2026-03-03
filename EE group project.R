library(ggplot2)
library(extRemes)  
library(lubridate) 
library(tidyr)     
library(dplyr)  
library(gridExtra)

#### from molly ####

data <- read.csv("group_8_data.csv")

mean(is.na(data)) * 100

names(data)

str(data)

plot(density(data$Ozone, na.rm = TRUE))


#use median since ozone factor is slightly skewed#


colMeans(is.na(data)) * 100


# Remove the column due to 89% na values

data$Barometric.pressure <- NULL



# Replace NA with median for all remaining numeric columns

data[] <- lapply(data, function(x) {
  if(is.numeric(x)) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
  }
  return(x)
  
})


colMeans(is.na(data)) * 100


names(data)
unique(data$city)


data <- data %>% mutate(
  log_Ozone = log(Ozone),
  log_wind_speed = log(Wind.Speed...Resultant),
  date = make_date(year, month)
)



ggplot(data, aes(x = city, y = logOzone)) +
  geom_boxplot(fill = "purple") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("City")+
  ylab("Log(Ozone level)")+
  ggtitle("Boxplots of Log(Ozone level) by city")




#### extreme pollution levels ####

ggplot(data, aes(y=log_Ozone, x=as.factor(month)))+
  geom_boxplot(fill = "purple")+
  xlab("Month")+
  ylab("Log Ozone")+
  ggtitle("Boxplots of Log Ozone level by Month")




ggplot(data, aes(y=log_Ozone, x=as.factor(year)))+
  geom_boxplot(fill = "purple")+
  xlab("Year")+
  ylab("Log Ozone")+
  ggtitle("Boxplots of Log Ozone level by Year")

# looks like we have seasonality but not a trend


grid.arrange(
  ggplot(data, aes(city)) + geom_bar(fill = 'lightblue', color = 'blue'),
  ggplot(data, aes(site)) + geom_bar(fill = 'lightblue', color = 'blue'),
  ggplot(data, aes(log_wind_speed)) + geom_histogram(fill = 'lightblue', color = 'blue'),
  ggplot(data, aes(Outdoor.Temperature)) + geom_histogram(fill = 'lightblue', color = 'blue'),
  ggplot(data, aes(log_Ozone)) + geom_histogram(fill = 'lightblue', color = 'blue')
)


ggplot(data=data,aes(y=log_Ozone,x=date, color = city)) +
  geom_line(alpha=0.7, size = 0.6) +
  theme_minimal() +
  labs(y="log Ozone")



data_max <- data %>% summarise(ozone_max = max(log_Ozone), .by = year)

ggplot(data=data_max, aes(y=ozone_max, x=year))+
  geom_line()+
  labs(y="Log Ozone annual maximum")
# shows a potential decresing trend in annual maximum ozone
# dips significantly in 2016

# gev model fitting

fit_gev <- fevd(data_max$ozone_max,method="MLE")
results <- summary(fit_gev)

# ci for gev model parameters
ci(fit_gev, alpha = 0.05, type = c("parameter"))
# the shape parameter is negative
# so we use the Weibull distribution
# we have a bounded upper tail => there is a theoretical upper limit to ozone concentrations





plot(fit_gev, type = "rl")
# this doesn't look like the model is a very good fit
# we have a very small n (10) so the return period is uncertain









# scatter plot of log ozone by year

ggplot(data, aes(x=year, y=Ozone))+
  geom_point()

# setting maximum values
data_yearly_max <- data %>%
  group_by(year) %>%
  summarise(max_ozone = max(Ozone), .groups = "drop")

# scatterplot with maximums highlighted

ggplot(data, aes(x = year, y = Ozone)) +
  geom_point() +
  geom_point(data = data_yearly_max,
             aes(x = year, y = max_ozone),
             colour = "red",
             inherit.aes = FALSE)


# splitting by city

data_yearly_max <- data %>%
  group_by(city, year) %>%
  summarise(
    max_ozone = max(Ozone),
    .groups = "drop"
  )

ggplot(data, aes(x = year, y = Ozone)) +
  geom_point() +
  geom_point(data = data_yearly_max,
             aes(x = year, y = max_ozone),
             colour = "red",
             inherit.aes = FALSE) +
  facet_wrap(~ city) +
  theme_minimal() +
  labs(y = "Ozone",
       title = "Monthly Ozone with Annual Maxima Highlighted")


# plotting the annual maxima per city
ggplot(data_yearly_max, aes(x = year, y = max_ozone)) +
  geom_line() +
  geom_point(colour = "red") +
  facet_wrap(~ city) +
  theme_minimal() +
  labs(y = "Annual Maximum Ozone")





# subsetting k=3 largest values
data_top3 <- data %>%
  group_by(city, year) %>%
  arrange(desc(Ozone), .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  mutate(rank = row_number()) %>%
  ungroup()



ggplot(data, aes(x = year, y = Ozone)) +
  geom_point() +
  geom_point(
    data = data_top3,
    aes(x = year, y = Ozone, colour = factor(rank)),
    inherit.aes = FALSE
  ) +
  facet_wrap(~ city) +
  theme_minimal() +
  labs(
    y = "Ozone",
    title = "Top Three Monthly Ozone Observations per City-Year"
  )


# using date as the time variable

ggplot(data, aes(x = date, y = Ozone)) +
  geom_point() +
  geom_point(
    data = data_top3,
    aes(x = date, y = Ozone, colour = factor(rank)),
    inherit.aes = FALSE
  ) +
  facet_wrap(~ city) +
  theme_minimal() +
  theme_minimal()







# is it the mean for that month? -> smooths out peaks
# US standard is 0.070ppm (epa.gov), anything over that is considered "extreme"

threshold <- 0.07

data <- data %>% 
  mutate(reg_exceed = Ozone > threshold)

mean(data$reg_exceed)

data %>%
  group_by(city) %>%
  summarise(exceed_rate = mean(reg_exceed))

exceed_summary <- data %>%
  group_by(city, year) %>%
  summarise(
    n_months = n(),
    n_exceed = sum(reg_exceed),
    exceed_rate = mean(reg_exceed),
    .groups = "drop"
  )

ggplot(exceed_summary, aes(x = year, y = exceed_rate)) +
  geom_line() +
  facet_wrap(~ city) +
  theme_minimal() +
  labs(y = "Proportion of months above threshold")

# only one city exceeds the regulated standard -> Glendora

