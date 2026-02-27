<<<<<<< Updated upstream
### LOADING IN THE DATA AND PACKAGES ###
# the packages
library(ggplot2)
library(gridExtra)

# the data
old_df <- read.csv("group_8_data.csv")

###############################################################################################
#                              EXPLORATORY DATA ANALYSIS                                      #
###############################################################################################
# 1.1 Visualizing the data
grid.arrange(
  ggplot(old_df, aes(city)) + geom_bar(fill = 'lightblue', color = 'blue'),
  ggplot(old_df, aes(site)) + geom_bar(fill = 'lightblue', color = 'blue'),
  ggplot(old_df, aes(Wind.Speed...Resultant)) + geom_histogram(fill = 'lightblue', color = 'blue'),
  ggplot(old_df, aes(Outdoor.Temperature)) + geom_histogram(fill = 'lightblue', color = 'blue'),
  ggplot(old_df, aes(Barometric.pressure)) + geom_histogram(fill = 'lightblue', color = 'blue'),
  ggplot(old_df, aes(Ozone)) + geom_histogram(fill = 'lightblue', color = 'blue')
             )

# 1.2 transform data
df <- df %>% mutate(
  log_Ozone = log(Ozone),
  log_wind_speed = log(Wind.Speed...Resultant),
  date = make_date(year, month)
)

# 1.2.1 Remove the column due to 89% na values
df$Barometric.pressure <- NULL

# 1.3 imputing the data
# 1.3.1 checking the % of na values
colMeans(is.na(df)) * 100

# 1.3.2 Replace NA with median for all remaining numeric columns
df[] <- lapply(df, function(x) {
=======
#hello everyone! changed

data<- read.csv("data.csv")

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
>>>>>>> Stashed changes
  if(is.numeric(x)) {
    x[is.na(x)] <- median(x, na.rm = TRUE)
  }
  return(x)
})

<<<<<<< Updated upstream
# 1.4 Visualising the transformed and imputed data
grid.arrange(
  ggplot(df, aes(city)) + geom_bar(fill = 'lightblue', color = 'blue'),
  ggplot(df, aes(site)) + geom_bar(fill = 'lightblue', color = 'blue'),
  ggplot(df, aes(log_wind_speed)) + geom_histogram(fill = 'lightblue', color = 'blue'),
  ggplot(df, aes(Outdoor.Temperature)) + geom_histogram(fill = 'lightblue', color = 'blue'),
  ggplot(df, aes(log_Ozone)) + geom_histogram(fill = 'lightblue', color = 'blue')
)

# 1.5 ozone vs time plot, with imputed values
ggplot(data=df[],aes(y=log_Ozone,x=date, color = city)) +
  geom_line(alpha=0.7, size = 0.6) +
  theme_minimal() +
  labs(y="log Ozone")

################################################################################
#                         Extreme Value Modeling                               #
################################################################################
# 2.1 getting the maximum each year (need to do for each city)
df_max <- df %>% summarise(df_max= max(df),.by=year)







# PLOTS FOR MEASURING log(OZONE) AGAINST THE METEROLOGICAL COVARIATES


# Temperature Variable

# Overall log(Ozone) against Outdoor Temperature, where we have omitted West Los Angeles
ggplot(subset(data, city != "West Los Angeles"), aes(x = Outdoor.Temperature, y = logOzone)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, colour = "purple") +
  xlab("Outdoor Temperature") +
  ylab("log(Ozone level)") +
  ggtitle("log(Ozone) vs Outdoor Temperature")


# log(Ozone) against Outdoor Temperature, where we have omitted West Los Angeles
ggplot(subset(data, city != "West Los Angeles"),
       aes(x = Outdoor.Temperature, y = logOzone)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "loess", se = FALSE, colour = "purple") +
  facet_wrap(~ city) +
  xlab("Outdoor Temperature") +
  ylab("log(Ozone level)") +
  ggtitle("log(Ozone) vs Outdoor Temperature (by city, excluding West Los Angeles)")
=======
colMeans(is.na(data)) * 100

names(data)
unique(data$city)

data$logOzone <- log(data$Ozone)

ggplot(data, aes(x = city, y = logOzone)) +
  geom_boxplot(fill = "purple") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("City")+
  ylab("log Ozone level")+
  ggtitle("Boxplots of Ozone levels by city")

names(data)

#eda for meteorological factors on ozone #

ggplot(data, aes(x=Outdoor.Temperature, y=logOzone))+
  geom_point()+ geom_smooth()

ggplot(data, aes(x=Wind.Speed...Resultant, y=logOzone))+
  geom_point()+ geom_smooth()

names(data)

cor_matrix <- cor(data[, sapply(data, is.numeric)])

corrplot::corrplot(cor_matrix, method="color", type="upper", tl.cex=0.8)


library(extRemes)

library(extRemes)  # library for GEV models
library(lubridate) # library to work with dates and time
library(tidyr)     # library for data tidying
library(dplyr)     # library for data manipulation 

ggplot(data=data,aes(y=logOzone,x=date))+
  geom_line(alpha=0.35)+
  labs(y="log ozone")

names(data)
>>>>>>> Stashed changes
