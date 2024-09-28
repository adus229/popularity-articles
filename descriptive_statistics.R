# installation of pakcages

install.packages("vtable")
install.packages("ggcorrplot")
install.packages("lares")

# importation of pakcage

library("vtable")
library("ggplot2")
library("ggcorrplot")
library("lares")
library("gridExtra")

# importation de la base

df = read.csv(file = "C:/Users/Dell/Desktop/Projet AMS/articles_data.csv",sep =",", header = TRUE)

# filter and take importants variables

df = df[df$timedelta > 21, ]

attach(df)

var = c("shares","n_tokens_title", "n_tokens_content", "num_hrefs","num_imgs","num_videos")

# voir  le haut du tablea avoir les variables s?lectionn?s

head(df[var])

# see if there are NAs values

sum(is.na(df[var]))

# labels of different variables

labs = c("Number of shares target","Number of words in the title", "Number of words in the content","Number of links","Number of images", "Number of videos")

# summary statistics

stargazer(df[var],omit.summary.stat = c("p25", "p75"))

# histogramm number of share (for target variables)

ggplot(df, aes(x = log(shares))) +
  
  geom_histogram(aes(y= ..density..), colour = 1, fill = "white") +
  
  geom_density(lwd= 1, colour = 4, fill = 4, alpha = 0.25)

# histogramm number of share video

ggplot(df, aes(x = num_videos)) + 
    geom_histogram()


# categorial varialbes that we will describe

cat_var = c("data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech") 

base = cbind(data_channel_is_entertainment, data_channel_is_bus, data_channel_is_world, data_channel_is_socmed, data_channel_is_tech, data_channel_is_lifestyle)
  
# plot the distribution for  entertainment data channel or not

par(mfrow=c(2,2))

graph1 = ggplot(df, aes(x = as.factor(data_channel_is_entertainment))) +
  
  geom_bar(fill ="sienna2", color = "black", aes(y = (..count..) / sum(..count..))) +
  
  geom_text(aes(y = ((..count..) / sum(..count..)), label = scales::percent((..count..) / sum(..count..))), stat ="count", vjust = -0.25) +
  
  scale_y_continuous(labels = percent ) +
  
  labs( x = "Entertainment or not", y = "count")


# plot the distribution for business data channel or not

graph2 = ggplot(df, aes(x = as.factor(data_channel_is_bus))) +
  
  geom_bar(fill ="cornflowerblue", color = "black", aes(y = (..count..) / sum(..count..))) +
  
  geom_text(aes(y = ((..count..) / sum(..count..)), label = scales::percent((..count..) / sum(..count..))), stat ="count", vjust = -0.25) +
  
  scale_y_continuous(labels = percent ) +
  
  labs( x = "Business or not", y = "count")

# plot the distribution for social media

graph3 = ggplot(df, aes(x = as.factor(data_channel_is_socmed))) +
  
  geom_bar(fill ="green", color = "black", aes(y = (..count..) / sum(..count..))) +
  
  geom_text(aes(y = ((..count..) / sum(..count..)), label = scales::percent((..count..) / sum(..count..))), stat ="count", vjust = -0.25) +
  
  scale_y_continuous(labels = percent ) +
  
  labs( x = "Social Media or not", y = "count")


# plot the distributon for tech data channel or not

graph4 = ggplot(df, aes(x = as.factor(data_channel_is_tech))) +
  
  geom_bar(fill ="cornflowerblue", color = "black", aes(y = (..count..) / sum(..count..))) +
  
  geom_text(aes(y = ((..count..) / sum(..count..)), label = scales::percent((..count..) / sum(..count..))), stat ="count", vjust = -0.25) +
  
  scale_y_continuous(labels = percent ) +
  
  labs( x = "Tech or not", y = "count")


# Lifestyle or not

graph5 = ggplot(df, aes(x = as.factor(data_channel_is_lifestyle))) +
  
  geom_bar(fill ="goldenrod", color = "black", aes(y = (..count..) / sum(..count..))) +
  
  geom_text(aes(y = ((..count..) / sum(..count..)), label = scales::percent((..count..) / sum(..count..))), stat ="count", vjust = -0.25) +
  
  scale_y_continuous(labels = percent ) +
  
  labs( x = "Lifestyle or not", y = "count")


graph6 = ggplot(df, aes(x = as.factor(data_channel_is_world))) +
  
  geom_bar(fill ="cornflowerblue", color = "black", aes(y = (..count..) / sum(..count..))) +
  
  geom_text(aes(y = ((..count..) / sum(..count..)), label = scales::percent((..count..) / sum(..count..))), stat ="count", vjust = -0.25) +
  
  scale_y_continuous(labels = percent ) +
  
  labs( x = "World or not", y = "count")


### mettre les pourcentages

grid.arrange(graph1, graph2, graph3,graph4, graph5, graph6, ncol=3)

# levels = c("0", "1"), labels = c("Yes",)

# plot the days of the publication of the articles

data = table(df$weekday_is_monday,df$weekday_is_tuesday,df$weekday_is_wednesday,df$weekday_is_thursday,df$weekday_is_friday,df$weekday_is_saturday,df$weekday_is_sunday)

days = c("Monday","Tuesday","Wednesday","Thirsday","Friday","Saturday","Sunday")

Monday = table(df$weekday_is_monday)
Tuesday = table(df$weekday_is_tuesday)
Wednesday = table(df$weekday_is_wednesday)
Thirsday = table(df$weekday_is_thursday)
Friday = table(df$weekday_is_friday)
Saturday = table(df$weekday_is_saturday)
Sunday = table(df$weekday_is_sunday)

data = as.data.frame(cbind(rbind(Monday,Tuesday,Wednesday,Thirsday,Friday,Saturday,Sunday), days))

data = transform(data, perc =  round(as.numeric(data$`1`) / nrow(df), 4))

data = transform(data, perc_lab = paste(perc * 100, "%"))

ggplot(data, aes(x="", y = data$X1, fill = days)) + 
  
  geom_bar(stat = "identity", width = 1, color = "white") +
  
  geom_text(aes(label = perc_lab), position = position_stack(vjust = 0.5)) +
  
  coord_polar("y", start = 0) + theme_void()


# correlation table for quantitative variable

# type of the variable

num_df = df[,unlist(lapply(df, is.integer))]

corr = cor(num_df)

ggcorrplot(corr, hc.order = T, type = "lower", lab = TRUE)


# show the 10 most relevant correlation

corr_cross(num_df, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)


require(scales)

ggplot(df, aes(as.factor(data_channel_is_bus), fill = data_channel_is_bus)) + 
  
  geom_bar(stat = "count",fill = "cornflowerblue" , color = "black") +
  
  labs( x = "Business or Not ", y = " count")

ggplot(df, aes(x = as.factor(data_channel_is_bus))) +
         
          geom_bar(aes(y = (..count..) / sum(..count..))) +
  
          geom_text(aes(y = ((..count..) / sum(..count..)), label = scales::percent((..count..) / sum(..count..))), stat ="count", vjust = -0.25) +
  
          scale_y_continuous(labels = percent ) +
  
          labs( x = "Business or not", y = "count")


