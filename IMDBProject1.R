library(ggplot2)
data = read.csv("movie_metadata.csv", sep = ",", h = TRUE)

library(plyr)
library(COUNT)
res = subset(data, select = director_name)
freq_df = count(res, vars = "director_name")
frequency_df = subset(freq_df, freq > 10 & freq < 100)
frequency_df
summary(frequency_df$freq)
nrow(frequency_df)
ggplot(frequency_df, aes(director_name, freq), fill = dose) +
  geom_bar(stat = "identity", position = "dodge", color = "#000099", lwd = 1, fill = "#80ffff", width = 0.8) +
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                   size=10, angle=90),
        axis.text.y = element_text(face="bold", color="#993333", 
                                   size=12)) +
  scale_x_discrete(name = "Director Name") +
  scale_y_discrete(limits = seq(1, 26, 2), name="No. of movies")+
  geom_text(aes(label=freq), vjust=1.6, color="#000099", size=3.0) +
  ggtitle("Movie Count of Top Directors")


dataSample = data[sample(nrow(data), 100), ]
dataSample
ggplot(data, aes(content_rating,imdb_score), fill = dose) +
  geom_histogram(stat = "identity", position = "dodge", color = "#090901", lwd = 0.7, fill = "#EF0606", width = 0.7) +
  coord_cartesian(ylim = c(0,10))+
  theme(axis.text.x = element_text(face="bold", color="#993333", 
                                   size=10, angle=90),
        axis.text.y = element_text(face="bold", color="#993333", 
                                   size=10)) +
  scale_x_discrete(name = "CONTENT RATING") +
  scale_y_discrete(limits = seq(0,10,0.5), name="IMDB SCORE") +
  ggtitle("IMDB SCORE ON CONTENT RATING")+coord_flip()



set = subset(data, duration > 180 & duration < 300, select = c("duration", "budget"))
set
nrow(set)
cols = c("red")
qplot(duration,  budget, data = set, geom = c("line", "point"), col = cols, xlab = "Duration (in minutes)", ylab = "Budget of movie", main = "Comparision between duration & budget of movie")
set = data[sample(nrow(data), 200), ]
set

plot.stepfun(set$num_user_for_reviews, set$num_critic_for_reviews, xlim = range(c(0, 800)), ylim = range(c(0.1, 1.0)), pch = 16, cex = 0.8, col = "blue", main = "Reviews Over Critics", xlab = "Number of user reviews", ylab = "Number of critic reviews")
abline(lm(set$num_user_for_reviews ~ set$num_critic_for_reviews))

dataSample = data[sample(nrow(data), 10), ]
dataSample
res = subset(dataSample, select = c(director_name,gross))
qplot(director_name,gross, data = res ,colour = "magenta", alpha = I(1), xlab = "Director name", ylab = "Gross", main = "Predict the best director")


set = subset(data, duration > 180 & duration < 300, select = c("duration", "budget"))
set
nrow(set)
cols = c("red")
qplot(duration,  budget, data = set, geom = c("line", "point"), col = cols, xlab = "Duration (in minutes)", ylab = "Budget of movie", main = "Comparision between duration & budget of movie")




