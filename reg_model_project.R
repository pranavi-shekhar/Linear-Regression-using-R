library(dplyr)
library(ggplot2)
library(reshape2)

load("C:/Users/Lakshmi/Desktop/datasciencecoursera/Regression-using-R/movies.Rdata")
 

# Remove actors and links and dates

movies = select(movies,-c(25:32,8:12,6))
attach(movies)

# Combine all Oscar data together for easy plotting - since they share the same categories "yes" and "no" and we are plotting only the counts for yes and no

oscar.data =apply(movies[c("best_pic_nom", "best_pic_win","best_actor_win","best_actress_win", "best_dir_win","top200_box")],2,table)
oscar.data = melt(oscar.data)
names(oscar.data) = c("Decision","Category","Counts")
oscar.data$Decision = tools::toTitleCase(as.character(oscar.data$Decision))
oscar.data$Category = tools::toTitleCase(as.character(gsub("_", " ", oscar.data$Category, fixed = TRUE))) 

# Exploratory Data Analysis

# Basic plotting to observe linearity and trends                  ------------------------------------------------


ggplot(movies,aes(x=imdb_rating,y = seq_along(imdb_rating))) + geom_point(color = "mediumorchid4")

ggplot(movies, aes(x=critics_score,y=imdb_rating)) + geom_point(color="steelblue",size=2)

ggplot(movies, aes(x=runtime,y=imdb_rating)) + geom_point(color="steelblue",size=2)
with(movies,cor(critics_score,imdb_rating))

ggplot(movies, aes(x=genre)) + geom_bar(fill = "mediumorchid4") + coord_flip()+ geom_text(stat = "count", aes(label=..count..))

ggplot(movies, aes(x=title_type)) + geom_bar(fill = "mediumorchid4") + geom_text(stat = "count", aes(label=..count..), vjust=-1) + ylim(0,700)

ggplot(oscar.data, aes(x = Category, y = Counts, fill = factor(Decision),width=0.25)) + geom_bar(stat = "identity") + scale_fill_manual(name = "Legend", values = c("No" = "mediumorchid4","Yes"="cornflowerblue")) + labs(x = "\n\nCategory", y="Counts\n\n")

ggplot(movies, aes(x=top200_box)) + geom_bar(fill = "mediumorchid4")+ geom_text(stat = "count", aes(label=..count..), vjust=-1)+ ylim(0,700)

ggplot(movies, aes(x=mpaa_rating)) + geom_bar(fill = "mediumorchid4")+ geom_text(stat = "count", aes(label=..count..), vjust=-1)+ ylim(0,700)


# Exploring interaction effects                                                            -------------------------------

ggplot(movies, aes(x=critics_score,y=imdb_rating,color = best_actor_win)) + geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE)

ggplot(movies, aes(x=critics_score,y=imdb_rating,color = best_actress_win)) + geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE)

ggplot(movies, aes(x=critics_score,y=imdb_rating,color = genre)) + geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE)

ggplot(movies, aes(x=critics_score,y=imdb_rating,color = mpaa_rating)) + geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE)

# Intercation b/w 2 categorical variables
new = movies %>% group_by(title_type,critics_rating) %>% summarise(mir = mean(imdb_rating))

ggplot(new, aes(title_type, mir)) +
geom_line(aes(group = critics_rating, color = critics_rating)) +
geom_point(aes(color = critics_rating)) +
xlab("\n\nTitle Type") + ylab("Mean IMDB Rating\n\n") +
labs(color = "Critics Rating\n")


# Building the model using backward selection
 
summary(lm(imdb_rating ~ critics_score + critics_rating + genre + best_actor_win + best_actress_win + title_type + mpaa_rating))$adj.r.squared

summary(lm(imdb_rating ~ critics_score + critics_rating + genre + best_actor_win + best_actress_win + title_type))$adj.r.squared

summary(lm(imdb_rating ~ critics_score + critics_rating + genre + best_actor_win + best_actress_win))$adj.r.squared

summary(lm(imdb_rating ~ critics_score + critics_rating + genre + best_actor_win + title_type))$adj.r.squared

summary(lm(imdb_rating ~ critics_score + critics_rating + genre + title_type))$adj.r.squared

summary(lm(imdb_rating ~ critics_score + critics_rating + title_type))$adj.r.squared

summary(lm(imdb_rating ~ critics_score + genre + title_type))$adj.r.squared

summary(lm(imdb_rating ~ critics_rating + genre + title_type))$adj.r.squared

model = lm(imdb_rating ~ critics_score + critics_rating + genre + title_type) 
summary(model)

# Validating assumptions of linear regression

plot(model.final, which=1, pch = 20, col = "steelblue")
plot(model, which=2, pch = 20, col = "steelblue")
hist(resid(model), col= "olivedrab3", xlab = "Residuals")
plot(model, which=3, pch = 20, col = "steelblue")

# Adding Interaction Effects - for explanation and step wise addition of terms look at the RMd/html documentation

model  = lm(imdb_rating ~ critics_score + critics_rating + genre + title_type + critics_score*genre + critics_rating*title_type)

# Diagnostic plots for model with interaction effects

par(mfrow=c(1,2))
plot(model, which=1, pch = 20, col = "mediumorchid4")
plot(model, which=2, pch = 20, col = "steelblue")
par(mfrow=c(1,2))
plot(model, which=3, pch = 20, col = "olivedrab3")
plot(resid(model),col = "mediumorchid4", xlab="Index",ylab = "Residuals",pch=20)


# Prediction

input = list(critics_score=85, genre = "Drama",critics_rating = "Certified Fresh",title_type = "Feature Film")
predict(model,input)
predict(model,input, interval = "prediction",level = 0.95)