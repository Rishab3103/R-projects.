# intro to data visualizaion
# in R: ggplot2
rm(list = ls())
graphics.off()

library(ggplot2) # ggplot2 part of tidyverse
# wickhar() -  a layered grammar of graphics
library(tidyverse)
# Elements of grammar of graphics:
# data, aesthetics and geom: geometric object (type of plot)

# what is the relation between GDP per capita and life expentance?
install.packages("gapminder")
library(gapminder)
gapminder

gap2007 <- gapminder%>%
  filter(year==2007)
gap2007

# Types of plots
# scatterplot, linegraph,  histrogram, boxplot, barplot

?mpg
str(mpg)
?cor
cor(mpg$displ, mpg$hwy)

ggplot(mpg, 
       aes(x = displ, y = hwy, color = class)) +
  geom_point()

?lm
lm(lm(hwy ~ displ, mpg))

# template
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point()+
  geom_smooth()

ggplot(data = mpg, mapping = aes(x=hwy)) + geom_histogram()
ggplot(data = mpg, mapping = aes(x=hwy, color=class)) + geom_density()
str(mpg)

ggplot(data = mpg, mapping = aes(x=class, y=hwy)) + geom_boxplot() +
  labs(title = "Title",
       subtitle = "Subtitle",
       x="X Axis",
       y="Y Axis")

ggplot(data = mpg, mapping = aes(x=class)) + geom_bar() +
  labs(title = "Title",
       subtitle = "Subtitle")


ggplot(data = mpg, mapping = aes(x=class, y=hwy)) + geom_col() +
  labs(title = "Title",
       subtitle = "Subtitle")
str(mpg)
economics
?economics
meanOfEconomics <- mean(economics$unemploy)
economics$unemploymentRate <- economics$unemploy/economics$pop
economics
ggplot(data = economics, mapping = aes(x=date, y=unemploymentRate)) + geom_line() +
  labs(title = "Title",
       subtitle = "Subtitle")


# dplyr (part of the tidyverse)
# R4-Data-Manipulation
# housekeeping
rm(list=ls())
graphics.off()
#introduction
#create new variables or summaries
#rename variables, apply transformations

library(nycflights13)
flights

filter(flights, month == 11, day == 13)
idx <- flights$month ==2 | flights$month ==3
flights[idx,]

my_df <- data.frame(x = c(1, NA, 1:10, NA))
filter(my_df, !is.na(x))

#pipe operator %>% ctrl+shift+m