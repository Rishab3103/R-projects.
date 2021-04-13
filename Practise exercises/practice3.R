rm=list(ls())
graphics.off()

library(ggplot2)
install.packages("gapminder")
library(gapminder)
library(tidyverse)
gap2007<- gapminder%>%
   filter(year==2007)
class(gap2007$lifeExp)
gap2007
ggplot(data = gap2007)
ggplot(data = gap2007 , mapping = aes(x=gdpPercap, y=lifeExp)

installed.packages("ggplot2")
library(ggplot2)
install.packages("diamonds")
library(diamonds)
diamonds
ggplot(data = diamonds, aes(x= carat, y= price)) + geom_point(size=2, shape=5, alpha= 0.4)
mpg
a<-mpg$displ
b<-mpg$hwy
ggplot(data = mpg, aes(x= displ, y= hwy)) + geom_boxplot(size=2, shape=5)
cor(a,b)
x<-data.frame(mpg)
z<- x[1:1000,]
print(z)

ggplot(data = z, aes(x= displ, y= hwy, color=class)) + geom_point(alpha= 0.4)

lm(hwy~displ, data=z)
