
library(ggplot2)
installed.packages("gapminder")
library("gapminder")
diamonds
mpg
nrow(mpg)
cor(mpg$displ,mpg$hwy)
ggplot(mpg, aes(displ,hwy))+ geom_point(alpha=0.5)

lm(mpg$hwy~mpg$displ)


#ggplot(mpg, aes(displ,hwy))+ geom_smooth(method = loess(formula = mpg$hwy~mpg$displ))
ggplot(mpg, aes(hwy)) + geom_histogram(binwidth = 2)
ggplot(mpg, aes(hwy)) + geom_density()
ggplot(mpg, aes(class,hwy)) + geom_boxplot() + coord_flip()
barplot(mpg$class)


mean(economics$unemploy)


rate<-economics$unemploy/economics$pop
rate
economics$unemplo_rate<-rate
ggplot(economics, aes(date,rate)) + geom_line()
