rm=list(ls())
graphics.off()
library(ggplot2)
diamonds
ggplot(data = diamonds, aes(x=carat, y= price))+ geom_point(alpha=0.5)
mpg
cor(mpg$displ,mpg$hwy)
ggplot(data = mpg, aes(x=displ, y=hwy, color=class)) + geom_point(alpha=0.5)
lm(displ~hwy, data=mpg)
ggplot(data=mpg, aes(x=displ,y=hwy, method="loess")) + geom_smooth(alpha=0.5 )
ggplot(data=mpg, aes(x=hwy, color=class)) + geom_histogram(binwidth =2)
mpg$class
mpg$hwy
ggplot(data=mpg, aes(x=hwy,y=class)) + geom_boxplot() +coord_flip()
barplot(data=mpg$displ, height = 2, xlab = "Display")+coord_flip()
economics
unemploy_rate=economics$unemploy/economics$pop
economics$rate<-unemploy_rate
economics
