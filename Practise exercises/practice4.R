rm=(list(ls()))
countries<-c("canada","australia","india")
values<-c(60,35,40)

names(values)<-countries
values["canada"]

getwd()
dir()
trucks<- read.csv("trucks.csv")
head(trucks)
tail(trucks)
str(trucks)
class(trucks$year)
class(trucks$make)

head(trucks)
summary(trucks)
dim(trucks)[1]
ncol(trucks)
nrow(trucks)
hist(trucks$price)




name<-c("Mercury", "Venus", "Earth", "Mars","Jupiter","Saturn","Neptune","Uranus")
type<-c("Terrestrial","Terrestrial","Terrestrial","Terrestrial","Gas","Gas","Gas","Gas")
diameter<-c(0.4,0.98,1.2,4,3,6.5,5,1.2)
rotation<-c(32,34,54,12,65,32,4,5)
rings<-c(F,F,F,F,T,T,T,T)
planets<-data.frame(name,type,diameter,rotation,rings)
summary(planets)
 
?iris
view(iris)

