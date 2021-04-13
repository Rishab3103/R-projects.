installed.packages("tidyverse")
install.packages("reshape2")
library(reshape2)
library(tidyverse)
library(ggplot2)
install.packages("dplyr")
library(dplyr) 
housing$TotalBsmtSF
housing<-read.csv("train.csv")
housing$GarageCars
features <- c("OverallQual", "LotArea","TotalBsmtSF","GrLivArea","GarageArea", "Fireplaces","GarageCars")
cormat <- round(cor(housing[c("SalePrice", features)], use = "complete.obs"), 2)

melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
factor(housing$Bedroom.AbvGr)
housing%>%
  ggplot(aes(SalePrice))+ geom_density()+facet_wrap(~Neighborhood)
class(housing$Pool.Area)
mean_sale<-mean(housing$SalePrice)
mean_sale
mean_pool_area<-mean(housing$Pool.Area)
housing%>%
  cor(SalePrice,Lot.Area)
Garage_Area<-housing%>%
  select(Garage.Area)

cor(housing$SalePrice, housing$Garage.Area, use = "complete.obs")
cor(housing$SalePrice,housing$Lot.Area)
cor(housing$SalePrice,housing$Pool.Area)
housing%>%
  ggplot(aes(log(SalePrice)))+ geom_density()+ facet_wrap(~Utilities)
housing%>%
  ggplot(aes(Garage.Area,log(SalePrice)))+geom_point()+geom_smooth(method="lm")
housing%>%
  ggplot(aes(Wood.Deck.SF,SalePrice))+ geom_point()+geom_smooth(method="lm")
housing%>%
  ggplot(aes(Enclosed.Porch,SalePrice))+ geom_point()+geom_smooth(method="lm")
housing%>%
  ggplot(aes(BsmtFin.SF.1,SalePrice))+ geom_point()+geom_smooth(method="lm")
housing%>%
  ggplot(aes(X1st.Flr.SF,SalePrice))+ geom_point()+geom_smooth(method="lm")
housing%>%
  ggplot(aes(X2nd.Flr.SF,SalePrice))+ geom_point()+geom_smooth(method="lm")
housing%>%
  ggplot(aes(Screen.Porch,SalePrice))+ geom_point()+geom_smooth(method="lm")
housing%>%
  ggplot(aes(TotRms.AbvGrd,SalePrice))+ geom_point()+geom_smooth(method="lm")
housing%>%
  ggplot(aes(Bsmt.Unf.SF,SalePrice))+ geom_point()+geom_smooth(method="lm")
housing%>%
  ggplot(aes(Bedroom.AbvGr,SalePrice))+ geom_point()+geom_smooth(method="lm")
housing%>%
  ggplot(aes(Gr.Liv.Area,SalePrice))+ geom_point()+geom_smooth(method="lm")
housing%>%
  ggplot(aes(SalePrice))+ geom_density()+facet_wrap(~House.Style)
housing%>%
  ggplot(aes(SalePrice, color=Garage.Type))+  geom_boxplot()
housing%>%
  ggplot(aes(SalePrice, color=Street))+ geom_boxplot()
housing%>%
  ggplot(aes(SalePrice, color=Lot.Config))+ geom_boxplot()
housing%>%
  ggplot(aes(SalePrice, color=Roof.Style))+ geom_boxplot()
housing%>%
  ggplot(aes(SalePrice, color=Land.Slope))+ geom_boxplot()
housing%>%
  ggplot(aes(SalePrice, color=Sale.Condition))+ geom_boxplot()
housing%>%
  ggplot(aes(SalePrice, color=Kitchen.Qual))+ geom_boxplot()
summary(housing)
housing%>%
  ggplot(aes(Lot.Frontage,SalePrice))+geom_point()+geom_smooth(method="lm")

housing$Total.Bsmt.SF
cor(housing$SalePrice,housing$Total.Bsmt.SF, use = "complete.obs")
kitchen<-lm(SalePrice~Kitchen.Qual, data = housing)
summary(kitchen)
plot(kitchen)
housing%>%
  ggplot(aes(Total.Bsmt.SF,SalePrice))+ geom_point()+geom_smooth(method="lm")
housing%>%
  ggplot(aes(log(SalePrice)))+ geom_histogram()+geom_density()
housing_filtered<-housing%>%
  select( SalePrice,GarageArea,GrLivArea,Fireplaces,TotalBsmtSF,OverallQual,LotArea,GarageCars,Neighborhood, MSZoning, Condition1,  SaleCondition, SaleType)%>%
  filter(!is.na(MSZoning))%>%
  filter(!is.na(Condition1))%>%
  filter(!is.na(SaleCondition))%>%
  filter(!is.na(SaleType))

housing_filtered%>%
  count(is.na(Garage.Area))
housing_filtered%>%
  count(is.na(X1st.Flr.SF)) 
housing_filtered%>%
  count(is.na(BsmtFin.SF.1))  
housing_filtered%>%
  count(is.na(Sale.Condition))
housing_filtered%>%
  count(is.na(Roof.Style))
housing_filtered%>%
  count(is.na(Overall.Qual))
housing_filtered%>%
  count(is.na(Land.Slope))
housing_filtered%>%
  count(is.na(Garage.Type))

regression_data<-lm(data = housing_filtered, SalePrice~GarageArea+GrLivArea+Fireplaces+TotalBsmtSF+ OverallQual+ LotArea+GarageCars)
regression_Zoning<-lm(data=housing_filtered, SalePrice~MSZoning)
zoning_condition<-lm(data = housing_filtered,SalePrice~MSZoning*Condition1)
regression_Condition1<-lm(data=housing_filtered, SalePrice~Condition1)
regression_SaleType<-lm(data=housing_filtered, SalePrice~SaleType)
regression_neighborhood<-lm(data=housing_filtered,SalePrice~Neighborhood)
plot(lm(data=housing_filtered,SalePrice~GarageArea*GrLivArea*Fireplaces*TotalBsmtSF*OverallQual))
regression_salecondition<-lm(data=housing_filtered, SalePrice~SaleCondition)
summary(regression_data)
cor(housing$SalePrice,housing$Full.Bath, use = "complete.obs")
cor(housing$SalePrice,housing$Half.Bath, use="complete.obs")
housing%>%
  count(is.na(Half.Bath))
plot(regression_data)
regression_Garage<-lm(data=housing_filtered, SalePrice~GarageArea)
summary(regression_data)
summary(regression_Zoning)
summary(regression_Condition1)
summary(regression_SaleType)
summary(regression_salecondition)
summary(regression_neighborhood)

plot(regression_Zoning)
plot(regression_Condition1)
plot(regression_SaleType)
plot(regression_salecondition)
housing_filtered%>%
  ggplot(aes(Garage.Area,log(SalePrice)))+geom_point(alpha=0.3)+geom_smooth(method="lm")
housing_filtered_refined<-housing_filtered%>%
  group_by(Fireplaces)%>%
  summarise(SalePrice_median=median(SalePrice))
housing_filtered_refined
ggplot(housing_filtered, aes(x=Fireplaces, y=SalePrice)) +
  geom_point(alpha=1/3) +
  # overwrite data and mapping locally
  geom_point(data=housing_filtered_refined, aes(x=Fireplaces, y=SalePrice_median), col="red") +
  geom_smooth(method="lm", se=FALSE)+labs(title="Regression Graph of Selling Price v No of Fireplaces in a house")
cor(housing_filtered$SalePrice,housing_filtered$Overall.Cond)
factor(housing$MS.SubClass)
cor(housing$SalePrice,housing$Overall.Cond)
factor(housing$SaleType)
median_sale<-median(housing$SalePrice)
housing$fe
housing%>%
  ggplot(aes(TotalBsmtSF,SalePrice))+geom_smooth(method = "lm")+geom_point(alpha=0.3, size=0.5)+labs(title = "Selling Price of a House vs Total Area of Basement in Square Feet")

housing%>%
  ggplot(aes(GarageArea,log(SalePrice)))+geom_smooth(method = "lm")+geom_point(alpha=0.3, size=0.5)+labs(title = "Selling Price of a House vs Garage Area")
housing%>%
  ggplot(aes(log(SalePrice), col=SaleCondition))+geom_boxplot()+labs(title = "Selling Price of a House with respect to sale condition")
install.packages("caret")
install.packages("leaps")
library(caret)
library(leaps)
library(MASS)
full.model1 <- lm(SalePrice ~., data = housing)
step.model1 <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
library(broom)
theme_set(theme_classic())
ggplot(housing_filtered, aes(GarageArea, SalePrice)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = GarageArea, yend = SalePrice), color = "red", size = 0.3)
housing_filtered$predicted_garage<-predict(regression_Garage)
housing_filtered$residual_garage<-residuals(regression_Garage)

#Regression Analysis

ggplot(housing_filtered, aes(x = GarageArea, y = log(SalePrice))) +  # Set up canvas with outcome variable on y-axis
  geom_point(size=0.2)+
  geom_point(aes(y = predicted_garage), col="red", shape = 1)+
  geom_segment(aes(xend =GarageArea, yend = predicted_garage),alpha=0.2)+
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey")+
  geom_point(aes(color = abs(residual_garage)), size=0.5)+
  scale_color_continuous(low = "black", high = "blue") +
  labs(title="Regression Graph for Sales Price vs Garage Area")
  theme_bw()
housing_filtered
install.packages("scales")
library(scales)  
housing_filtered$predicted_GrLivArea<-predict(lm(data = housing_filtered, SalePrice~GrLivArea))
housing_filtered$residual_GrLivArea<-residuals(lm(data = housing_filtered, SalePrice~GrLivArea))
  ggplot(housing_filtered, aes(x = GrLivArea, y = SalePrice)) +  # Set up canvas with outcome variable on y-axis
    geom_point(size=0.2)+
    geom_point(aes(y = predicted_GrLivArea), col="red", shape = 1)+
    geom_segment(aes(xend =GrLivArea, yend = predicted_GrLivArea),alpha=0.2)+
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey")+
    geom_point(aes(color = abs(residual_GrLivArea)), size=0.2)+
    scale_color_continuous(low = "black", high = "blue") +
    scale_y_continuous(labels=scales::unit_format(suffix="k",prefix="$",scale=0.001))+
    guides(color=FALSE)+
    labs(title="Regression Graph for Sales Price vs Houses without basement")+
    theme_ipsum()
housing_filtered$predicted_totalBSMTSF<-predict(lm(data = housing_filtered, SalePrice~TotalBsmtSF))
housing_filtered$residual_totalBSMTSF<-residuals(lm(data = housing_filtered, SalePrice~TotalBsmtSF))
ggplot(housing_filtered, aes(x = TotalBsmtSF, y = SalePrice)) +  # Set up canvas with outcome variable on y-axis
  geom_point(size=0.2)+
  geom_point(aes(y =predicted_totalBSMTSF), col="red", shape = 1)+
  geom_segment(aes(xend =TotalBsmtSF, yend = predicted_totalBSMTSF),alpha=0.2)+
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey")+
  geom_point(aes(color = abs(residual_totalBSMTSF)))+
  scale_color_continuous(low = "black", high = "blue") +
  labs(title="Regression Graph for Sales Price vs Total Basement Area")
theme_bw()  
housing_filtered$predicted_overallqual<-predict(lm(data = housing_filtered, (SalePrice)~OverallQual))
housing_filtered$residual_overallqual<-residuals(lm(data = housing_filtered,(SalePrice)~OverallQual))
ggplot(housing_filtered, aes(x = OverallQual, y = (SalePrice))) +  # Set up canvas with outcome variable on y-axis
  geom_point()+
  geom_point(aes(y = predicted_overallqual), col="red", shape = 1)+
  geom_segment(aes(xend =OverallQual, yend = predicted_GrLivArea),alpha=0.2)+
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey")+
  geom_point(aes(color = abs(residual_overallqual)), size=0.2)+
  scale_color_continuous(low = "black", high = "blue") +
  labs(title="Regression Graph for Sales Price vs Overall Quality")+
  theme_bw()
housing_filtered$predicted_lotarea<-predict(lm(data = housing_filtered, SalePrice~LotArea))
housing_filtered$residual_lotarea<-residuals(lm(data = housing_filtered, SalePrice~LotArea))
ggplot(housing_filtered, aes(x = LotArea, y = SalePrice)) +  # Set up canvas with outcome variable on y-axis
  geom_point(size= 0.4)+
  geom_point(aes(y = predicted_lotarea), col="red", shape = 1)+
  geom_segment(aes(xend =LotArea, yend = predicted_lotarea),alpha=0.2)+
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey")+
  geom_point(aes(color = abs(residual_lotarea)), size=0.2)+
  scale_color_continuous(low = "black", high = "blue") +
  labs(title="Regression Graph for Sales Price vs Lot Area")+
  theme_bw()
housing_filtered$
housing_filtered$predicted_zoning<-predict(regression_Zoning)
housing_filtered$residual_zoning<-residuals(regression_Zoning)
ggplot(housing_filtered, aes(x = MSZoning, y = SalePrice)) +  # Set up canvas with outcome variable on y-axis
  geom_point(size= 0.4)+
  geom_point(aes(y = predicted_zoning), col="red", shape = 1)+
  geom_segment(aes(xend =MSZoning, yend = predicted_zoning),alpha=0.2)+
  geom_smooth(method = "lm", se = FALSE, color = "red")+
  geom_point(aes(color = abs(residual_zoning)), size=0.2)+
  scale_color_continuous(low = "black", high = "blue") +
  labs(title="Regression Graph for Sales Price vs Zoning")+
  theme_bw()
plot(regression_data)

#zoning_condition<-lm(data = housing_filtered,SalePrice~)
install.packages("olsrr")
library(olsrr)
regression_data<-lm(data = housing_filtered, SalePrice~GarageArea+GrLivArea+Fireplaces+TotalBsmtSF+ OverallQual+ LotArea+GarageCars)
summary(regression_data)
R<-ols_step_all_possible((regression_data)$R-Square)
model1<-lm(data = housing, SalePrice~GarageArea+GrLivArea)
ols_step_all_possible(housing)
summary(lm(data = housing, SalePrice~MSZoning*SaleType*SaleCondition))

housing
summary(lm(data=housing, Fireplaces~Neighborhood))

set1<-housing%>%
  select(Neighborhood,SalePrice,GarageArea,GrLivArea,Fireplaces,TotalBsmtSF,OverallQual,LotArea,GarageCars)%>%
  filter(SalePrice>=median(SalePrice))%>%
  arrange(desc(SalePrice))%>%
  filter(SalePrice>425000)
ggplot(data = set1, aes(SalePrice))+ geom_histogram()+ facet_wrap(~Neighborhood)
summary(lm(data = set1, SalePrice~Neighborhood))

summary(lm(data = housing, SalePrice~Neighborhood))
install.packages("lattice")
create.stripplot(data=housing, SalePrice~Neighborhood)
install.packages("car")
library(car)
ols_coll_diag(regression_data)
variable<-c("Garage Area", "Ground Living Area","Fireplace", "Total Basement Area","Overall Quality", "Lot Area", "Garage Cars")
vif_quant<-car::vif(regression_data)
vif_qual<-car::vif(lm(data = housing, SalePrice~Neighborhood+MSZoning+Condition1+SaleCondition+SaleType ))
vif_qual
vif_variable_qual<-c("Neighborhood","MSZoning","Condition1","SaleCondition","SaleType")
vif_variable_qual
vif_variable_qual_df<-data.frame(vif_qual,vif_variable_qual)
vif_df<-data.frame(vif_quant, variable)
ggplot(vif_df, aes(variable,vif_quant)) + geom_col(width = 0.3, color="skyblue", fill=rgb(0.1,0.4,0.5,0.7 )) + labs(title = "Variable Inflation Factor")+theme_ipsum()
summary(lm(data=housing,SalePrice~SaleType))
ggplot(vif_variable_qual_df, aes(vif_variable_qual,GVIF..1..2.Df..)) + geom_col(width = 0.3, color="skyblue", fill=rgb(0.1,0.4,0.5,0.7 )) + labs(title = "Generalized Variable Inflation Factor", x="Variables", y="GVIF^(1/(2*df))")+theme_ipsum()

r_squared<-c(0.7685,0.5838)
var<-c("Quantitative", "Qualitative")
r_squared_df<-data.frame(r_squared,var)
ggplot(r_squared_df, aes(var,r_squared)) + geom_col(width = 0.3, color="skyblue", fill=rgb(0.1,0.4,0.5,0.7 )) + labs(title = "Comparison of the R-Squared values of the Independent Variables")+theme_ipsum()
summary(lm(data=housing, SalePrice~Neighborhood+MSZoning+Condition1+SaleCondition+SaleType))       
predict_df<-data.frame(predict_quant,predict_qual)
r_squared_quant<-c(0.3887,0.5021,0.218,0.3765,0.6257,0.4101,0.06961)
var_quant<-c("Garage Area", "Ground Living Area","Fireplace", "Total Basement Area","Overall Quality","Garage Cars","Lot Area")
r_squared_quant_df<-data.frame(r_squared_quant, var_quant)
ggplot(r_squared_quant_df, aes(var_quant,r_squared_quant)) + geom_col(width = 0.3, color="skyblue", fill=rgb(0.1,0.4,0.5,0.7 )) + labs(title = "Comparison of the R-Squared values of the Quantitative Variables")+theme_ipsum()
r_squared_qual<-c(0.5456,0.1076,0.03263,0.1355,0.1373)
variable_qual<-c("Neighborhood","MS Zoning","Condition1","Sale Condition", "Sale Type")
r_squared_qual_df<-data.frame(r_squared_qual, variable_qual)
ggplot(r_squared_qual_df, aes(variable_qual,r_squared_qual)) + geom_col(width = 0.3, color="skyblue", fill=rgb(0.1,0.4,0.5,0.7 )) + labs(title = "Comparison of the R-Squared values of the Categorical Variables")+theme_ipsum()
plot(lm(data=housing, SalePrice~Neighborhood))
install.packages("hrbrthemes")
library(hrbrthemes)
housing$PavedDrive
summary(lm(data=housing, SalePrice~MiscVal))
housing$WoodDeckSF

residuals_quant<-residuals(lm(data=housing, SalePrice~GarageArea+GrLivArea+Fireplaces+TotalBsmtSF+ OverallQual+ LotArea+GarageCars))
predicted_quant<-predict(lm(data=housing, SalePrice~GarageArea+GrLivArea+Fireplaces+TotalBsmtSF+ OverallQual+ LotArea+GarageCars))
residuals_qual<-residuals(lm(data=housing, SalePrice~Neighborhood+MSZoning+Condition1+SaleCondition+SaleType))
predicted_qual<-predict(lm(data=housing, SalePrice~GarageArea+GrLivArea+Fireplaces+TotalBsmtSF+ OverallQual+ LotArea+GarageCars))
residualvpredict<-data.frame(residuals_quant,predicted_quant,residuals_qual,predicted_qual)
ggplot(data=residualvpredict, aes(residuals_quant, predicted_quant))+geom_point()+geom_smooth(method="lm")

plot(lm(data=housing, SalePrice~GarageArea+GrLivArea+Fireplaces+TotalBsmtSF+ OverallQual+ LotArea+GarageCars))+theme_ipsum()

plot(lm(data=housing, SalePrice~Neighborhood+MSZoning+Condition1+SaleCondition+SaleType))


