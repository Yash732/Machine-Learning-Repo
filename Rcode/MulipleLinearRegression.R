library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(reshape)

#Linear Regression Data
incomeDS = read.csv("D:/codes/datasets/income.data.csv")
print(head(incomeDS))

model<-lm(happiness~income,data =incomeDS)
summary(model)

pred1<-predict(model)
print(pred1)

#homoscedasticity
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

#graph plot
income_graph<-ggplot(incomeDS,aes(x =income,y =happiness))+geom_point()
income_graph<-income_graph + geom_smooth(method = "lm",col= "red") +stat_regline_equation(label.x=3,label.y=7)+ theme_bw()+labs(title= "Income vs. Happiness Graph",x = "Income(in 10,000$",y =" Happiness(0to 10)" )
income_graph


#Multiple Linear Regression Data
heartDS<- read.csv("D:/codes/datasets/heart.data.csv")
head(heartDS)
summary(heartDS)

model2<-lm(heart.disease~biking + smoking,data=heartDS)

#check homoscedasticity
par(mfrow =c(2,2))
plot(model2)
par(mfrow=c(1,1))

#prediction
pred2<-predict(model2)
head(pred2)

#graph plot
heart_graph<-ggplot(heartDS,aes(x =smoking,y=heart.disease))+ geom_point()
heart_graph
#rescaling is required for better results

plotting.data<-expand.grid(biking = seq(min(heartDS$biking), max(heartDS$biking), length.out=30),smoking=c(min(heartDS$smoking), mean(heartDS$smoking), max(heartDS$smoking)))
plotting.data$predicted.y <- predict.lm(model2, newdata=plotting.data)
plotting.data$smoking <- round(plotting.data$smoking, digits = 2)
plotting.data$smoking <- as.factor(plotting.data$smoking)

#rescaled plot
heart_plot <- ggplot(heartDS, aes(x=biking, y=heart.disease)) +geom_point() +geom_line(data=plotting.data,aes(x =biking,y=predicted.y,color= smoking),size =1.2) + theme_bw() + labs(title ="Heart Disease as a fn. of Biking and Smoking",x=" Biking %pop",y ="Heart disease %pop")
heart_plot

#wide to long
wide<-data.frame(id= c(1,1,2,2),time= c(1,2,1,2),x1=c(5,3,6,2),x2=c(6,5,1,4))
meltdata<-melt(wide,id = c("id","time"))
print(meltdata)

# casting from long to wide
castdata=cast(wide,id+time~variable+value,mean)
