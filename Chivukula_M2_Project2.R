#1
name <- "Plotting Basics: Chivukula"
print(name)

#2
install.packages("FSA")
library(FSA)
install.packages("FSAdata")
library(FSAdata)
install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)
install.packages("plotrix")
library(plotrix)
install.packages("ggplot2")
library(ggplot2)
install.packages("moments")
library(moments)

#3
df_bulltroutrml2 <- read.csv("BullTroutRML2.csv")
print(df_bulltroutrml2)
str(BullTroutRML2)

#4
headtail(df_bulltroutrml2, n=3)

#summary
summary(df_bulltroutrml2)


#5
df_filtered <- filterD(df_bulltroutrml2, lake=="Harrison")
print(df_filtered)
str(df_filtered)

#6
headtail(df_filtered, n=5)

#7
structure(df_filtered)

#8
summary(df_filtered)

#9
plot(age~fl, data = df_filtered, xlim = c(0,500), 
     ylim = c(0,15), ylab = "Age (yrs)", xlab = "Fork Length (mm)", 
     main ="Plot 1: Harrison Lake Trout", pch = 19, col='black')

#10
age <- df_filtered$age
hist(age, main = "Plot 2: Harrison Fish Age Distribution", 
     ylab = "Frequency", xlab = "Age (yrs)", xlim = c(0,15), 
     ylim = c(0,15), col = "cadetblue", col.main="cadetblue", 
     border="Green", breaks=10)

#11
age2 = age/mean(age)
ggplot(data=df_filtered, aes(y=age)) +geom_density()+
  geom_point(data=df_filtered, aes(y=age, x=fl,size=age2),alpha = age2,
             color="green") +lims(x = c(0,500),y=c(0,15))+
  labs(title = "Plot 3: Harrison Density Shaded by Era"
       ,x = "Fork Length(mm)",y = "Age(yrs)")

#12 
tmp = headtail(df_filtered, n = 3)
print(tmp)

#13 
tmp$era

#14 
pchs<-c(3,4)

#15 
cols<-c("red","gray60")

plot(df_bulltroutrml2)
boxplot(df_bulltroutrml2)

#16
num=as.numeric(tmp$era)
print(num)

#17 
cols[tmp$era]

#18 
plot(age~fl,data=df_filtered,main="Plot 4: Symbol & color by Era",
     xlim=c(0,500),ylim=c(0,15),ylab="Age(yrs)",
     xlab="Fork Length(mm)",pch=pchs,col=cols)

#19 
plot(age~fl,data=df_filtered,main="Plot 5: Regression Overlay",
     xlim=c(0,500),ylim=c(0,15),ylab="Age(yrs)",
     xlab="Fork Length(mm)",pch=pchs,col=cols)
regression_line=lm(age~fl,data=df_filtered)
abline(regression_line,lty=2,lwd=2)

#20 
plot(age~fl,data=df_filtered,main="Plot 6: Legend Overlay", 
     xlim=c(0,500),ylim=c(0,15),ylab="Age(yrs)", 
     xlab="Fork Length(mm)",pch=pchs,col=cols)
regression_line = lm(age~fl,data=df_filtered)
abline(regression_line,lty=2,lwd=2)
legend("topleft", inset=c(0.05), legend=levels(df_filtered$era), 
       pch=pchs, col=cols,bty="n",title="Era",cex = 0.70)
