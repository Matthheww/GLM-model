#loading required libraries
library("readxl")
library(ggplot2)
library(HSAUR3)
library(Hmisc)

df<- read.csv("C:\\Users\\luke1\\Downloads\\nba_logreg.csv")

#splitting the dataset into train and test
df2<- sort(sample(nrow(df), nrow(df)*.7))

#assigning and creating the two dataframes
dftrain <- df[df2,]
dftest <- df[-df2,]

#check for missing values
sum(is.na(df))

#identifying duplicate rows
df1<-df[duplicated(df), ]

#creating dataframe without duplicate rows
df<-df[!duplicated(df), ]

#correlation matrix is generated to identify the strongest correlations
cor(df[,unlist(lapply(df,is.numeric))])

#specific correlations between dependant and independant variables
cor(df$GP,df$TARGET_5Yrs)
cor(df$MIN,df$TARGET_5Yrs)

#shapiro wilks test is conducted to evaluate normality
shapiro.test(df$GP)
shapiro.test(df$MIN)

#plotting histogram to evaluate normality
ggplot(data = df,aes(x=GP))+geom_histogram(bins = 5,col='black',fill='steelblue2')+ theme_light() + 
  xlab("Games Played") +ylab("Frequency") + ggtitle("Games Played - Histogram")
ggplot(data = df,aes(x=MIN))+geom_histogram(bins=6,col='black',fill='steelblue2')+ theme_light()+
  xlab("Minutes Played")+ ylab("Frequency")+ggtitle("Minutes Played - Histogram")

#plotting target variable distribution as bar chart
ggplot(data = df,aes(x=TARGET_5Yrs))+geom_bar(col='black',fill='steelblue2')+theme_light()+
  xlab("Target 5 Years")+ylab("Count")+ggtitle("Target Variable Distribution")

#convert target variable from numerical to factor
df[,'TARGET_5Yrs']<-factor(df[,'TARGET_5Yrs'])

#creating conditional plots to identify relationships between variables
data("df",package="HSAUR3")
layout(matrix(1:2,ncol=2))
cdplot(TARGET_5Yrs ~ GP, data = df,xlab= "Games Played",ylab="Target Variable",main="Conditional Density Plot - Games Played")
cdplot(TARGET_5Yrs ~ MIN, data= df,xlab='Minutes Played',ylab = 'Target Variable',main="Conditional Density Plot - Miniutes Played")


#creating GLM model with one variable
model1 <- glm(TARGET_5Yrs ~ GP, data = dftrain,family = binomial())

#summary of the created model
summary(model1)

#creating GLM model with two variables
model2 <- glm(TARGET_5Yrs ~ GP + MIN, data = dftrain,family = binomial())

#summary of the created model
summary(model2)

#creating model based on interaction between the two independent variables
model3 <- glm(TARGET_5Yrs ~ GP * MIN, data = dftrain,family = binomial())

summary(model3)

#anova test to choose best model

anova(model1,model2,test = "Chisq")

anova(model1,model3,test="Chisq")

anova(model2,model3,test = "Chisq")

#confidence interval with respect to the model 2
confint(model2,parm = "GP")
confint(model2,parm = "MIN")

#odds ratio with respect to model 2
exp(coef(model2)["GP"])
exp(coef(model2)["MIN"])

#generating predictions based on responses on test data and model2
prediction <- predict(model2,dftest,type = 'response')


#bubble plot
ggplot(data=dftest,aes(x=GP,y=MIN,size=prediction))+geom_point(alpha=0.6,col='salmon4')+ theme_light()+ ggtitle('Bubble Plot - Distribution of Independent Variables based on Model')+
  scale_size(range=c(1,10))+xlab("Games Played")+ylab("Minutes Played")

