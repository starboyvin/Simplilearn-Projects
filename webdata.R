library(readxl)

df = read_excel("internet.xlsx")
df

#understanding of the dataset

summary(internet)

#bounces min=0,max=30
#exit min=0 max=36
#From the result of summarized dataset, it is observed that the numerical data includes 
#information related to the maximum, minimum, and mean data. 
#The categorical data like continent includes the data of the number of times the category 
#has been repeated in the dataset. We can see that there is a maximum value of 30 bounces 
# for the website.
#This site was accessed maximum number of times by visitors from North A








#ques2
#As mentioned earlier, a unique page view represents the number of sessions during which 
# that page was viewed one or more times. A visit counts all instances, no matter how many 
# times the same visitor may have been to your site. So the team needs to know whether the 
# unique page view value depends on visits. 

cor(internet$Uniquepageviews,internet$Visits)

ano<-aov(Uniquepageviews~Visits, data=internet)
summary(ano)  

#We can infer from the results that the visits variable has a significant impact on 
#Unique.Pageviews. So the team can conclude that unique page values depend on visits. 

#ques3
#Find out the probable factors from the dataset, which could affect the exits.
#Exit Page Analysis is usually required to get an idea about why a user leaves the
#website for a session and moves on to another one. Please keep in mind that exits should
#not be confused with bounces

anoo<-aov(Exits~.,data = internet)
summary(anoo)

#ques4
#Every site wants to increase the time on page for a visitor. 
#This increases the chances of the visitor understanding the site content better and
#hence there are more chances of a transaction taking place. 
#Find the variables which possibly have an effect on the time on page. 

anooo<-aov(Timeinpage~.,data = internet)
summary(anooo)

#only source group is not affecting the time in page views rest all are significantly 
# afecting the timein page views


#ques 5

#A high bounce rate is a cause of alarm for websites which depend on visitor engagement.
#Help the team in determining the factors that are impacting the bounce
#this bounce rate is having variables 
#data for the variable bounces has to be between 0 and 1, 

internet$Bounces=internet$Bounces*0.01
rmm<-glm(Bounces~Timeinpage+Continent+Exits+Sourcegroup+Uniquepageviews+Visits,data = internet,family = "binomial")

summary(rmm)

#As can be inferred from the result shown, the BouncesNew, Unique.Pageviews and visits are 
#the variables that impact the target variable bounces
#it has greater significance. 







