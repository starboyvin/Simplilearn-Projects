
# importing data in R environment
View(Comcast)

# finding missing values first
na_vector <- is.na(Comcast)

length(na_vector[na_vector==T])

# processing date

library(lubridate)

Comcast$Date <- dmy(Comcast$Date)
Comcast$Date

#Extracting Monthly and Daily Ticket Count

library(lubridate)
library(stringi)
library(ggplot2)
library(ggpubr)
library(dplyr)

monthly_count <- summarise(group_by(Comcast,Month =as.integer(month(Date))),Count = n())
daily_count <- summarise(group_by(Comcast,Date),Count =n())
monthly_count <-arrange(monthly_count,Month)
daily_count

# Comparing Monthly and Daily Complaints

library(lubridate)
library(stringi)
library(ggplot2)
library(ggpubr)
library(dplyr)

ggplot(data = monthly_count,aes(Month,Count,label = Count))+
          geom_line()+
          geom_point(size = 0.8)+
          geom_text()+
          scale_x_continuous(breaks = monthly_count$Month)+
          labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")+
          theme(plot.title = element_text(hjust = 0.5))

ggplot(data = daily_count,aes(as.POSIXct(Date),Count))+
          geom_line()+
          geom_point(size = 1)+
          scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
          labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")+
          theme(axis.text.x = element_text(angle = 75),
                plot.title = element_text(hjust = 0.5))

# table with the frequency of complaint types.

network_tickets<- contains(Comcast$Customer.Complaint,match = 'network',ignore.case = T)
internet_tickets<- contains(Comcast$Customer.Complaint,match = 'internet',ignore.case = T)
billing_tickets<- contains(Comcast$Customer.Complaint,match = 'bill',ignore.case = T)
email_tickets<- contains(Comcast$Customer.Complaint,match = 'email',ignore.case = T)
charges_ticket<- contains(Comcast$Customer.Complaint,match = 'charge',ignore.case = T)

Comcast$ComplaintType[internet_tickets]<- "Internet"
Comcast$ComplaintType[network_tickets]<- "Network"
Comcast$ComplaintType[billing_tickets]<- "Billing"
Comcast$ComplaintType[email_tickets]<- "Email"
Comcast$ComplaintType[charges_ticket]<- "Charges"

Comcast$ComplaintType[-c(internet_tickets,network_tickets,
                              billing_tickets,charges_ticket,email_tickets)]<- "Others"

table(Comcast$ComplaintType)

# Categorical varible
open_complaints<- (Comcast$Status == "Open"| Comcast$Status =="Pending")
closed_complaints<-(Comcast$Status == "Closed"| Comcast$Status =="Solved")
Comcast$ComplaintStatus[ open_complaints]<-"Open" 
Comcast$ComplaintStatus[closed_complaints]<- "Closed" 
# Stacked barchart for complaints based on State and Status
Comcast<- group_by(Comcast,State,ComplaintStatus)
chart_data<- summarise(Comcast,Count = n())
ggplot(as.data.frame(chart_data) ,mapping = aes(State,Count))+
          geom_col(aes(fill = ComplaintStatus),width = 0.95)+
          theme(axis.text.x = element_text(angle = 90),
                axis.title.y = element_text(size = 15),
                axis.title.x = element_text(size = 15),
                title = element_text(size = 16,colour = "#0073C2FF"),
                plot.title = element_text(hjust =  0.5))+
          labs(title = "Ticket Status Stacked Bar Chart ",
               x = "States",y = "No of Tickets",
               fill= "Status")
# Georgia state having maximum complaints
chart_data%>%
          filter(ComplaintStatus == "Open")->
          open_complaints
open_complaints[open_complaints$Count == max(open_complaints$Count),c(1,3)]




# Calculating Resolution Percentage based on Total and Catagory

resolved_data <- group_by(comcast_data,ComplaintStatus)
total_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data))) 
resolved_data <- group_by(comcast_data,Received.Via,ComplaintStatus)
Category_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data))) 






# Ploting Pie Chart for Total Resolved Vs Category Resolved
par(mfrow = c(1,2))
total<-ggplot(total_resloved,
              aes(x= "",y =percentage,fill = ComplaintStatus))+
          geom_bar(stat = "identity",width = 1)+
          coord_polar("y",start = 0)+
          geom_text(aes(label = paste0(round(percentage*100),"%")),
                    position = position_stack(vjust = 0.5))+
          labs(x = NULL,y = NULL,fill = NULL)+
          theme_classic()+theme(axis.line = element_blank(),
                                axis.text = element_blank(),
                                axis.ticks = element_blank())
category<-ggplot(Category_resloved,
                 aes(x= "",y =percentage,fill = ComplaintStatus))+
          geom_bar(stat = "identity",width = 1)+
          coord_polar("y",start = 0)+
          geom_text(aes(label = paste0(Received.Via,"-",round(percentage*100),"%")),
                    position = position_stack(vjust = 0.5))+
          labs(x = NULL,y = NULL,fill = NULL)+
          theme_classic()+theme(axis.line = element_blank(),
                                axis.text = element_blank(),
                                axis.ticks = element_blank())
ggarrange(total,category,nrow = 1, ncol = 2)






# #Insights

# As per the above analysis we observe that in the 2nd half of the June month Comcast received
# high amount of complaints in which most of the complaints are releted to internet service 
# issue and the highest amount of complaints are received from the state Georgia. 
# The highest unresolved complaints are reletaed from the state Georgia and the total amount 
# of resolved complaints are 77% in which 38% are received the internet and 39% are from the 
# customer care calls.





















