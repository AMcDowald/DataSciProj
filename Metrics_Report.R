library(xlsx)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
#==============TEST============
#Variables
report <- "/home/amcdowald/wrksite/summer/Statuses_within_the_hour.csv"
status.data <-
  read.csv(
    report,
    sep = ",",
    quote = "\"",
    header = TRUE
  )

View(status.data)
~/wrksite/Status_Change.png
Time=as.POSIXct(status.data$Status.Change.Time)
Time=scale_x_datetime(
  labels = date_format("%Y-%m-%d %H:%M:%S","%H:%M:%S")
  ,  
  breaks = date_breaks("1 minute")
) 

#View(Time)
#MISS SLA REPORT
#Shift_by_ID = as.factor(status.data$ShiftID)
#Date_UTC = as.POSIXlt(Miss_SLA$Ticket.Created..UTC.)
#Respond_Time = Miss_SLA$Respond.Time
#Client = Miss_SLA$Client
Engineer_Status=status.data$Engineer.Status
Respond_Graph = ggplot(status.data, aes(x=Time, y=Engineer_Status, group=status.data$User, color=status.data$User)) + geom_point() + geom_line() + 
  scale_colour_manual(values=c("blue", "green", "yellow","red","black","brown")) + 
  ggtitle("Graph of Respond Times")

Respond_Graph = Respond_Graph + scale_x_datetime(breaks = date_breaks("20 min"), 
                 minor_breaks=date_breaks("1 min"), labels=date_format("%H:%M:%S"))
#png("~/wrksite/Respond_Graph.png",width = 480, height = 480, units = "px")
Respond_Graph
#dev.off()
#====================================
#Variables
oldrep1="~/Downloads/Applications_Management_2017-07-17.xlsx"
report <- "~/Downloads/Applications_Management_2017-07-25.xlsx"
Miss_SLA <- read.xlsx(report, sheetName = "Miss SLA")
Tlog <- read.xlsx(report, sheetName = "TLog")


#MISS SLA REPORT
Shift_by_ID = as.factor(Miss_SLA$ShiftID)
Date_UTC = as.POSIXlt(Miss_SLA$Ticket.Created..UTC.)
Respond_Time = Miss_SLA$Respond.Time
Client = Miss_SLA$Client
Respond_Graph = ggplot(Miss_SLA, aes(x=Date_UTC, y=Respond_Time, group=Client, color=Client)) + geom_point() + geom_line() + 
  scale_colour_manual(values=c("blue", "green", "yellow","red","black","brown")) + 
  ggtitle("Graph of Respond Times")

#Mean Respond Time of Each Shift
Mean_Graph = ggplot(data=Miss_SLA, aes(x=Shift_by_ID, y=Respond_Time, fill=Shift_by_ID)) +
  geom_bar(stat="summary", fun.y="mean") + 
  coord_cartesian(ylim=c(15,30)) +
  ggtitle("Mean Respond Time of Each Shift")

#Sum Respond Time of Each Shift
Sum_Graph = ggplot(data=Miss_SLA, aes(x=Shift_by_ID, y=Respond_Time)) +
  geom_bar(stat="summary", fun.y="sum") + 
  scale_colour_manual(values=c("green", "yellow","red")) +
  coord_flip() +
  ggtitle("Sum Respond Time of Each Shift")

#SHIFT WITH THE HIGHEST MISSES
Shift_by_ID = as.factor(Miss_SLA$ShiftID)
Shift_SLA_Graph = ggplot(data=Miss_SLA,aes(x=Shift_by_ID)) + geom_bar() + geom_text(stat='bin',aes(label=..count..),vjust=-1) +
  ggtitle("Shift with the Highest Missed")

png("~/wrksite/report1.png",width = 480, height = 480, units = "px")
Respond_Graph
dev.off()

png("~/wrksite/report2.png",width = 480, height = 480, units = "px")
Mean_Graph
dev.off()

png("~/wrksite/report3.png",width = 480, height = 480, units = "px")
Sum_Graph
dev.off()

png("~/wrksite/report4.png",width = 480, height = 480, units = "px")
Shift_SLA_Graph
dev.off()

# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Respond_Graph, vp = vplayout(1, 1:2))  # key is to define vplayout
# print(Team_Dist, vp = vplayout(2, 1:2))
#print(Sum_Graph, vp = vplayout(2, 2))
#=============================================




#TLOG
#SHIFT WITH THE MOST TICKETS
Shift_by_ID = as.factor(Tlog$ShiftID)
tickets = Tlog$Radar.TID
Most_Tickets= ggplot(Tlog, aes(x=Shift_by_ID)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  scale_colour_manual(values=c("green", "yellow","red")) +
  coord_flip() +
  ggtitle("Most Tickets by  Shift")

  
#Users with the most tickets

Owners = as.factor(Tlog$Owner)
tickets = Tlog$Radar.TID
Team_Dist=ggplot(Tlog, aes(x=Owners)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  coord_flip() +
  ggtitle("Tlog Ticket Distribution")
  
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(2, 2)))
# vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print(Shift_SLA_Graph, vp = vplayout(1, 1))  # key is to define vplayout
# print(Most_Tickets, vp = vplayout(1, 2))
# print(Mean_Graph, vp = vplayout(2, 1))
# print(Sum_Graph, vp = vplayout(2, 2))

png("~/wrksite/report5.png",width = 480, height = 480, units = "px")
Most_Tickets
dev.off()

png("~/wrksite/report6.png",width = 480, height = 480, units = "px")
Team_Dist
dev.off()
#Get the users logged in on the day


# input a script
#source("~/Metrics_Report.R") 
# direct output to a file
#sink("report.pdf", append=FALSE, split=TRUE)
# return output to the terminal
#sink()


