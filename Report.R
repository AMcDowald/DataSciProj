library(xlsx)
library(ggplot2)

report <- read.xlsx("~/Documents/mastercard2.xlsx", sheetName = "Sheet1")
View(report)
report$Date <- as.POSIXlt(report$Ticket.Created..UTC.)
View(df)
View(report$Date)

df <- data.frame(report$Respond.Time, report$Date)

#MISS SLA REPORT
report1 <- read.xlsx("~/Downloads/Applications_Management_2017-07-17.xlsx", sheetName = "Miss SLA")
Date_UTC = as.POSIXlt(report1$Ticket.Created..UTC.)
Respond_Time = report1$Respond.Time
Client = report1$Client
ggplot(report1, aes(x=Date_UTC, y=Respond_Time, group=Client, color=Client)) + geom_point() + geom_line() + 
  scale_colour_manual(values=c("blue", "green", "yellow","red","black","brown")) + 
  ggtitle("Graph of Respond Times")

#Mean Respond Time of Each Shift
Shift_by_ID = report1$ShiftID
ggplot(data=report1, aes(x=Shift_by_ID, y=Respond_Time)) +
  geom_bar(stat="summary", fun.y="mean") + 
  scale_colour_manual(values=c("green", "yellow","red")) +
  coord_flip() +
  ggtitle("Mean Respond Time of Each Shift")

#Sum Respond Time of Each Shift
Shift_by_ID = report1$ShiftID
ggplot(data=report1, aes(x=Shift_by_ID, y=Respond_Time)) +
  geom_bar(stat="summary", fun.y="sum") + 
  scale_colour_manual(values=c("green", "yellow","red")) +
  coord_flip() +
  ggtitle("Sum Respond Time of Each Shift")
#https://stackoverflow.com/questions/20139978/ggplot2-label-values-of-barplot-that-uses-fun-y-mean-of-stat-summary

#SHIFT WITH THE HIGHEST MISSES
Shift_by_ID = as.factor(Shift_by_ID)
ggplot(data=report1,aes(x=Shift_by_ID)) + geom_bar() + geom_text(stat='bin',aes(label=..count..),vjust=-1) +
ggtitle("Shift with the Highest Missed")

#SHIFT WITH THE MOST TICKETS
Tlog <- read.xlsx("~/Downloads/Applications_Management_2017-07-17.xlsx", sheetName = "TLog")
Shift_by_ID = as.factor(Tlog$ShiftID)
tickets = Tlog$Radar.TID

ggplot(Tlog, aes(x=Shift_by_ID)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  scale_colour_manual(values=c("green", "yellow","red")) +
  coord_flip() +
  ggtitle("Most Tickets by  Shift")

#















ggplot(df, aes(df$report.Date, df$report.Respond.Time)) + geom_line() + xlab("Dates") + ylab("Respond Time")

ggplot(report, aes(report$Ticket.Created..UTC.)) + geom_line(aes(y = report$Respond.Time, colour = "y"))





df <- structure(
  list(Cars = structure(c(3L, 2L, 3L, 1L, 2L, 1L), .Label = c("BlueCar", "Greencar", "YellowCar"), class = "factor"), 
       Date = structure(c(1L, 1L, 1L, 1L, 1L, 2L), .Label = c("2017-07-08", "207-07-08"), class = "factor"), 
       Time = structure(c(6L, 1L, 2L, 3L, 4L, 5L), .Label = c("05:01:35", "05:03:31", "05:52:55", "10:21:57", "12:07:51", "17:41:00"), class = "factor"), Speed = c(20L, 30L, 10L, 4L, 2L, 15L)),
       .Names = c("Cars", "Date", "Time", "Speed"), class = "data.frame", row.names = c(NA,-6L))



ggplot(df, aes(x=Time, y=Speed, group=Cars, color=Cars)) + geom_line() + 
  scale_colour_manual(values=c("blue", "green", "yellow"))

RT = report$Respond.Time
TC = report$Ticket.Created..UTC.
labelin <- TC[1:13]
max_y <- max(RT)
plot_colors <- c("blue","red","forestgreen")
new <- plot(RT, type="o", col=plot_colors[2], 
     ylim=c(0,max_y), axes=FALSE, ann=FALSE)

# Make x axis using Mon-Fri labels
axis(1, at=1:13, lab=FALSE)
text(TC, par("usr")[1] - 13, labels = labelin, srt = 90, pos = 1, xpd = TRUE)
axis(2, at=5*0:max_y)


# add a title for the right axis
mtext("y=1/x", side=4, line=3, cex.lab=1,las=2, col="blue")
# Create box around plot
box()
#dev.off()


library(reshape)
library(lattice)
df <- data.frame(date = c("01-04-2001 00:00","01-04-2001 00:00","01-04-2001 00:00",
                          "01-05-2001 00:00","01-05-2001 00:00","01-05-2001 00:00",
                          "01-06-2001 00:00","01-06-2001 00:00","01-06-2001 00:00",
                          "01-07-2001 00:00","01-07-2001 00:00","01-07-2001 00:00"), 
                 id = c(1,2,3,1,2,3,1,2,3,1,2,3), a = c(1,2,3,4,5,6,7,8,9,10,11,12), 
                 b = c(2,2.5,3,3.2,4,4.6,5,5.6,8,8.9,10,10.6))
df2 <- melt(df, id.vars = c("date", "id"), measure.vars = c("a", "b"))

xyplot(value ~ date | variable, group = id, df2, t='l')
