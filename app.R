library(shiny)
library("gdata", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")

#Set directory and load data
setwd("/home/amcdowald/wrksite/DS610/R_app")
theft.data <- read.csv("/home/amcdowald/wrksite/DS610/R_app/data/Theftvehicles_with_time.csv", sep="\t", quote="\"", header = TRUE)
Data_final.data <- read.csv("./data/Data_final.csv")
View(theft.data)
#Define variables
Count_Theft <- Data_final.data$Count_Theft
Count_Subway <- Data_final.data$Count_Subway
Count_Restaurant <- Data_final.data$Count_Restaurant
#Count_Graffiti < - Data_final.data$Count_Graffiti
theft_time <- theft.data$CMPLNT_FR_TM
theft_weekday <- theft.data$Day
theft_dayOrNight <-theft.data$Day_Or_Night
theft_weekday <-ifelse (theft.data$Day == "Monday",0, ifelse(theft.data$Day =="Tuesday", 1, ifelse(theft.data$Day =="Wednesday", 2, ifelse(theft.data$Day =="Thursday", 3, ifelse(theft.data$Day == "Friday", 4, ifelse(theft.data$Day == "Saturday", 5, ifelse(theft.data$Day =="Sunday", 6, NA)))))))
theft_dayOrNight <-ifelse (theft.data$Day_Or_Night == "NIGHT",0, ifelse(theft.data$Day_Or_Night =="DAY", 1, NA))
Crime_Aggregated <- theft.data$Crime_Aggregated
print(theft_weekday)
print(theft.data$Day)
caption <- "THIS IS WEIRD"
# theft_weekday <- as.factor(theft_weekday)
# theft_dayOrNight <- as.factor(theft_dayOrNight)

server <- shinyServer(
  function(input, output) {
    output$plot_1 <- renderPlot({
      data <- switch(input$var, 
                     "theft_weekday" =  theft_weekday,
                     "Crime_Aggregated" = Crime_Aggregated
      )
      data2 <- switch(input$var2, 
                     "theft_weekday" =  theft_weekday,
                     "Crime_Aggregated" = Crime_Aggregated
      )
    
      
      logit <- glm(formula = theft_dayOrNight ~ data2+data, data=theft.data, family="gaussian"(link="identity"))
      output$table1 <- renderTable(summary(res))
      res <- residuals(logit)
      hist(res,prob=T,breaks=9, xlab="Residuals", main="Histogram of the residuals" )
      
     
    })}
  )

ui <- shinyUI(
    fluidPage( titlePanel(p("My Application" ,align = "center")),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        p("HELLO")
      ), mainPanel(
        tabsetPanel(
          tabPanel("Plot", 
        p("Knowing theft rate for street segments where a customer parks his/her car on
#     weekdays during work hours, or in the evenings and nights close to home where
#     he/she lives, makes insurance price more precise. Also, knowing habits of customer
#     in spending weekend days and nights, as well as his/her age, helps to make
#     insurance price tailored for every single case. Based on the lifestyle and habits of
#     Manhattan residents and visitors, we distinguish six time and day ranges (for
#     instance, 8am-6pm weekdays work hours, 6-7am and 7-8pm weekdays commute
#     hours, etc.). We calculate theft rate for these six time and day ranges. Though, for a
#     more precise application the theft rate can be calculated for each day and every
#     hour, using the same methodology. For instance, in the graphs below we provide
#     theft rate maps for two time and day ranges, where grey lines demonstrate streetsegments
#     with rate zero or close to zero, thick black lines correspond to high rate,
#     and thick purple and especially red lines show the highest rate of theft from motor
#     vehicle:", style = "font-family: 'times'; font-si16pt")),
        tabPanel("Table", tableOutput("table1"))
        )
        
      )
  )
  ),  fluidRow(
      sidebarLayout(
        sidebarPanel(
          selectInput("var", 
                    label = "Choose a variable to display",
                    choices = c("theft_weekday", "Crime_Aggregated"),
                    selected = "Crime_Aggregated"),
          selectInput("var2", 
                      label = "Choose a variable to display",
                      choices = c("theft_weekday", "Crime_Aggregated"),
                      selected = "theft_weekday")
        ),
        mainPanel(plotOutput("plot_1"))
      )
))
)
# server <- function(input, output) {
#   output$text1 <- renderText({ 
#     paste("You have selected", input$var)
#   })
#   
#   output$text2 <- renderText({ 
#     paste("You have chosen a range that goes from",
#           input$range[1], "to", input$range[2])
#   })
#   
#   #output$distPlot <- renderPlot({
#     #hist(rnorm(input$obs), col = 'darkgray', border = 'white')
#   #})
# }
# 
# ui <- fluidPage(
#   title = "TITLE OF THE WINDOW", theme = "bootstrap.css",
#   titlePanel("Hello team!"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       helpText("Create demographic maps with 
#         information from the 2010 US Census."),
#                  
#                  selectInput("var", 
#                              label = "Choose a variable to display",
#                              choices = c("Percent White", "Percent Black",
#                                          "Percent Hispanic", "Percent Asian"),
#                              selected = "Percent White"),
#                  
#                  sliderInput("range", 
#                              label = "Range of interest:",
#                              min = 0, max = 100, value = c(0, 100))
#       
#     ),
#     
#     mainPanel(
#     h1("Implementation of your application / Screenshots if possible:",align = "center") ,
#     
#     p("Knowing theft rate for street segments where a customer parks his/her car on
#     weekdays during work hours, or in the evenings and nights close to home where
#     he/she lives, makes insurance price more precise. Also, knowing habits of customer
#     in spending weekend days and nights, as well as his/her age, helps to make
#     insurance price tailored for every single case. Based on the lifestyle and habits of
#     Manhattan residents and visitors, we distinguish six time and day ranges (for
#     instance, 8am-6pm weekdays work hours, 6-7am and 7-8pm weekdays commute
#     hours, etc.). We calculate theft rate for these six time and day ranges. Though, for a
#     more precise application the theft rate can be calculated for each day and every
#     hour, using the same methodology. For instance, in the graphs below we provide
#     theft rate maps for two time and day ranges, where grey lines demonstrate streetsegments
#     with rate zero or close to zero, thick black lines correspond to high rate,
#     and thick purple and especially red lines show the highest rate of theft from motor
#     vehicle:", style = "font-family: 'times'; font-si16pt")
#     
#     #plotOutput("distPlot")
#     )
#   )
# )
# renderPlot({
# data <- switch(input$var, 
#                "Count_Theft" =  numeric(Data_final.data$Count_Theft),
#                "Count_Subway" = numeric(Data_final.data$Count_Subway),
#                "Count_Restaurant" = numeric(Data_final.data$Count_Restaurant)
# )
# hist(input$data ,prob=T,breaks=9, xlab="Age", main="Histogram of Age")}
# )
# server <- shinyServer(
#   function(input, output) {}
# )
# 
# ui <- fluidPage(
#   shinyUI(fluidPage()
#           )
# )
shinyApp(ui = ui, server = server)
