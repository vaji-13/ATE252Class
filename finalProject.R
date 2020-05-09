crash <- read.csv("Crash2015.csv")
totalRows<- nrow(crash)#382181 :Total Rows(Total Crashes)

#calculate the percentage of fatigue drivers involved in crashes which result in injury or fatality
deathInjuryTotal <- sum(crash$Injury.or.Fatal=="Yes")#179338
FatigueddeathInjuryTotal <- sum(crash$Injury.or.Fatal=="Yes" & crash$Fatigue...Asleep=="Yes")#3833
FatigueddeathInjuryTotal*100/deathInjuryTotal#2.13

#make a vector of sleep-related crashes of young driver's Hour of te day
f <- c(crash$Hour.of.Day[crash$Driver.20.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes"],
       crash$Hour.of.Day[crash$Driver.19.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes"],
       crash$Hour.of.Day[crash$Driver.18.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes"],
       crash$Hour.of.Day[crash$Driver.17.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes"],
       crash$Hour.of.Day[crash$Driver.16.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes"])
unique(f)
f <- f[f!= 99] #removing the item equals to 99
#make a histogram of the vector during the hours aof the day
hist(f,xlim=c(-1,24),ylim = c(0,180),breaks=seq(-1,23,1),labels = TRUE,xlab = "Hour of the day",
     ylab="number of crashes",col="lightyellow",main="Fatigue/Asleep-related crashes of young group by hours of the day")
#density histogram
hist(f,xlim=c(-1,24),ylim = c(0,0.1),breaks=seq(-1,23,1),labels = TRUE,xlab = "Hour of the day",
     main="histogram",probability = TRUE)
lines(density(f),col="red")
summary(f)
#calculate the mode
install.packages("modeest")
library(modeest)
mfv(f)#6 most frequent value or mode

#--------------------------------------------------------------------------

#creating a vector for sleep-related crashes of young drivers by hours of the day
youngAsleepHourly  <- NULL

for( i in 0:23){
  
  a <- sum(crash$Hour.of.Day==i & crash$Driver.20.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes",
           crash$Hour.of.Day==i & crash$Driver.19.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes",
           crash$Hour.of.Day==i & crash$Driver.18.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes",
           crash$Hour.of.Day==i & crash$Driver.17.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes",
           crash$Hour.of.Day==i & crash$Driver.16.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes",
           na.rm = TRUE)
  youngAsleepHourly <- c(youngAsleepHourly,a)
}
 #this vector starts from time 0 which means 12 am.
summary(youngAsleepHourly)

#sd(youngAsleepHourly,na.rm = TRUE) = 26.13

# in which hour of the day maximum crashes happened?
which(youngAsleepHourly==max(youngAsleepHourly))-1# minus 1 because the vector started from 0
# 6 oclock
which(youngAsleepHourly==min(youngAsleepHourly))-1
#which shows at 12 ,19 and 21 pm we had the minimum crashes

#what hours of the day crash numbers were less and greater than the mean
which(youngAsleepHourly < mean(youngAsleepHourly))-1
which(youngAsleepHourly > mean(youngAsleepHourly))-1
abline(h=mean(youngAsleepHourly),col="blue")#draw a line to demonstate the Mean value

plot(0:23,youngAsleepHourly,type = "l",
     main = "Asleep-related crashes among Young Drivers \n by Hour of the day",xlab = "Time of the Day",ylab = "Number of Crashes",col="blue")


#creating a plot for sleep-related crashes of "Old" drivers by hours of the day
oldAsleepHourly  <- NULL
for( i in 0:23){
  
  a <- sum(crash$Hour.of.Day==i & crash$Driver.65...74.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes",
           crash$Hour.of.Day==i & crash$Driver.75.Plus=="Yes" & crash$Fatigue...Asleep=="Yes",
           na.rm = TRUE)
  oldAsleepHourly <- c(oldAsleepHourly,a)
}

summary(oldAsleepHourly)

sum(oldAsleepHourly)#950
plot(0:23,oldAsleepHourly,type = "l",main = "Total Old(65+) years old Drivers which reported fatigue or asleep",xlab = "Time of the Day",ylab = "",col="red")


#----------------------------------------------------------------------------------------------
#calculate the number of sleep-related crashes for each age group
sleepNum16to20 <- sum(crash$Driver.16.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes",
                      crash$Driver.17.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes",
                      crash$Driver.18.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes",
                      crash$Driver.19.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes",
                      crash$Driver.20.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes")#1329

sleepNum50to64 <- sum(crash$Driver.50...64.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes")#1755
sleepNum65to74 <- sum(crash$Driver.65...74.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes")#601
sleepNum75plus<- sum(crash$Driver.75.Plus=="Yes" & crash$Fatigue...Asleep=="Yes")#350
sleepNum21to49 <- totalSleep-(sleepNum16to20+sleepNum50to64+sleepNum65to74+sleepNum75plus)#3746
totalSleep <- sum(crash$Fatigue...Asleep=="Yes")#7781

#calculate the number of overall crashes for each age group

totalNum16to20<-sum(crash$Driver.16.Years.Old=="Yes",
                    crash$Driver.17.Years.Old=="Yes",
                    crash$Driver.18.Years.Old=="Yes",
                    crash$Driver.19.Years.Old=="Yes",
                    crash$Driver.20.Years.Old=="Yes")#70822

totalNum50to64 <- sum(crash$Driver.50...64.Years.Old=="Yes")#109986
totalNum65to74 <- sum(crash$Driver.65...74.Years.Old=="Yes")#39729
totalNum75plus <- sum(crash$Driver.75.Plus=="Yes")#24417
totalNum21to49 <- totalRows-(totalNum16to20+totalNum50to64+totalNum65to74+totalNum75plus)#137227

#making a summary table for young and old age group to plot 

name <- c("Age16 to 20","Age 21 to 49","Age 50 to 64","Age 65 plus")
num <- c(sleepNum16to20,sleepNum21to49,sleepNum50to64,(sleepNum65to74+sleepNum75plus))
percentage <- 100*c(sleepNum16to20/(totalNum16to20),sleepNum21to49/totalNum21to49,sleepNum50to64/totalNum50to64,(sleepNum65to74+sleepNum75plus)/(totalNum65to74+totalNum75plus))
df2 <- data.frame(name,num,percentage)
df2
#make some plots
mybar<- barplot(df2$num,names.arg = df2$name,main = "Number of Fatigue/Sleep-related Creashes by Driver's Age",
                col="lightblue",ylim = c(0,4200))
text(x=mybar,y=df2$num,labels=df2$num,cex=1,pos=3)#Add text at top of bars(labels)


mybar2<- barplot(df2$percentage,names.arg = df2$name,main = "Percentage of Fatigue/Sleep-related Creashes to Overall Crashes \nby Driver's Age ",
                col=c("lightblue", "red","cornsilk","lightblue"),ylim = c(0,3.2),space = 1.5)
text(x=mybar2,y=df2$percentage,labels=paste("%",format(df2$percentage,digits = 2)),cex=1.5,pos=3)#Add text at top of bars(labels)

#run the hypothesis test
#first make a matrix of data 
sleepNum16to20#1329
totalNum16to20-sleepNum16to20#69493
sleepNum65to74+sleepNum75plus#951
(totalNum75plus+totalNum65to74)-(sleepNum65to74+sleepNum75plus)#63195


k <- matrix(c(1329,69493,951,63195),ncol=2,byrow=TRUE)
colnames(k) <- c("sleeprelated","non-sleeprelated")
rownames(k) <- c("young","old")
k <- as.table(k)
k
prop.test(k,correct = FALSE)
#so it shows the 95% confidence interval estimates the difference between the 
#two proportions is between 0.018% and0.014%.
#the p-value is very small so we can reject the Null Hyp(the proportions are equall),
#indicates the proportions of the characteristic studied are statistically significantly different in the 2 groups.
#and  since the proportion in first group is greater than the secong,we can concluse
#the young drivers more likely to have a sleep-related crash than old Drivers
#prop.test(k,correct = FALSE,alternative = "greater")

#----------------------------------------------------------------------------------
#make a shiny app

#first make a vector for 21 to 29 years age group to use in the shiny app
t <- crash[!crash$Fatigue...Asleep== "No" & !crash$Fatigue...Asleep=="NULL", ]
t <- t[!t$Driver.16.Years.Old=="Yes" &
         !t$Driver.17.Years.Old=="Yes" &
         !t$Driver.18.Years.Old=="Yes" &
         !t$Driver.19.Years.Old=="Yes" &
         !t$Driver.20.Years.Old=="Yes" &
         !t$Driver.50...64.Years.Old=="Yes" &
         !t$Driver.65...74.Years.Old=="Yes" &
         !t$Driver.75.Plus=="Yes" , ]
tc <- c(t$Hour.of.Day) 

#--------------------------------------------
install.packages("shiny")
library(shiny)
# Define UI for app 
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sleep/Fatigue related Crashes in PA during 2015 to 2017"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel("Please select the Age Group :",
                 selectInput(inputId = "Age",label = "Age Groups",
                             choices = list("age16to20","age21to49","age50to64","age65+"))
                 
                 
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: 
      plotOutput(outputId = "Plot")
      
    )
  )
)


# Define server logic required to draw a plot ----
server <- function(input, output) {
  
  output$Plot <- renderPlot({
    
        if (input$Age=="age16to20") y <- f
        if (input$Age=="age21to49") y <- tc
        if (input$Age=="age50to64") y <- crash$Hour.of.Day[crash$Driver.50...64.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes"]
        if (input$Age=="age65+")    y <- c(crash$Hour.of.Day[crash$Driver.65...74.Years.Old=="Yes" & crash$Fatigue...Asleep=="Yes"],
                                           crash$Hour.of.Day[crash$Driver.75.Plus=="Yes" & crash$Fatigue...Asleep=="Yes"])
   if (y==tc) hist(y,xlim=c(0,25),ylim = c(0,1500),breaks=22,col = "lightgreen",
                   main = " Sleep-related Crashes during the day",
                   labels = TRUE,ylab="Number of Crashes",xlab = "Time of the Day")
        else if (y==f) hist(y,xlim=c(0,25),ylim = c(0,200),breaks=18,col = "#75AADB",
                                       main = " Sleep-related Crashes during the day",
                                       labels = TRUE,ylab="Number of Crashes",xlab = "Time of the Day")
        else
          hist(y,xlim=c(0,25),ylim = c(0,550),breaks=25,col = "pink",
          main = " Sleep-related Crashes during the day",
          labels = TRUE,ylab="Number of Crashes",xlab = "Time of the Day")
    
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
#------------------------------------------------------------------------------------

#make a pie chart of overal crashs by age group
name <- c("Age16 to 20","Age 21 to 49","Age 50 to 64","Age 65 plus")
num <- c(totalNum16to20,totalNum21to49,totalNum50to64,(totalNum65to74+totalNum75plus))
percentage <- 100*c(totalNum16to20/(total),totalNum21to49/total,totalNum50to64/total,(totalNum65to74+totalNum75plus)/total)
df3 <- data.frame(name,num,percentage)
df3
slices <- df3$percentage
lbls <- df3$name
pct <- format(slices,digits = 2)
lbls <- paste(lbls, pct,sep="\n") # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to label
pie(slices, labels = lbls, main="Percentage of Overall Crashes \nby Driver's Age ",
    col = c("lightblue", "red", "purple","cornsilk"))


