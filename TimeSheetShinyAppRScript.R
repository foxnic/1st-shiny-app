# Access the readr library so we can import the csv file
library(readr)

# Import the csv file. Change the Date column to date format. Change the Week, Month, and Year columns to factors.
TimesheetData <- read_csv("TimesheetData.csv", col_types = cols(Date = col_date(format = "%d/%m/%Y"), Month = col_factor(), Week = col_factor(), Year = col_factor()))

# View the data in the csv file that's been imported
View(TimesheetData)

# Create a new data frame called HoursByWeek containing the total hours by week
HoursByWeek <- aggregate(TimesheetData$Hours, by=list(Category=TimesheetData$Week), FUN=sum)

# Rename the column headings to Week and TotalHours
names(HoursByWeek) <- c("Week", "TotalHours")

# Create a new data frame called HoursByPerson containing total hours by person
HoursByPerson <- aggregate(TimesheetData$Hours, by=list(Category=TimesheetData$Person), FUN=sum)

# Rename the column headings to Person and TotalHours
names(HoursByPerson) <- c("Person", "TotalHours")

# Create a new data frame called HoursByProject containing total hours by project
HoursByProject <- aggregate(TimesheetData$Hours, by=list(Category=TimesheetData$Project), FUN=sum)

# Rename the column headings to Project and TotalHours
names(HoursByProject) <- c("Project", "TotalHours")

# Create a bar chart for HoursByWeek. Start by calling the ggplot2 library. Get the max number of weeks as a variable to use in the customisation of the tick marks to ensure each bar has a tick mark on the x axis.

library(ggplot2)


# MaxWeeks <- max(HoursByWeek$Week)

HoursByWeekPlot <- ggplot(HoursByWeek, aes(x = Week, y = TotalHours)) +
  geom_bar(stat = "identity", fill = "#e1ad46") +
  geom_text(aes(label = TotalHours), vjust = 1.6, color = "white", size = 4.5) +
  ggtitle("Total Hours By Week") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) +  
  theme(axis.text.y = element_blank(),axis.ticks=element_blank()) + 
  theme(axis.text.x = element_text(size=12, color = "#666666")) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) +
  #scale_x_continuous(breaks=seq(1,3,1)) +
  labs(y = "Total Hours")

HoursByWeekPlot


# Create a bar chart for HoursByPerson. 

HoursByPersonPlot <- ggplot(HoursByPerson, aes(x = Person, y = TotalHours)) +
  geom_bar(stat = "identity", fill = "#49a4aa") +
  geom_text(aes(label = TotalHours), vjust = 1.6, color = "white", size = 4.5) +
  ggtitle("Total Hours By Person") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) +  
  theme(axis.text.y = element_blank(),axis.ticks=element_blank()) + 
  theme(axis.text.x = element_text(size=12, color = "#666666")) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) +
  labs(y = "Total Hours")

HoursByPersonPlot

# Create a bar chart for HoursByProject. 

HoursByProjectPlot <- ggplot(HoursByProject, aes(x = Project, y = TotalHours)) +
  geom_bar(stat = "identity", fill = "#dd0244") +
  geom_text(aes(label = TotalHours), vjust = 1.6, color = "white", size = 4.5) +
  ggtitle("Total Hours By Project") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank()) +  
  theme(axis.text.y = element_blank(),axis.ticks=element_blank()) + 
  theme(axis.text.x = element_text(size=12, color = "#666666")) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=14)) +
  labs(y = "Total Hours")

HoursByProjectPlot


