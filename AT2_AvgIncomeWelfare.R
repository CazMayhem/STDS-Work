#--------------------------------------------------------------------------------------------
# R code for plotting Welfare data from AT2 by State
# Carol Paipa Sept 2019
#--------------------------------------------------------------------------------------------
# - creating High / Low Income and Welfare Groups
# - plot all postcodes against average income/welfare
# - show top/bottom 20 AverageIncome's - against AverageWelfare
# - Grid to show Tax collected $ to $Welfare by State - not facet_wrap()
# - Create a function to setup the Plots for the final Grid
#--------------------------------------------------------------------------------------------
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("plyr")
#install.packages("devtools")
#install.packages("ggrepel")
#install.packages("ggplotify")

library(tidyverse)
library(ggplot2)
library(modelr)
library(httr)
library(stringr)
library(lubridate)
library(dplyr)
library(devtools)
library(grid)
library(gridExtra)
library(cowplot)
library(ggrepel)
library(readxl)
library(scales)

# Load Postcode, State, Locality
#--------------------------------------------------------------------------------------------
urlPC <- "https://github.com/CazMayhem/welfare-proj/blob/master/LOCALITY_LGA_2016.xlsx?raw=true"
p02 <- tempfile()
GET(urlPC, write_disk(p02<- tempfile(fileext = ".xlsx")))
pc <- as.data.frame(read_excel(p02, sheet = 1, col_names = TRUE))
#convert to data frame
pcDF <- as.data.frame(pc)
pcDF2 <- pcDF %>% rename(Postcode = POSTCODE, State = STATE, Locality = LOCALITY_NAME) %>% mutate(Postcode = as.character(Postcode))
#shrink down to 1 postcode per row
pcDef <- pcDF2 %>% 
  filter(is.na(Postcode)!=TRUE)  %>% 
  select(Postcode, State, Locality) %>% 
  group_by(Postcode) %>% 
  summarise(min(State), min(Locality), n())  %>% 
  rename(State = `min(State)`, Locality = `min(Locality)`) 
pcDef$`n()`  <- NULL
#str(pcDF2)
#str(pcDef)

# Read in Welfare Data prepared by Jose - has Population Proportions
#--------------------------------------------------------------------------------------------
data <- read_csv("https://github.com/CazMayhem/welfare-proj/blob/master/welfaredata.csv?raw=true")
sub <- c(32:61,8:18,5,2,90)
dataset <- data[, -(sub)]

# remove Infinity values (and beyond....)
d0 <- do.call(data.frame, lapply(dataset, function(x) replace(x, is.infinite(x), NA)))

# add in Total Average Income & Welfare for our graphs
#--------------------------------------------------------------------------------------------
d1 <- d0 %>% rename(State = State.x) %>%
  mutate(AvgIncome = mean(AverageIncome), AvgWelfare = mean(Averagewelfare, na.rm=TRUE))

# find extreme ends of Income/Welfare scale
#--------------------------------------------------------------------------------------------
p1 <- d1 %>% filter(Year==2016) %>% filter(Averagewelfare>=15000) %>%
  select(Year, State, Postcode, Averagewelfare, AverageIncome, AvgIncome, AvgWelfare) %>% mutate(Group="High Welfare") 
p2 <- d1 %>% filter(Year==2016) %>% filter(Averagewelfare<=4000) %>%
  select(Year, State, Postcode, Averagewelfare, AverageIncome, AvgIncome, AvgWelfare) %>% mutate(Group="Low Welfare") 
p3 <- d1 %>% filter(Year==2016) %>% filter(AverageIncome>=110000) %>%
  select(Year, State, Postcode, Averagewelfare, AverageIncome, AvgIncome, AvgWelfare) %>% mutate(Group="High Income") 
p4 <- d1 %>% filter(Year==2016) %>% filter(AverageIncome<=28000) %>%
  select(Year, State, Postcode, Averagewelfare,AverageIncome, AvgIncome, AvgWelfare) %>% mutate(Group="Low Income") 

n <- max(length(p1), length(p2), length(p3), length(p4))

length(p1) <- n
length(p2) <- n
length(p3) <- n
length(p4) <- n

# now combine all 4 new data frames into 1 for graphing
pcBind <- rbind(p1,p2,p3,p4)

pcBind <- pcBind %>% mutate(Postcode = as.character(Postcode))
#str(pcBind)
#str(pcDef)

# link welfare data by postcode to LGA dataset for the State or other LGA columns
pcIn <- pcBind %>% inner_join(pcDef, by = "Postcode") %>% rename(State = State.x)  %>% mutate(Suburb = str_c(Postcode, " ", Locality))

# Set up Plot total Income-Welfare for Australia - I'm only doing 2016
#---------------------------------------------------------------------
pg1 <- d1 %>% 
  filter(Year==2016) %>% 
  #arrange(AverageIncome) %>% 
  #top_n(20, Averagewelfare) %>% 
  ggplot(aes(x = Averagewelfare, y = AverageIncome, colour = State)) + 
  geom_point() +
  geom_hline(aes(yintercept=AvgIncome, color = "red") ) +
  geom_vline(aes(xintercept=AvgWelfare, color = "blue")) +
  scale_x_continuous(name="Average Welfare") +
  scale_y_continuous(name="Average Income", labels=comma) +
  guides(fill = guide_legend(nrow=1, label.hjust=0)) +
  theme(legend.position="bottom") +
  labs(title="Average Income-Welfare Australia", 
       caption="Source: data.gov.au/dss")

# Set up Plot Income-Welfare for High-Low Postcodes - I'm only doing 2016
#------------------------------------------------------------------------
pg2 <- pcIn %>% 
  filter(Year==2016) %>% 
  ggplot(aes(x = Averagewelfare, y = AverageIncome, colour = Group)) + 
  geom_point() +
  geom_hline(aes(yintercept=AvgIncome, color = "red") ) +
  geom_vline(aes(xintercept=AvgWelfare, color = "blue")) +
  scale_x_continuous(name="Average Welfare") +
  scale_y_continuous(name="Average Income", labels=comma) +
  theme(legend.position="bottom") + 
  geom_text_repel(aes(label=Suburb), size=3) +
  guides(fill = guide_legend(nrow=1, label.hjust=0)) +
  labs(title="High-Low Welfare-Income Postcodes", 
       caption="Source: data.gov.au/dss")

# now show the above 2 plots on a grid - 1row by 2col
theme_set(theme_cowplot(font_size=10)) # reduce default font size
plot_grid(pg1, pg2)

#--------------------------------------------------------------------
# Plot top/bottom 20 Age Pension postcodes against Avg Income/Pension
# you can use other measures - AverageWelfare, AverageSuperannuation, etc...
#--------------------------------------------------------------------
# 2 temp dataframes
dAvg1 <- d1 %>% filter(Year==2016) %>% arrange(AverageIncome) %>% top_n(20, AverageIncome) 
dAvg2 <- d1 %>% filter(Year==2016) %>% arrange(AverageIncome) %>% top_n(-20, AverageIncome) 

# now combine together - top-to-bottom (tall) to show on final graph
dAvg <- rbind(dAvg1, dAvg2)

#str(dAvg)
#View(dAvg)

# show top-n and bottom-n => Averagewelfare, AverageIncome
#--------------------------------------------------------------------
dAvg %>% 
  filter(Year==2016) %>% 
  #arrange(AverageIncome) %>% top_n(5, Averagewelfare) %>% 
  ggplot(aes(x = Averagewelfare, y = AverageIncome, colour = State, depth=pro_Age.Pension)) + 
  geom_point() +
  geom_hline(aes(yintercept=AvgIncome) ) +  #, color = "red"
  geom_vline(aes(xintercept=AvgWelfare)) +  #, color = "blue"
  guides(alpha = guide_legend(nrow = 1)) +
  theme(legend.position="bottom") +
  scale_y_continuous(labels = comma) +
  geom_text_repel(aes(label=Postcode), size=3) +
  labs(title="Average Income-Welfare Australia", 
       caption="Source: data.gov.au/dss",
       x="Average $ Welfare",
       y="Average $ Income")

#-----------------------------------------------------------------------
# Plot top/bottom 20 Age Pension postcodes via Grid and using a function
#-----------------------------------------------------------------------
plotTopN <- function(df, sFilter, xVal, yVal1, yVal2, title1, xLab, yLab) {
  
  xAxis <- enquo(xVal)
  #yAxis1 <- enquo(yVal1)
  #yAxis2 <- enquo(yVal2)
    #p <- ggplot(df, aes_string(xVal, yVal, group = grVal, colour = grVal))
  fi <- filter(df, State==sFilter) 
  ar <- arrange(fi, -Averagewelfare) 
  tn <- top_n(ar, 20, Averagewelfare)
  ga <- gather(tn, key, value, AverageIncome, Average)
  p <- ggplot(ga, aes(x=xVal, y=value, fill=key, colour=key))
  
  p + geom_bar(stat = "identity", position = "dodge") + 
    theme(axis.text.x = element_text(angle=65, vjust=0.6), legend.position = "bottom") +
    scale_y_continuous(labels = comma) + 
    labs(title=title1, 
         subtitle="",
         caption="Source: data.gov.au",
         x=xLab,
         y=yLab)
}

# call plotTopN() function defined above - set up p1...8 only, show Grid below
p1 <- plotTopN (d1, "NSW", "State", "Income", "Welfare", "Average $Income to $Welfare", "Postcode (NSW)", "$ million")
p2 <- plotTopN (d1, "QLD", "State", "Tax", "Welfare", "Tax collected v's Welfare paid", "Postcode (QLD)", "$ million")
p3 <- plotTopN (d1, "VIC", "State", "Tax", "Welfare", "Tax collected v's Welfare paid", "Postcode (VIC)", "$ million")
p4 <- plotTopN (d1, "ACT", "State", "Tax", "Welfare", "Tax collected v's Welfare paid", "Postcode (ACT)", "$ million")
p5 <- plotTopN (d1, "WA", "State", "Tax", "Welfare", "Tax collected v's Welfare paid", "Postcode (WA)", "$ million")
p6 <- plotTopN (d1, "SA", "State", "Tax", "Welfare", "Tax collected v's Welfare paid", "Postcode (SA)", "$ million")
p7 <- plotTopN (d1, "TAS", "State", "Tax", "Welfare", "Tax collected v's Welfare paid", "Postcode (TAS)", "$ million")
p8 <- plotTopN (d1, "NT", "State", "Tax", "Welfare", "Tax collected v's Welfare paid", "Postcode (NT)", "$ million")

# show above 8 geom_bar p1...8 calls
# if plot_grid fails - free up some memory, eg: close some of the plots showing (all plots)
theme_set(theme_cowplot(font_size=8)) # reduce default font size
plot_grid(p1, p2, p3, p4, p5, p6, p7, p8 )

