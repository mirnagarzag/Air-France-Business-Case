######################################
#Business Case Assignment
######################################

# calling necessary libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)
library(tidyr)
library(scales)
library(flexdashboard)
library(readxl)

# reading the dataset and calling it air_france
air_france <- read_excel("Documents/Hult/SPRING/R/Air France Business Case/Air France Case Spreadsheet Supplement.xls",sheet=2)
kayak <- read_excel("Documents/Hult/SPRING/R/Air France Business Case/Air France Case Spreadsheet Supplement.xls",sheet=3)

View(air_france)
air_france_keywords <- air_france$Keyword
# checking the Variables of interest
table(air_france$`Publisher Name`)
table(air_france$`Match Type`)
table(air_france$`Bid Strategy`)

#descriptive statistics of the columns of interest
summary(air_france$`Engine Click Thru %`)
summary(air_france$`Avg. Pos.`)
summary(air_france$`Total Cost`)
summary(air_france$`Total Volume of Bookings`)

# Checking if columns have any missing values
# The output shows that bid strategy is the only column with missing values
# 1224 missing values so we decided to remove that column entirely
colSums(is.na(air_france))

# Creating own table and transforming percentages to decimals
air_france <- air_france %>%
  select(`Publisher Name`, `Match Type`, Campaign, `Search Engine Bid`, 
         Clicks, `Click Charges`, `Avg. Cost per Click`, 
         Impressions, `Engine Click Thru %`, `Avg. Pos.`, `Trans. Conv. %`, 
         `Total Cost/ Trans.`, Amount, `Total Cost`, 
         `Total Volume of Bookings`) %>%
  mutate(`Engine Click Thru %` = `Engine Click Thru %`/100, 
         `Trans. Conv. %` = `Trans. Conv. %`/100)

# renaming the columns to change names, remove caps and add a underscore for readability 
colnames(air_france) <- c("publisher_name", "match_type", "campaign", 
                          "search_ebid", "clicks", "click_charges", 
                          "avg_cost_p_click", "impressions", 
                          "eng_click_thru_rate", "avg_pos", "trans_conv_rate", 
                          "total_cost_per_trans", "amount", "total_cost", 
                          "total_v_bookings")

# New data sets separated by region
air_us <- air_france[grepl("US", air_france$`publisher_name`) , ]
air_global <- air_france[grepl("Global", air_france$`publisher_name`) , ]


# Creating New Data Sets Dependent by Company
# we decided to collapse global and US for each publisher into one publisher
# since the difference is not required for this analysis
air_yahoo    <-  air_france[grepl("Yahoo", air_france$`publisher_name`) , ]
air_google   <-  air_france[grepl("Google", air_france$`publisher_name`) , ]
air_overture <-  air_france[grepl("Overture", air_france$`publisher_name`) , ]
air_msn      <-  air_france[grepl("MSN", air_france$`publisher_name`) , ]

#creating variables that will be used to measure KPIs
air_france$probability_of_action <- round(air_france$eng_click_thru_rate * 
                                            air_france$trans_conv_rate,
                                          digits = 4)
air_france$net_revenue <- air_france$amount - air_france$total_cost
air_france$ROA <- round(air_france$amount / air_france$total_cost)
# average revenue per booking in dollars
air_france$Avg_Rev_Booking <- c(air_france$amount / air_france$total_v_bookings)
# cost per booking in dollar amoair_france
air_france$Cost_Booking <- c(air_france$total_cost / air_france$total_v_bookings) 


# creating new data with variables we deemed relevant to our analysis
final_air <- air_france[ , c("publisher_name", "campaign", "eng_click_thru_rate" , "total_cost" , "total_v_bookings")]

# checking if the data contains any na's
for (i in 1:ncol(final_air)) {
  
  sum <- sum(is.na(final_air[ , i]))
  
  print(sum)
  
} # Closing the For Loop

table(final_air$campaign)

#all campaigns
all_campaigns <- final_air[!grepl("Unassigned" , final_air$campaign) & 
                             !grepl("Business Class" , final_air$campaign) & 
                             !grepl("Google_Yearlong 2006" , final_air$campaign), ]

table(all_campaigns$campaign)

#Creating a new table to compare variables for each search engine
engine_table <- air_france %>% group_by(publisher_name)%>%
  summarise(avg_revenue_booking_sum = round(sum(net_revenue),digits=0), roa_sem = round(sum(ROA),digits=2),
            total_volume_of_bookings_sem = sum(total_v_bookings))

new_row <- c("kayak",round((230126/208),digits=0),round((233694/3567),digits=0),208)
new_engine <- rbind(engine_table,new_row)

View(new_engine)

###############################
#Constructing charts
###############################

#Creating a chart to see how much  each search engines cost right now
data_pie <- air_france[,c("publisher_name","click_charges")]
piechart_cost <- plot_ly(data=data_pie,labels=~publisher_name, values=~click_charges, type="pie")
piechart_cost

#Creating a chart to show the total volume of bookings ,return on advertisement based on each publisher
chart <- ggplot(data =new_engine , aes(x = total_volume_of_bookings_sem, 
                              y=roa_sem,color=publisher_name)) + geom_jitter(size=4)+
  geom_text(aes(label = publisher_name), hjust = 0, vjust = 0)
chart

as.numeric(new_engine$total_volume_of_bookings_sem)
as.numeric(new_engine$roa_sem)

# constructing chart of cost per click
avg_clicks <- ggplot(data = air_france, aes(x = publisher_name, y = avg_cost_p_click)) +
  geom_jitter(alpha = 0.2, color = "grey55") + geom_boxplot(color = "black", 
                                                            outlier.shape = NA) +
  labs(title = "Average Cost Per Click of Each Publisher" , x = "Publisher Name",
       y = "Average Cost Per Click") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
avg_clicks

# Chart to look at the investment per region and publisher for each campagn
air_bar <- ggplot(air_us, aes( fill = campaign , x = `publisher_name`)) + 
  geom_bar( position = "fill" , alpha = 0.9) + 
  theme_classic() + 
  ggtitle("Campaign Investment Per Region and Publisher")  + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_rect(fill = "#F2F2F2" )) + 
  theme(plot.background = element_rect(fill = "#F2F2F2")) 

ggplotly(air_bar)

# Chart to look at the investment per region and publisher for each campagn
air_bar_global <- ggplot(air_global, aes( fill = campaign , x = `publisher_name`)) + 
  geom_bar( position = "fill" , alpha = 0.9) + 
  theme_classic() + 
  ggtitle("Campaign Investment Per Region and Publisher")  + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_rect(fill = "#F2F2F2" )) + 
  theme(plot.background = element_rect(fill = "#F2F2F2")) 

ggplotly(air_bar_global)

 # constructing chart of impressions per publisher
volume_impressions <- ggplot(data = air_france, aes(x = publisher_name, 
                                                    y = impressions)) +
  geom_col() + labs(title = "Impressions for each Publisher" , 
                    x = "Publisher Name", y = "Impressions") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = )
volume_impressions


###KAYAK
air_france_one <- air_france %>%
  select(clicks, total_v_bookings,net_revenue,publisher_name)%>%
  summarise(clicks = sum(clicks), bookings = sum(total_v_bookings), 
            net_revenue = sum(net_revenue)) %>%
  mutate(clicks = round(clicks/52, digits = 0), bookings = round(bookings/52,
                                                                 digits = 0), 
         net_revenue = round(net_revenue/52, digits = 0))
# renaming the column names
kayak <- kayak[ 3, ]
colnames(kayak) <- c("search_engine", "clicks", "media_cost", "total_bookings",
                     "avg_ticket", "total_revenue", "net_revenue")

# selecting only variables needed 
kayak <- kayak %>%
  select(publisher_name = search_engine, clicks, bookings = total_bookings,net_revenue)

# combining the two datasets into one
#air_france_one <- rbind(air_france_one, kayak)

# changing the dataset again
#air_france_one <- as.data.frame(air_france_one)


# creating graph for revenue from all publishers
publisher_vs_total_revenue <- ggplot(data = air_france, aes(x = publisher_name, 
                                                                y = Avg_Rev_Booking)) +
  geom_col() + scale_y_continuous(labels = comma) +
  labs(title = 'Total Revenue Per publisher') + 
  xlab('Publisher Name') + 
  ylab('Total Revenue') + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))

publisher_vs_total_revenue <- ggplotly(publisher_vs_total_revenue)
publisher_vs_total_revenue

# creating graph for revenue from all publishers 
publisher_vs_roa <- ggplot(data = air_france, aes(x = publisher_name, 
                                                            y = ROA)) +
  geom_col() + scale_y_continuous(labels = comma) +
  labs(title = 'Total ROA Per publisher') + 
  xlab('Publisher Name') + 
  ylab('Total ROA') + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.4))
publisher_vs_roa









##########################################################
#Comparisons between good keywords and all keywords
##########################################################

#getting the mode of the keywords using a UDF
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(air_france_keywords)

# creating a new dataframe with only observations that satisfy our thresholds
# revenue has a threshold of 120, ROA has one of 2
new_air_france <- subset(air_france, air_france$net_revenue >= 120 &
                           air_france$ROA > 1)

# summary of total cost, net revenue and sum for total keywords
air_france_summary <- air_france %>%
  select(total_cost, net_revenue, ROA) %>%
  summarise(sum(total_cost), sum(net_revenue), sum(ROA))

# summary of total cost, net revenue and sum for good keywords
new_air_france_summary <- new_air_france %>%
  select(total_cost, net_revenue, ROA) %>%
  summarise(sum(total_cost), sum(net_revenue), sum(ROA))

# finding count of all keywords
number_good_keywords <- length(which(air_france$net_revenue >= 120 &
                                       air_france$ROA > 1))
number_good_keywords
# finding count of good keywords 
number_total_keywords <- nrow(air_france)
number_total_keywords
# calculating the difference between bad and good keywords according to the threshholds
# we have set. saving it in keyword_gb(goodbad)
keyword_gb<-number_total_keywords - number_good_keywords
keyword_gb

# the words are considered to be bad because they did not reach revenue and ROA targets.
# this means that only about 7.1 of our keywords are considered good and the rest of the 92.9 
# are considered bad.
percentage_gb <- 4207/4510*100
percentage_gb

# visualizing this to see the difference in all the key words vs the good keys words
# according to the threshold we have set
difference_dataframe <- rbind(air_france_summary, new_air_france_summary)

# renaming the columns
colnames(difference_dataframe) <- c("total_cost", "net_revenue", "ROA")

# adding a column for names
difference_dataframe$title <- c("All Keywords", "Good Keywords Only")

# plotting graph of difference in net revenue 
revenue_difference <-  ggplot(difference_dataframe, aes(x = title, y = net_revenue)) +
  geom_col() + scale_y_continuous(labels = comma) +
  labs(x = "", y = "Total Net Revenue", 
       title = "Change in Total Net Revenue") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# turning the graph into an interactive plotly graph
revenue_difference <- ggplotly(revenue_difference)
revenue_difference

# total cost decreases by 44.85% when bad keywords are removed
((388388 - 755316) / 755316) * 100

# Although we spend so much more with all the keywords the return is not proportionate
# However when the bad keywords are removed the revenue increases by 6.4%
((4148333 - 3906597) / 3906597) * 100



# plotting graph of difference in ROA 
# the visual shows us that there is not that much of a difference in ROA between All keywords,
# and good keywords. Demonstrating that air_france should be investing more in the quality of
# keywords rather than the quantity.
roa_difference <- ggplot(difference_dataframe, aes(x = title, y = ROA)) +
  geom_col() + scale_y_continuous(labels = comma) +
  labs(x = "", y = "Return on Ads", 
       title = "Change in Return on Ads") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.8))

# turning the graph into an interactive plotly graph
roa_difference <- ggplotly(roa_difference)
roa_difference

# As seen in the graphs the difference is not that significant. 
# The revenue only  decreases by 0.15 % when the bad key words are removed.

((19906.77 - 19875.01) / 19906.77) * 100




#########################################################
# Checking the business success with logistic regression 

#Creating an empty variable for the for loop
air_france_copy <- air_france
air_france_copy$booking_bi<- NA

#Creating a for loop to convert if an observation had a bookin "1" or not "0"
for(i in 1:nrow(air_france_copy)){
  if(air_france_copy$total_v_bookings[i]==0){
    air_france_copy$booking_bi[i] <- 0
  }else{
    air_france_copy$booking_bi[i] <- 1
  } #closing if statement
} #closing for loop

# creating a UDF to normalize with min-max rescaling
normalize <- function(var){
  my_norm <- (var-min(var))/(max(var)-min(var))
  return(my_norm)
} #closing the normalize UDF

#normalizing variables to compare the variables side by side
air_france_copy$clicks_norm <- normalize(var=air_france$clicks)
air_france_copy$click_charges_norm <- normalize(var=air_france$click_charges)
air_france_copy$impressions_norm <- normalize(var=air_france$impressions)
air_france_copy$avg_pos_norm <- normalize(var=air_france$avg_pos)


# training and testing our data
# step one
training_idx<-sample(1:nrow(air_france_copy), size=0.8*nrow(air_france_copy))
#step 2
air_france_train <- air_france_copy[training_idx,]
air_france_test <- air_france_copy[-training_idx,]

#building our logistic regression
air_france_logit <- glm(booking_bi~+clicks+click_charges+avg_pos+impressions
                          ,data=air_france_train, family="binomial")
#Summary of the logistic regression to analyze which variable is better at predicting the bookings
summary(air_france_logit)
exp(.01046)-1 #clicks - for every additional click, the odds of business success increase by 1%
exp(-.001047)-1 #click_charges for every additional click charge, the odds of business success decrease by .1%
exp(-.1792)-1 #avg_pos for every additional average post, the odds of business success increase by 21.5%
exp(-.000004524)-1 #impressions for every additional impression, the odds of business success decrease by .000452399%

# crating a unitless normalized logistic model
air_france_logit_norm <- glm(booking_bi~clicks_norm+click_charges_norm+impressions_norm+avg_pos_norm, 
                             data=air_france_train, family="binomial")
summary(air_france_logit_norm)
#clicks has the strongest positive impact on business success
#impressions has the strongest negative impact
#avg_pos has the weakest impact

