library(tidyverse)
library(stringr)

file4 <- read.csv("/Users/muhammad.zaryab/Desktop/scripts/excelformatting/IGS Q1.csv")
file4 <- file4 %>% select(Date, Account, Permalink, Campaign, Templates..Outbound.Message., Category..Outbound.Message., Editorial..Outbound.Message., Amazon.Exclusive..Outbound.Message., Amazon.Original..Outbound.Message.,Instagram.Business.Post.Impressions..SUM., PublishedTime)
file4 <- file4 %>% filter(Campaign == "WHM 2023 (Amazon Music)")

file2 <- file4

file2 <- read.csv("/Users/muhammad.zaryab/Desktop/Sprinklr insta story2.csv")
file2 <- file2 %>% select(Date, Account, Permalink, Campaign, Templates..Outbound.Message., Category..Outbound.Message., Editorial..Outbound.Message., Amazon.Exclusive..Outbound.Message., Amazon.Original..Outbound.Message.,Instagram.Business.Post.Impressions..SUM., PublishedTime)

# convert datetime column to POSIXct class
file2$PublishedTime <- as.POSIXct(file2$PublishedTime, format = "%d/%m/%Y %H:%M")
file2$PublishedTime <- as.POSIXct(file2$PublishedTime, format = "%Y-%m-%d %H:%M")

#file2 <- file2 %>%
#  filter(Account=="amazonmusicuk") %>%
#  arrange(PublishedTime) %>%
#  mutate(time_diff = difftime(PublishedTime, lag(PublishedTime), units = "mins"))

## remove duplicates here
file2 <- file2[!duplicated(file2$Permalink), ]

file2 <- file2 %>%
  arrange(desc(Account), PublishedTime) %>%
  mutate(time_diff = difftime(PublishedTime, lag(PublishedTime), units = "mins"))

## dropping mins suffix
file2$time_diff <- gsub("[^0-9]", "", file2$time_diff)

## Creating separate files for each country

uk <- file2 %>% filter(Account=="amazonmusicuk")
it <- file2 %>% filter(Account=="amazonmusicit")
es <- file2 %>% filter(Account=="amazonmusices")
fr <- file2 %>% filter(Account=="amazonmusicfr")
de <- file2 %>% filter(Account=="amazonmusicde")






# assign group IDs based on time differences
group_id <- cumsum(file2$time_diff < 10 | is.na(file2$time_diff))

file2$group_id <- group_id


# Initialize the output vector
converted_group_id <- numeric(length(group_id))
converted_group_id[1] <- 1


# Convert the vector
for (i in 2:length(group_id)) {
  if (group_id[i] == group_id[i-1]) {
    converted_group_id[i] <- 1
  } else {
    converted_group_id[i] <- converted_group_id[i-1] + 1
  }
}
file2$finalid <- converted_group_id


file2 <- file2 %>% select(Date, Account, Permalink, Campaign, Templates..Outbound.Message., Category..Outbound.Message., Editorial..Outbound.Message., Amazon.Exclusive..Outbound.Message., Amazon.Original..Outbound.Message.,Instagram.Business.Post.Impressions..SUM., PublishedTime,finalid)

# create new column
file2$first_post_impressions <- ifelse(file2$finalid == 1, file2$Instagram.Business.Post.Impressions..SUM., NA)

# replace NAs with previous values
file2$first_post_impressions <- na.locf(file2$first_post_impressions)

## completionrate
file2$comprate <- ifelse(file2$finalid==1, "", (file2$Instagram.Business.Post.Impressions..SUM./file2$first_post_impressions)*100)

file2$comprate <- as.numeric(file2$comprate)

## get average
mean(file2$comprate, na.rm = TRUE)


# remove rows with duplicate links
uniquefile <- unique(file2[!duplicated(file2$Permalink), ])



file2 <- subset(file2, !duplicated(file2$Permalink))

## filtering by month
library(lubridate)
file3 <- file2
file3$Date <- as.Date(file3$Date, format = "%d/%m/%Y")
df_feb <- file3[month(file3$Date) == 2 & year(file3$Date) == 2023,]

uniquefile$Date <- as.Date(uniquefile$Date, format = "%d/%m/%Y")
uniquefilefeb <- uniquefile[month(uniquefile$Date) == 2 & year(uniquefile$Date) == 2023,]
uniquefilejan <- uniquefile[month(uniquefile$Date) == 1 & year(uniquefile$Date) == 2023,]

# Calculate mean while ignoring NA values and values greater than 100
mean(uniquefilejan$comprate[!is.na(uniquefilejan$comprate) & uniquefilejan$comprate <= 100])

write.csv(uniquefile, "/Users/muhammad.zaryab/Desktop/scripts/excelformatting/febstories.csv")
