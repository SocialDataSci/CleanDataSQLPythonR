#######################################################################################################################
## Author: Brianna Noland, brianna.noland@gmmail.com
## Date: 6/4/2016
#######################################################################################################################

library(data.table)
options(warn=-1)

# Read in data downloaded from https://www.whitehouse.gov/briefing-room/disclosures/visitor-records
whitehouse_visitors <- read.csv("TestData2016Messey.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))

#######################################################################################################################
# Look at the names of the columns
colnames(whitehouse_visitors) 
# There are a mix of upper and lower case headings. These should be consistent
colnames(whitehouse_visitors) <- tolower(colnames(whitehouse_visitors))
# double check to make sure it looks right
colnames(whitehouse_visitors) 

#######################################################################################################################
# Take look at the first rows of the dataframe to get a feel for the data
head(whitehouse_visitors, 10)
# Notice that namelast, namefirst, vistee_namelast, visitee_namefirst, caller_name_last, and caller_name_first are a
# mix of upper and lowercase. This should be consistent
cols_to_lower <- c('namelast', 'namefirst', 'visitee_namelast', 'visitee_namefirst', 'caller_name_last', 
                   'caller_name_first')
for(column in cols_to_lower) {
     whitehouse_visitors[,column] <- iconv(whitehouse_visitors[,column], from = 'UTF-8', to = 'latin1')
     tryCatch(whitehouse_visitors[,column] <- tolower(whitehouse_visitors[,column]),
          error=function(e) {
               print(paste('Error in column : ', column))
               })
}

# Notice that I changed the encoding of the name columns from utf-8 to latin1. I did this to account for special
# characters that may be present in the names

#######################################################################################################################
# check the data types of the columns
sapply(whitehouse_visitors, class)
# A few things jump out at me: 1) based on the data description, bdgnbr, access_type, poa, pod, post, terminal_suffix,
# meeting_loc, meeting_room, and caller_room should be factors because they represent data that would have a limited 
# number of unique entries (there may be more, but we don't know yet), 2) appt_made_date, toa, tod, appt_start_date, 
# appt_end_date, appt_cancel_date, lastentrydate, release_date, and requestsubmitted should be date objects

to_factor <- c('bdgnbr', 'access_type', 'poa', 'pod', 'post', 'terminal_suffix', 
               'meeting_loc', 'meeting_room', 'caller_room')
for(column in to_factor) {
     whitehouse_visitors[,column] <- as.factor(whitehouse_visitors[,column])
}
# double check the results
sapply(whitehouse_visitors, class)

# Notice that release_date doesn't contain seconds while the other date columns do - because this format is different,
# we will handle the correction in a separate line of code, similarly, requestsubmitted is in a different format
to_date <- c('appt_made_date', 'appt_start_date', 'appt_end_date', 'appt_cancel_date', 
             'lastentrydate', 'toa', 'tod')
for(column in to_date) {
     whitehouse_visitors[,column] <- as.POSIXct(whitehouse_visitors[,column], format = '%m/%d/%Y %H:%M')
}

whitehouse_visitors$release_date <- as.POSIXct(whitehouse_visitors$release_date, format = '%m/%d/%Y')
whitehouse_visitors$requestsubmitted <- as.POSIXct(whitehouse_visitors$requestsubmitted, format = '%Y-%m-%d')

#######################################################################################################################
# review a summary of the data
summary(whitehouse_visitors)
# Notice here that caller_room is an empty column, so we can drop it safely
whitehouse_visitors <- whitehouse_visitors[,-which(colnames(whitehouse_visitors) == 'caller_room')]

colnames(whitehouse_visitors)

#######################################################################################################################
# Lets take a look at guest_phone and guest_zip, as they should have standard formats. We can check to see if they
# conform to the appropriate lengths (zip = 5, and phone = 9)

# Zip code
# Get the number of times each length of zip code appears (note that the zip codes are strings)
zip_lengths <- as.data.table(nchar(whitehouse_visitors$guest.zip))
zip_length_counts <- zip_lengths[,.N, by = V1]
zip_length_counts

# We can see that most zip codes are 5 or 10 characters. These are most likely valid codes, but since the majority are 
# the first five digits, keep only the first five digits of those that are 9 or 10 i.e remove -xxxx or xxxx)

# Some of the zip codes contain spaces. Remove the spaces.
whitehouse_visitors$guest.zip <- gsub(' ', '', whitehouse_visitors$guest.zip)

whitehouse_visitors$guest.zip[(!is.na(whitehouse_visitors$guest.zip)) & (nchar(whitehouse_visitors$guest.zip) == 9 | nchar(whitehouse_visitors$guest.zip) == 10)] <- substr(whitehouse_visitors$guest.zip, 1, 5)
whitehouse_visitors$guest.zip[!(nchar(whitehouse_visitors$guest.zip) == 9 | nchar(whitehouse_visitors$guest.zip) == 10 | nchar(whitehouse_visitors$guest.zip) == 5)] <- NA


# Can repeat a similar excercise for phone number

#######################################################################################################################

# Examine car length and car weight
# These are numbers that can reasonably ordered and relative magnitude makes sense (whereas phone # and zip code do not)
# so lets convert these to integers

# Car weight
# First lets look at the first few values
head(whitehouse_visitors$car.weight, 20)

# We can see that there are a variety of formats. Lets remove whitespace, 'lbs', and commas
char_to_rem <- c(' ', 'lbs', ' lbs', ',')
whitehouse_visitors$car.weight <- as.integer(trimws(gsub('lbs|lb|,', '', whitehouse_visitors$car.weight)))

# Plot car weights to look for outliers
plot(whitehouse_visitors$car.weight)

# It looks like we've got 5 values that appear to be outliers. We can set those to NA
whitehouse_visitors$car.weight[whitehouse_visitors$car.weight > 60000] <- NA
plot(whitehouse_visitors$car.weight)
# Car length
# First lets look at the first few values
head(whitehouse_visitors$car.length.in.ft, 20)

# It looks like there are a number of nonnumeric values. Coerce the column to an integer and the nonnumeric values
# will convert to NA
whitehouse_visitors$car.length.in.ft <- as.integer(whitehouse_visitors$car.length.in.ft)

# Plot for outliers
plot(whitehouse_visitors$car.length.in.ft)

#######################################################################################################################

# Replace empty cells and Nulls with NAs
sapply(whitehouse_visitors, function(x), gsub(''))

# Determine how to deal with NAs
# First, calculate the percentage of each column that NA
sapply(whitehouse_visitors, function(x) round(sum(is.na(x))/length(x)*100, 4))

# How many NAs you can allow, and how you handle them are dependent on the application. 
# In this case, we will drop all rows with 20% or more NAs
whitehouse_visitors <- whitehouse_visitors[sapply(whitehouse_visitors, function(x) round(sum(is.na(x))/length(x)*100, 4)) < 20]

#######################################################################################################################

summary(whitehouse_visitors)
