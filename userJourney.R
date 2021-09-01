library(dplyr)
library(magrittr)
library(tidyr)

setwd("~/Documents/Workbench-Build127-with-optional-libs")

filenames = c('2020-08.csv',
              '2020-09.csv',
              '2020-10.csv',
              '2020-11.csv',
              '2020-12.csv',
              '2021-01.csv',
              '2021-02.csv',
              '2021-03.csv',
              '2021-04.csv',
              '2021-05.csv',
              '2021-06.csv',
              '2021-07.csv'
              )

filename <- filenames[3]
data <- read.csv(filename)

###############################
# Basic User Journey Function #
###############################
getUserJourney = function(data, user.id) {
  user <- data %>% filter(user_id == user.id)
  user.journey <- data %>% filter(
    user_id == user.id |
      (network_userid != '' & network_userid %in% user$network_userid) |
      (domain_userid != '' & domain_userid %in% user$domain_userid)
  )
  user.journey
}

####################################################################
# Attempting to grab an applied prospective student's user journey #
####################################################################
##
# Let's look at March
filename <- filenames[8]
data <- read.csv(filename)

# Grab user ids for anyone who submitted an application form in this time.
users.applied <- data %>%
  filter(se_category == 'application' & se_action == 'submitted') %>%
  select(user_id) %>%
  unique()

# Grab an individual user id.
user.id <- users.applied$user_id[1]

network.ids <- NULL
domain.ids <- NULL

######
### Loop through all files to get any network or domain user.ids tied to our individual user.id.
######
# Define the individual function here.
get_ids <- function(filename) {
  message(paste('Starting with: ' , filename))
  temp_data <- read.csv(filename)
  
  # Filter on our user.id and select our ids of interest.
  temp_data %<>% 
    filter(user_id == user.id) %>%
    select(domain_userid, network_userid)
  
  temp.network.ids <- temp_data %>%
    select(network_userid) %>%
    filter(network_userid != '') %>%
    unique()
  network.ids <<- rbind(network.ids, temp.network.ids) %>%
    unique()
  
  temp.domain.ids <- temp_data %>%
    select(domain_userid) %>%
    filter(domain_userid != '') %>%
    unique()
  
  domain.ids <<- rbind(domain.ids, temp.domain.ids) %>%
    unique()
}
# Clear out some memory before starting
data <- NULL

# Now for the workhorse:
lapply(filenames, get_ids)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# @TODO update the above (and below) to stream rows, instead of load files and then process? @
#   Alternatively, maybe a text search for the string as an initial pass may be more         @
#   efficient, by eliminating months we know we won't find.                                  @
#   Otherwise, can also look at going in reverse order, so that we're moving back from       @
#   a touchpoint, and if we come across a month or two with no hits, stop early on the       @
#   assumption that we've found the earliest (knowable) touchpoint.                          @
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

############################################################################################
# Here we now have a single user_id as well as any matched network_userid or domain_userid #
# tied to a user who we know has submitted an application form.                            #
#                                                                                          #
# Next step is to loop through our data (again) so that we may pull out any events         #
# that match up to any of these three associated IDs.                                      #
############################################################################################
events <- NULL
# Define the individual function here.
get_events <- function(filename) {
  message(paste('Starting with: ' , filename))
  temp_data <- read.csv(filename)
  
  # Filter on our user.id, network.ids, and domain.ids.
  temp_data %<>% filter(
    user_id == user.id |
      # Match a non-null network or domain userid, with either a null or known user id.
      (network_userid != '' & network_userid %in% network.ids$network_userid & (user_id %in% c(user.id, ''))) |
      (domain_userid != '' & domain_userid %in% domain.ids$domain_userid & (user_id %in% c(user.id, ''))) |
      (network_userid != '' & network_userid %in% domain.ids$domain_userid)
    )

  events <<- rbind(events, temp_data)
}

# Now for the workhorse:
lapply(filenames, get_events)

write.csv(events, 'test_journey_march_user1_netAndDomainCombo.csv')
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#\  *
#///////////////////////////////////////# \|#|
# AND WE ARE DONE. WERE WE SUCCESSFUL?! #  |#|
#///////////////////////////////////////# /|#|
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\#/ |#|