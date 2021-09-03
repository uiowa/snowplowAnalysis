library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(treemapify)
library(httr)

# Read in our user journey. Update to load different journeys.
user.journey <- read.csv('userJourney_March_1.csv')

# Make a 'type' variable. If it was something other than a structured event,
# we know it was a page view, so update those.
user.journey %<>% mutate(
  type = se_category
)
# Alternative here is to set it to the page_url (or host) for a more granular visualization.
user.journey$type[which(is.na(user.journey$type))] <- 'page_view'

# Use postscript if we want to save, and not just view.
postscript(file="dotmap.ps")
# Plot events across time as a dotmap.
ggplot(user.journey) +
  geom_point(aes(x = as.POSIXct(collector_tstamp), y = type),
             size = 2, shape = 1) +
  coord_fixed(ratio = 80000)
dev.off()

# Create a host variable. This data was in the original database,
# but not pulled down for memory concerns, opting instead for this
# bit of processing on an as-needed basis.
user.journey$host <- user.journey$page_url
for(i in 1:nrow(user.journey)) {
  if(user.journey$host[i] %in% user.journey$host[which(!is.na(user.journey$host))]) {
    user.journey$host[i] <- parse_url(user.journey$host[i])$host
  }
}
# If we don't want 'page_view' as type, but instead the host,
# cycle through again now that we have that available.
for(i in 1:nrow(user.journey)) {
  if(user.journey$type[i] == 'page_view') {
    user.journey$type[i] <- user.journey$host[i]
  }
}
# Take a peek at what we have.
unique(user.journey$type)

# Data prep for treemaps. Gruop by host and page_url,
# then get counts of our page visits.
user.journey %<>% group_by(host) %>%
  mutate(
    host_counter = n()
  ) %>%
  ungroup() %>%
  group_by(page_url) %>%
  mutate(
    page_counter = n()
  ) %>%
  ungroup()

# PS if we want to export as a vector image.
postscript(file="treemap.ps")

#Filter out structured events, select our relavent data,
# and build our treemap.
user.journey %>% filter(!is.na(host)) %>%
  select(host_counter, page_counter, host, page_url) %>%
  unique() %>%
  ggplot(aes(area = page_counter,
             fill = host,
             label = page_url,
             subgroup = host)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "centre",
                             grow = T,
                             # Turn alpha off if exporting as a vector image.
                             #alpha = 0.5,
                             colour = "black",
                             fontface = "italic",
                             min.size = 0) +
  geom_treemap_text(colour = 'white',
                    place = 'topleft',
                    reflow = T,
                    # Allow for tiny text if exporting as a vector image,
                    # as we can always zoom in if needed in that case.
                    min.size = 0) +
  scale_fill_discrete()
dev.off()
