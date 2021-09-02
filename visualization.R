library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(treemapify)
library(httr)

user.journey <- read.csv('userJourney_March_1.csv')
user.journey %<>% mutate(
  type = se_category
)
user.journey$type[which(is.na(user.journey$type))] <- 'page_view'

postscript(file="dotmap.ps")
ggplot(user.journey) +
  geom_point(aes(x = as.POSIXct(collector_tstamp), y = type),
             size = 2, shape = 1) +
  coord_fixed(ratio = 80000)
dev.off()

user.journey$host <- user.journey$page_url
for(i in 1:nrow(user.journey)) {
  if(user.journey$host[i] %in% user.journey$host[which(!is.na(user.journey$host))]) {
    user.journey$host[i] <- parse_url(user.journey$host[i])$host
  }
}
for(i in 1:nrow(user.journey)) {
  if(user.journey$type[i] == 'page_view') {
    user.journey$type[i] <- user.journey$host[i]
  }
}
unique(user.journey$type)

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

postscript(file="treemap.ps")
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
                             #alpha = 0.5,
                             colour = "black",
                             fontface = "italic",
                             min.size = 0) +
  geom_treemap_text(colour = 'white',
                    place = 'topleft',
                    reflow = T,
                    min.size = 0) +
  scale_fill_discrete()
dev.off()

user.jour <- read.csv('userJourney_March_1.csv')
user.jour$user_ipaddress <- 'XXX'
write.csv(user.jour, 'example.csv')
