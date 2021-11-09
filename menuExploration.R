library(dplyr)
library(magrittr)
library(readr)
library(tidyr)
library(ggplot2)
library(ggrepel)

menu.links <- c('https://uiowa.edu/admissions',
                'https://admissions.uiowa.edu/',
                  'https://grad.admissions.uiowa.edu/',
                  'https://admissions.uiowa.edu/future-students/international-students',
                'https://distance.uiowa.edu/',
                  'https://uiowa.local.drupal.uiowa.edu/admissions/cost-and-aid',
                'https://uiowa.edu/academics',
                'https://uiowa.edu/academics/areas-study',
                'https://uiowa.edu/academics/leading-programs',
                'https://uiowa.edu/academics/career-preparation',
                'https://uiowa.edu/academics/colleges',
                'https://uiowa.edu/research',
                'https://uiowa.edu/student-life',
                'https://uiowa.edu/student-life/iowa-city',
                'https://uiowa.edu/student-life/living-campus',
                'https://uiowa.edu/student-life/safety-and-support',
                'https://now.uiowa.edu/',
                  'https://events.uiowa.edu/',
                  'https://osc.uiowa.edu/media',
                'https://uiowa.edu/about-iowa')

footer.primary <- c(
  'http://hris.uiowa.edu/',
    'https://icon.uiowa.edu/',
    'https://www.maui.uiowa.edu/',
    'https://myui.uiowa.edu/my-ui/home.page',
  'https://jobs.uiowa.edu/',
    'https://office365.uiowa.edu/',
    'https://uiowa.edu/student-outcomes',
  'https://its.uiowa.edu/webconferencing'
)
footer.secondary <- c(
  'https://registrar.uiowa.edu/academic-calendar',
  'https://ubill.fo.uiowa.edu/',
    'http://catalog.registrar.uiowa.edu/',
    'https://hr.uiowa.edu/',
    'https://www.lib.uiowa.edu/',
    'https://studenthealth.uiowa.edu/'
)
footer.tertiary <- c(
  'https://uiowa.edu/a-z',
  'https://maps.uiowa.edu/',
    'https://uiowa.local.drupal.uiowa.edu/student-life/safety-and-support',
  'https://iam.uiowa.edu/whitepages/search',
  'https://emergency.uiowa.edu/',
    'https://freespeech.uiowa.edu/'
)

universal.footer <- c(
  'https://uiowa.edu/privacy',
  'https://opsmanual.uiowa.edu/community-policies/nondiscrimination-statement',
  'https://uiowa.edu/accessibility',
  'https://nativeamericancouncil.org.uiowa.edu/'
)

top.menu <- c(
  'https://admissions.uiowa.edu/apply/apply?utm_source=uiowa&utm_campaign=homepage_header',
  'https://admissions.uiowa.edu/visit-campus',
  'https://www.maui.uiowa.edu/maui/pub/admissions/webinquiry/undergraduate.page'
)

data %>%
  filter(page_url %in% c(footer.primary, footer.secondary, footer.tertiary)) %>%
  filter(page_referrer == 'https://uiowa.edu/') %>%
  count()


# Get only page_views following from uiowa.edu
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
getData = function(filename) {
  data <- read.csv(filename)

  trimmed <- data %>% filter(
    page_referrer == 'https://uiowa.edu/'
    )
  events <<- rbind(events, trimmed)
}

events <- NULL
lapply(filenames, getData)

write.csv(events, 'uiowaedu-referrer.csv')
events <- read.csv('uiowaedu-referrer.csv')

events %<>% mutate(
  month = substr(collector_tstamp, 0, 7)
)

events %<>%
  mutate(searches = grepl("search", page_url),
            menu = page_url %in% menu.links,
            top_menu = page_url %in% top.menu,
            footer_menu = page_url %in% c(footer.primary, footer.secondary, footer.tertiary),
            footer = page_url %in% universal.footer,
            coronavirus = page_url == "https://coronavirus.uiowa.edu/"
  )

events %<>% select(
  month, searches, menu, top_menu, footer_menu, footer, coronavirus
)

events %<>%
  mutate(link_type = case_when(rowSums(.[-1]) == 0 ~ "other",
                               TRUE ~ names(.[-1])[max.col(.[-1], 'last')])
         )
events %<>% select(month, link_type)
events$month <- as.factor(events$month)
events$link_type <- as.factor(events$link_type)

# Data transformation to prep for plotting w/out months
df <- events %>% 
  group_by(link_type) %>%
  count() %>%
  ungroup() %>%
  mutate(perc = `n` / sum(`n`)) %>%
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

# Data transformation to prep for plotting w/months
df <- events %>%
  group_by(month, link_type) %>%
  mutate(count = n()) %>%
  unique()

# stacked bar chart
ggplot(df, aes(x = 2, y = reorder(-perc), fill = link_type)) +
  geom_col(color = 'black') +
  geom_label(mapping = aes(label = labels),
             position = position_fill(vjust = 0.5),
             show.legend = FALSE) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# donut chart
hsize = 1.5
ggplot(df, aes(x = hsize, y = perc, fill = link_type, label = labels)) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  geom_text_repel(position = position_stack(vjust = 0.5),
                  direction = "y") +
  xlim(c(0.2, hsize + 0.5)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  guides(fill = guide_legend(title = "Link Type"))

postscript(file="lineplot.ps")
# line plot
ggplot(df, aes(x = month, y = count, group = link_type)) +
  geom_line(aes(color = link_type))
dev.off()

# do it now, but with percentages
df %<>% 
  group_by(month) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percent = count / total)

postscript(file="percent-lineplot.ps")
ggplot(df, aes(x = month, y = percent, group = link_type)) +
  geom_line(aes(color = link_type, linetype = link_type), size = 1.5)
dev.off()

# stacked area chart
postscript(file="stacked_area.ps")
ggplot(df, aes(x = month, y = percent, group = link_type, fill = link_type)) +
  geom_area(color = "black", size=.2, alpha = .7) +
  scale_fill_brewer(type = 'qual')
dev.off()
