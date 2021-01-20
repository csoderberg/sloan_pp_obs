#load libraries
library(tidyverse)
library(here)
library(lubridate)

#read in data
all_stable_metrics <- read_csv(here::here('all_stable_metrics.csv'))

# how many of each combination are their?
all_stable_metrics %>%
  filter(!is.na(has_coi) & !is.na(has_data_links) & !is.na(has_prereg_links)) %>%
  group_by(guid) %>%
  arrange(desc(date)) %>%
  slice(1L) %>%
  group_by(has_coi, has_data_links, has_prereg_links) %>%
  tally()

# breakdown of data by provider
all_stable_metrics %>%
  filter(!is.na(has_coi) & !is.na(has_data_links) & !is.na(has_prereg_links)) %>%
  group_by(guid) %>%
  arrange(desc(date)) %>%
  slice(1L) %>%
  group_by(has_data_links, pp_provider) %>%
  tally()

# breakdown of pre-reg by provider
all_stable_metrics %>%
  filter(!is.na(has_coi) & !is.na(has_data_links) & !is.na(has_prereg_links)) %>%
  group_by(guid) %>%
  arrange(desc(date)) %>%
  slice(1L) %>%
  group_by(has_prereg_links, pp_provider) %>%
  tally()

final_metrics <- all_stable_metrics %>%
                              filter(!is.na(has_coi) & !is.na(has_data_links) & 
                                       !is.na(has_prereg_links)) %>%
                              group_by(guid) %>%
                              mutate(final_view_count = max(cum_metric_view, na.rm = T),
                                     final_log_views = max(log_cum_metric_view, na.rm = T),
                                     final_download_count = max(cum_metric_download, na.rm = T),
                                     final_log_downloads = max(log_cum_metric_download, na.rm = T)) %>%
                              slice(1L) %>%
                              ungroup() %>%
                              mutate(month_pub = month(date_published))


#### Quick comparisons between levels 2 months after publication ####

## quick comparison between COI levels 2 months out

#views
wilcox.test(final_view_count ~ has_coi, 
            final_metrics %>% filter(timeperiod == 'complete'))
t.test(final_log_views ~ has_coi, 
       final_metrics %>% filter(timeperiod == 'complete'))

#downloads
wilcox.test(final_download_count ~ has_coi, 
            final_metrics %>% filter(timeperiod == 'complete'))
t.test(final_log_downloads ~ has_coi, 
       final_metrics %>% filter(timeperiod == 'complete'))

## quick comparison between COI levels using finals as of 12/18/2020, regardless of time

#views
wilcox.test(final_views ~ has_coi, 
            final_metrics)
t.test(log_final_views ~ has_coi, 
       final_metrics)

#downloads
wilcox.test(final_downloads ~ has_coi, 
            final_metrics)
t.test(log_final_downloads ~ has_coi, 
       final_metrics)

## quick comparison between COI levels using finals as of 12/18/2020, regardless of time, but only those with sloanids (so no direct downloads)

#views
wilcox.test(final_sloanid_views ~ has_coi, 
            final_metrics)
t.test(log_final_sloanid_views ~ has_coi, 
            final_metrics)

#downloads
wilcox.test(final_sloanid_downloads ~ has_coi, 
            final_metrics)
t.test(log_final_sloanid_downloads ~ has_coi, 
            final_metrics)


## quick comparison between data availability levels 2 months out

#views
wilcox.test(final_view_count ~ has_data_links, 
            final_metrics %>% filter(timeperiod == 'complete') %>% filter(has_data_links != 'not_applicable'))
t.test(final_log_views ~ has_data_links, 
       final_metrics %>% filter(timeperiod == 'complete') %>% filter(has_data_links != 'not_applicable'))

#downloads
wilcox.test(final_download_count ~ has_data_links, 
            final_metrics %>% filter(timeperiod == 'complete') %>% filter(has_data_links != 'not_applicable'))
t.test(final_log_downloads ~ has_data_links, 
       final_metrics %>% filter(timeperiod == 'complete') %>% filter(has_data_links != 'not_applicable'))


## quick comparison between data levels using finals as of 12/18/2020, regardless of time

#views
wilcox.test(final_views ~ has_data_links, 
            final_metrics %>% filter(has_data_links != 'not_applicable'))
t.test(log_final_views ~ has_data_links, 
            final_metrics %>% filter(has_data_links != 'not_applicable'))

#downloads
wilcox.test(final_downloads ~ has_data_links, 
            final_metrics %>% filter(has_data_links != 'not_applicable'))
t.test(log_final_downloads ~ has_data_links, 
            final_metrics %>% filter(has_data_links != 'not_applicable'))

## quick comparison between data levels using finals as of 12/18/2020, regardless of time, but only those with sloanids (so no direct downloads)

#views
wilcox.test(final_sloanid_views ~ has_data_links, 
            final_metrics %>% filter(has_data_links != 'not_applicable'))
t.test(log_final_sloanid_views ~ has_data_links, 
            final_metrics %>% filter(has_data_links != 'not_applicable'))

#downloads
wilcox.test(final_sloanid_downloads ~ has_data_links, 
            final_metrics %>% filter(has_data_links != 'not_applicable'))
t.test(log_final_sloanid_downloads ~ has_data_links, 
            final_metrics %>% filter(has_data_links != 'not_applicable'))

## quick comparison between pre-reg availability levels 2 months out

#views
wilcox.test(final_view_count ~ has_prereg_links, 
            final_metrics %>% filter(timeperiod == 'complete') %>% filter(has_prereg_links != 'not_applicable'))
t.test(final_log_views ~ has_prereg_links, 
       final_metrics %>% filter(timeperiod == 'complete') %>% filter(has_prereg_links != 'not_applicable'))

#downloads
wilcox.test(final_download_count ~ has_prereg_links, 
            final_metrics %>% filter(timeperiod == 'complete') %>% filter(has_prereg_links != 'not_applicable'))
t.test(final_log_downloads ~ has_prereg_links, 
       final_metrics %>% filter(timeperiod == 'complete') %>% filter(has_prereg_links != 'not_applicable'))

## quick comparison between data levels using finals as of 12/18/2020, regardless of time

#views
wilcox.test(final_views ~ has_prereg_links, 
            final_metrics %>% filter(has_prereg_links != 'not_applicable'))
t.test(log_final_views ~ has_prereg_links, 
       final_metrics %>% filter(has_prereg_links != 'not_applicable'))

#downloads
wilcox.test(final_downloads ~ has_prereg_links, 
            final_metrics %>% filter(has_prereg_links != 'not_applicable'))
t.test(log_final_downloads ~ has_prereg_links, 
       final_metrics %>% filter(has_prereg_links != 'not_applicable'))



## quick comparison between data levels using finals as of 12/18/2020, regardless of time, but only those with sloanids (so no direct downloads)


#views
wilcox.test(final_sloanid_views ~ has_prereg_links, 
            final_metrics %>% filter(has_prereg_links != 'not_applicable'))
t.test(log_final_sloanid_views ~ has_prereg_links, 
       final_metrics %>% filter(has_prereg_links != 'not_applicable'))

#downloads
wilcox.test(final_sloanid_downloads ~ has_prereg_links, 
            final_metrics %>% filter(has_prereg_links != 'not_applicable'))
t.test(log_final_sloanid_downloads ~ has_prereg_links, 
       final_metrics %>% filter(has_prereg_links != 'not_applicable'))


### just in psyarchix


## quick comparison between pre-reg availability levels 2 months out

#views
wilcox.test(final_view_count ~ has_prereg_links, 
            final_metrics %>% filter(timeperiod == 'complete') %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))
t.test(final_log_views ~ has_prereg_links, 
       final_metrics %>% filter(timeperiod == 'complete') %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))

#downloads
wilcox.test(final_download_count ~ has_prereg_links, 
            final_metrics %>% filter(timeperiod == 'complete') %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))
t.test(final_log_downloads ~ has_prereg_links, 
       final_metrics %>% filter(timeperiod == 'complete') %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))

## quick comparison between data levels using finals as of 12/18/2020, regardless of time

#views
wilcox.test(final_views ~ has_prereg_links, 
            final_metrics %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))
t.test(log_final_views ~ has_prereg_links, 
       final_metrics %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))

#downloads
wilcox.test(final_downloads ~ has_prereg_links, 
            final_metrics %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))
t.test(log_final_downloads ~ has_prereg_links, 
       final_metrics %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))



## quick comparison between data levels using finals as of 12/18/2020, regardless of time, but only those with sloanids (so no direct downloads)


#views
wilcox.test(final_sloanid_views ~ has_prereg_links, 
            final_metrics %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))
t.test(log_final_sloanid_views ~ has_prereg_links, 
       final_metrics %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))

#downloads
wilcox.test(final_sloanid_downloads ~ has_prereg_links, 
            final_metrics %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))
t.test(log_final_sloanid_downloads ~ has_prereg_links, 
       final_metrics %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))


## quick comparison between data levels using finals as of 12/18/2020, regardless of time, but only those with sloanids (so no direct downloads)


#views
wilcox.test(final_sloanid_views ~ has_prereg_links, 
            final_metrics %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))
t.test(log_final_sloanid_views ~ has_prereg_links, 
       final_metrics %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))

#downloads
wilcox.test(final_sloanid_downloads ~ has_prereg_links, 
            final_metrics %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))
t.test(log_final_sloanid_downloads ~ has_prereg_links, 
       final_metrics %>% filter(has_prereg_links != 'not_applicable' & pp_provider == 'psyarxiv'))
