#load libraries
library(tidyverse)
library(here)
library(lubridate)

Sys.setenv(TZ='UTC')

# load data
db_pp_data <- read_csv(here::here('db_pp_data.csv')) %>%
                rename(pp_provider = '_id') %>%
                mutate(assert_change_post_pub = case_when(created <= date_published ~ 0,
                                                    created > date_published ~ 1))

# get pp to pull download/view data for
stable_assert_pp <- db_pp_data %>%
                      group_by(id) %>%
                      mutate(any_assert_changes = max(assert_change_post_pub)) %>%
                      filter(any_assert_changes == 0) %>%
                      slice(1L) %>%
                      ungroup() %>%
                      select(-c(action, log_value, created, assert_change_post_pub, any_assert_changes)) %>%
                      mutate(nine_weeks = date_published + days(63))

pp_pull_complete_data <- stable_assert_pp %>%
                            filter(nine_weeks < '2020-12-18') #to get all pp that have full data based on the day the data pull will happen

pp_pull_partial_data <- stable_assert_pp %>%
                            filter(nine_weeks >= '2020-12-18')

write_csv(pp_pull_complete_data, 'pp_pull_complete_data.csv')
write_csv(pp_pull_partial_data, 'pp_pull_partial_data.csv')


#### cleaning of data retrieved from elastic search queries
complete_views <- read_csv(here::here('complete_views_df.csv'))
partial_views <- read_csv(here::here('partial_views_df.csv'))
complete_downloads <- read_csv(here::here('complete_downloads_df.csv'))
partial_downloads <- read_csv(here::here('partial_downloads_df.csv'))

all_time_views <- read_csv(here::here('pp_views_end_df.csv')) %>%
                      rename(final_views = 'view_count') %>%
                      mutate(log_final_views = log(final_views + 1))
all_time_downloads <- read_csv(here::here('pp_downloads_end_df.csv'))  %>%
                          rename(final_downloads = 'download_count') %>%
                          mutate(log_final_downloads = log(final_downloads + 1))
all_time_sloanid_views <- read_csv(here::here('sloanid_pp_views_df.csv'))  %>%
                            rename(final_sloanid_views = 'view_count') %>%
                            mutate(log_final_sloanid_views = log(final_sloanid_views + 1))
all_time_sloanid_downloads <- read_csv(here::here('sloanid_pp_downloads_df.csv'))  %>%
                                rename(final_sloanid_downloads = 'download_count') %>%
                                mutate(log_final_sloanid_downloads = log(final_sloanid_downloads + 1))


# function to create cumulative and log cumulative metrics counts for each pp
cum_sums <- function(data, variable, type, metric) {
  data %>%
    group_by(guid) %>%
    arrange(date) %>%
    mutate(cum_metric = cumsum({{variable}}),
           log_cum_metric = log(cum_metric + 1),
           timeperiod = {{type}},
           metric = {{metric}})
}

# add cumulative info to each datafile
complete_views <- cum_sums(complete_views, view_count, 'complete', 'view')
partial_views <- cum_sums(partial_views, view_count, 'partial', 'view')
complete_downloads <- cum_sums(complete_downloads, download_count, 'complete', 'download')
partial_downloads <- cum_sums(partial_downloads, download_count, 'partial', 'download')

all_views <- rbind(complete_views, partial_views) %>%
                rename(raw_count = 'view_count')
all_downloads <- rbind(complete_downloads, partial_downloads) %>%
                    rename(raw_count = 'download_count')

all_stable_views <- stable_assert_pp %>%
                      left_join(all_views, by = 'guid')
all_stable_downloads <- stable_assert_pp %>%
                          left_join(all_downloads, by = 'guid')

all_stable_metrics <- rbind(all_stable_views, all_stable_downloads) %>%
                        distinct() %>%
                        filter(!is.na(metric)) %>%
                        pivot_wider(names_from = metric, values_from = c(raw_count, cum_metric, log_cum_metric))%>%
                        left_join(all_time_views, by = 'guid') %>%
                        left_join(all_time_downloads, by = 'guid') %>%
                        left_join(all_time_sloanid_views, by = 'guid') %>%
                        left_join(all_time_sloanid_downloads, by = 'guid')

write_csv(all_stable_metrics, 'all_stable_metrics.csv')
