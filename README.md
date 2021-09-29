homework02
================
Brendan Zimmer
9/28/2021

\#library

``` r
library(nycflights13)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(ggplot2)
```

\#Looking at the different columns for flights

``` r
glimpse(flights)
```

    ## Rows: 336,776
    ## Columns: 19
    ## $ year           <int> 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2~
    ## $ month          <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
    ## $ day            <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1~
    ## $ dep_time       <int> 517, 533, 542, 544, 554, 554, 555, 557, 557, 558, 558, ~
    ## $ sched_dep_time <int> 515, 529, 540, 545, 600, 558, 600, 600, 600, 600, 600, ~
    ## $ dep_delay      <dbl> 2, 4, 2, -1, -6, -4, -5, -3, -3, -2, -2, -2, -2, -2, -1~
    ## $ arr_time       <int> 830, 850, 923, 1004, 812, 740, 913, 709, 838, 753, 849,~
    ## $ sched_arr_time <int> 819, 830, 850, 1022, 837, 728, 854, 723, 846, 745, 851,~
    ## $ arr_delay      <dbl> 11, 20, 33, -18, -25, 12, 19, -14, -8, 8, -2, -3, 7, -1~
    ## $ carrier        <chr> "UA", "UA", "AA", "B6", "DL", "UA", "B6", "EV", "B6", "~
    ## $ flight         <int> 1545, 1714, 1141, 725, 461, 1696, 507, 5708, 79, 301, 4~
    ## $ tailnum        <chr> "N14228", "N24211", "N619AA", "N804JB", "N668DN", "N394~
    ## $ origin         <chr> "EWR", "LGA", "JFK", "JFK", "LGA", "EWR", "EWR", "LGA",~
    ## $ dest           <chr> "IAH", "IAH", "MIA", "BQN", "ATL", "ORD", "FLL", "IAD",~
    ## $ air_time       <dbl> 227, 227, 160, 183, 116, 150, 158, 53, 140, 138, 149, 1~
    ## $ distance       <dbl> 1400, 1416, 1089, 1576, 762, 719, 1065, 229, 944, 733, ~
    ## $ hour           <dbl> 5, 5, 5, 5, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 6, 6, 6~
    ## $ minute         <dbl> 15, 29, 40, 45, 0, 58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 59, 0~
    ## $ time_hour      <dttm> 2013-01-01 05:00:00, 2013-01-01 05:00:00, 2013-01-01 0~

\#1

``` r
flights %>% 
  filter(is.na(dep_time))
```

    ## # A tibble: 8,255 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1       NA           1630        NA       NA           1815
    ##  2  2013     1     1       NA           1935        NA       NA           2240
    ##  3  2013     1     1       NA           1500        NA       NA           1825
    ##  4  2013     1     1       NA            600        NA       NA            901
    ##  5  2013     1     2       NA           1540        NA       NA           1747
    ##  6  2013     1     2       NA           1620        NA       NA           1746
    ##  7  2013     1     2       NA           1355        NA       NA           1459
    ##  8  2013     1     2       NA           1420        NA       NA           1644
    ##  9  2013     1     2       NA           1321        NA       NA           1536
    ## 10  2013     1     2       NA           1545        NA       NA           1910
    ## # ... with 8,245 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

\#for dep\_time there are 8,255 rows missing that have NA values. This
most likely means the plane never took off. I believe this becasue the
dep\_delay, arr\_time, arr\_delay, air\_time, and some tailnum also have
NA values. Some tailnum are recoreded when other columns have a NA, but
I think this could be due to the plane arriving at the airport, probably
getting checked in and then never leaving. Also, I noticed that many of
these flights are grouped up around the same day and sometimes time, I
am not sure if that is significant or not though.

\#2

``` r
flights %>% 
  select(dep_time, sched_dep_time) %>% 
  mutate(dep_minutes_since_midnight = ((dep_time %/% 100)*60 + dep_time %% 100),
         sched_dep_minutes_since_midnight = ((sched_dep_time %/% 100)*60 + sched_dep_time %% 100))
```

    ## # A tibble: 336,776 x 4
    ##    dep_time sched_dep_time dep_minutes_since_midnight sched_dep_minutes_since_m~
    ##       <int>          <int>                      <dbl>                      <dbl>
    ##  1      517            515                        317                        315
    ##  2      533            529                        333                        329
    ##  3      542            540                        342                        340
    ##  4      544            545                        344                        345
    ##  5      554            600                        354                        360
    ##  6      554            558                        354                        358
    ##  7      555            600                        355                        360
    ##  8      557            600                        357                        360
    ##  9      557            600                        357                        360
    ## 10      558            600                        358                        360
    ## # ... with 336,766 more rows

\#3

``` r
cancelled_flights <- flights %>% 
  mutate(cancelled = (is.na(arr_delay) | is.na(dep_delay))) %>% 
  group_by(year, month, day) %>% 
  summarise(
    cancelled_num = sum(cancelled),
    avg_cancelled = mean(cancelled),
    flights_num = n()
  )
```

    ## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.

``` r
cancelled_flights
```

    ## # A tibble: 365 x 6
    ## # Groups:   year, month [12]
    ##     year month   day cancelled_num avg_cancelled flights_num
    ##    <int> <int> <int>         <int>         <dbl>       <int>
    ##  1  2013     1     1            11       0.0131          842
    ##  2  2013     1     2            15       0.0159          943
    ##  3  2013     1     3            14       0.0153          914
    ##  4  2013     1     4             7       0.00765         915
    ##  5  2013     1     5             3       0.00417         720
    ##  6  2013     1     6             3       0.00361         832
    ##  7  2013     1     7             3       0.00322         933
    ##  8  2013     1     8             7       0.00779         899
    ##  9  2013     1     9             9       0.00998         902
    ## 10  2013     1    10             3       0.00322         932
    ## # ... with 355 more rows

``` r
cancelled_delay_flights <- flights %>% 
  mutate(cancelled = is.na(dep_delay)) %>% 
  group_by(year, month, day) %>% 
  summarise(
    cancelled_prop = mean(cancelled),
    avg_dep_delay = mean(dep_delay, na.rm = TRUE)
  ) 
```

    ## `summarise()` has grouped output by 'year', 'month'. You can override using the `.groups` argument.

``` r
ggplot(cancelled_delay_flights) +
  geom_point(aes(x = avg_dep_delay, y = cancelled_prop))
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

\#There is a definite decrease in flights every day execept in the first
3, while there is also an incrase of the number of flights. There is
also a strong positive relationship between avg\_dep\_delay and
cancelled flights. So yes they are related meaning most liekly as the
average\_dep\_delay increase the average number of flights canceled also
increases.
