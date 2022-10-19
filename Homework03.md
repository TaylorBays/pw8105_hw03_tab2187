Homework 03
================
2022-10-15

## Problem 0

Created a R project to organize my work, a R Markdown to write
reproducible reports, relative paths to load data from local files, and
reasonable naming structures to my files. Successfully established a git
repository to upload commit history.

\##Problem 1

``` r
# install.packages("devtools")
devtools::install_github("p8105/p8105.datasets")
```

    ## Skipping install of 'p8105.datasets' from a github remote, the SHA1 (412759e3) has not changed since last install.
    ##   Use `force = TRUE` to force installation

Had to download Rtools42 for windows to access the p8105 datasets
(<https://cran.r-project.org/bin/windows/Rtools/rtools42/rtools.html>).
Hopefully this is legit and I did not download a virus. Since that is
completed I can move on to problem 1.

``` r
library(p8105.datasets)
data("instacart")
View(instacart)
```

#### Read in the data

``` r
data("instacart")

instacart = 
  instacart %>% 
  as_tibble(instacart)
```

This dataset contains 1384617 rows and 15 columns, with each row
resprenting a single product from an instacart order. Variables include
identifiers for user, order, and product; the order in which each
product was added to the cart. There are several order-level variables,
describing the day and time of the order, and number of days since prior
order. Then there are several item-specific variables, describing the
product name (e.g. Yogurt, Avocado), department (e.g. dairy and eggs,
produce), and aisle (e.g. yogurt, fresh fruits), and whether the item
has been ordered by this user in the past. In total, there are 39123
products found in 131209 orders from 131209 distinct users.

Below is a table summarizing the number of items ordered from aisle. In
total, there are 134 aisles, with fresh vegetables and fresh fruits
holding the most items ordered by far.

``` r
instacart %>% 
  count(aisle) %>% 
  arrange(desc(n))
```

    ## # A tibble: 134 × 2
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # … with 124 more rows

Next is a plot that shows the number of items ordered in each aisle.
Here, aisles are ordered by ascending number of items.

``` r
instacart %>% 
  count(aisle) %>% 
  filter(n > 10000) %>% 
  mutate(aisle = fct_reorder(aisle, n)) %>% 
  ggplot(aes(x = aisle, y = n)) + 
  geom_point() + 
  labs(title = "Number of items ordered in each aisle") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
```

<img src="Homework03_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" />

Our next table shows the three most popular items in aisles
`baking ingredients`, `dog food care`, and `packaged vegetables fruits`,
and includes the number of times each item is ordered in your table.

``` r
instacart %>% 
  filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>%
  group_by(aisle) %>% 
  count(product_name) %>% 
  mutate(rank = min_rank(desc(n))) %>% 
  filter(rank < 4) %>% 
  arrange(desc(n)) %>%
  knitr::kable()
```

| aisle                      | product_name                                  |    n | rank |
|:---------------------------|:----------------------------------------------|-----:|-----:|
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |
| baking ingredients         | Light Brown Sugar                             |  499 |    1 |
| baking ingredients         | Pure Baking Soda                              |  387 |    2 |
| baking ingredients         | Cane Sugar                                    |  336 |    3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |

Finally is a table showing the mean hour of the day at which Pink Lady
Apples and Coffee Ice Cream are ordered on each day of the week. This
table has been formatted in an untidy manner for human readers. Pink
Lady Apples are generally purchased slightly earlier in the day than
Coffee Ice Cream, with the exception of day 5.

``` r
instacart %>%
  filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>%
  group_by(product_name, order_dow) %>%
  summarize(mean_hour = mean(order_hour_of_day)) %>%
  spread(key = order_dow, value = mean_hour) %>%
  knitr::kable(digits = 2)
```

    ## `summarise()` has grouped output by 'product_name'. You can override using the
    ## `.groups` argument.

| product_name     |     0 |     1 |     2 |     3 |     4 |     5 |     6 |
|:-----------------|------:|------:|------:|------:|------:|------:|------:|
| Coffee Ice Cream | 13.77 | 14.32 | 15.38 | 15.32 | 15.22 | 12.26 | 13.83 |
| Pink Lady Apples | 13.44 | 11.36 | 11.70 | 14.25 | 11.55 | 12.78 | 11.94 |

\##Problem 2 Load, tidy, and otherwise wrangle the data. Your final
dataset should include all originally observed variables and values;
have useful variable names; include a weekday vs weekend variable; and
encode data with reasonable variable classes. Describe the resulting
dataset (e.g. what variables exist, how many observations, etc).

–\>In this data set we have maintained the variable week, day_id, and
day. There is a new variable named activity_num so that we can clearly
see the different activities done on a given day. Next to activity_num
is activity minutes so we can clearly see how long the patient spent
doing various activities clearly. As TA Jimmy Kelliher pointed out on
the discussion board all other variables besides activity\_\* should be
retained. In this level of tidying the variables are retained.

``` r
accel_1=
  read.csv("accel_data.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    activity_1:activity_1440,
    names_to = "activity_num",
    values_to = "activity_minutes") %>%
  janitor::clean_names()
accel_1
```

    ## # A tibble: 50,400 × 5
    ##     week day_id day    activity_num activity_minutes
    ##    <int>  <int> <chr>  <chr>                   <dbl>
    ##  1     1      1 Friday activity_1               88.4
    ##  2     1      1 Friday activity_2               82.2
    ##  3     1      1 Friday activity_3               64.4
    ##  4     1      1 Friday activity_4               70.0
    ##  5     1      1 Friday activity_5               75.0
    ##  6     1      1 Friday activity_6               66.3
    ##  7     1      1 Friday activity_7               53.8
    ##  8     1      1 Friday activity_8               47.8
    ##  9     1      1 Friday activity_9               55.5
    ## 10     1      1 Friday activity_10              43.0
    ## # … with 50,390 more rows

Traditional analyses of accelerometer data focus on the total activity
over the day. Using your tidied dataset, aggregate across minutes to
create a total activity variable for each day, and create a table
showing these totals. Are any trends apparent?

–\>With the table created from this code chunk we see that the person
had higher activity levels on Friday’s. His second and third highest
ranking days were Wednesday and Thursday. The patient was also the most
active during Week 2. His second and third highest ranking weeks were
Week 3 and 5. The patient has odd activity data on Saturday’s for Week 3
and Week 4. We can hypothesize that the patient took off the
accelerometer to charge it around the same time each Saturday but did
not take off the monitor for the other Saturday’s.

``` r
accel_2=
  read.csv("accel_data.csv") %>% 
  janitor::clean_names() %>% 
  pivot_longer(
    activity_1:activity_1440,
    names_to = "activity_num",
    values_to = "activity_minutes") %>%
  mutate(
    day = factor(day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
    weekday_vs_weekend =
      if_else(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) %>% 
   mutate(
    activity_minutes = as.numeric(activity_minutes)) %>% 
  group_by(week, day) %>% 
  summarise(total_activity = sum(activity_minutes)) %>% 
  pivot_wider(
    names_from = "day",
    values_from = "total_activity"
  ) %>% 
  janitor::clean_names()
```

    ## `summarise()` has grouped output by 'week'. You can override using the
    ## `.groups` argument.

``` r
accel_2
```

    ## # A tibble: 5 × 8
    ## # Groups:   week [5]
    ##    week  monday tuesday wednesday thursday  friday saturday sunday
    ##   <int>   <dbl>   <dbl>     <dbl>    <dbl>   <dbl>    <dbl>  <dbl>
    ## 1     1  78828. 307094.   340115.  355924. 480543.   376254 631105
    ## 2     2 295431  423245    440962   474048  568839    607175 422018
    ## 3     3 685910  381507    468869   371230  467420    382928 467052
    ## 4     4 409450  319568    434460   340291  154049      1440 260617
    ## 5     5 389080  367824    445366   549658  620860      1440 138421

Accelerometer data allows the inspection activity over the course of the
day. Make a single-panel plot that shows the 24-hour activity time
courses for each day and use color to indicate day of the week. Describe
in words any patterns or conclusions you can make based on this graph.

–\>We see that the patient had on average longer active periods on
Friday’s. For each week presented in the plot Friday has outliers
extending farther down the x-axis. This makes sense because Friday had
the most active minutes. The Saturday’s for week 4 and 5 appear to only
have 1 activity minutes, which makes sense because the Saturday total
for these weeks were very low.

``` r
accel_1 %>%
  ggplot(aes(x = activity_minutes, y = day, color = day)) +
  geom_point(aes(size = .3), alpha =.3)  +
  geom_smooth(se = FALSE) + 
  facet_grid(. ~ week)+
  theme(axis.text.x = element_text(angle =90, vjust = 0.5, hjust=1))
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

<img src="Homework03_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" />

``` r
ggplot(accel_1, aes(x = activity_num, y=activity_minutes, color = day )) +
  geom_point(alpha = 0.5) +
  labs(
    x = "Activity Number",
    y = "Minutes of the Day",
    title= "Scatterplot of Activity Number Per Minute of Each Day"
  )
```

<img src="Homework03_files/figure-gfm/unnamed-chunk-11-1.png" width="90%" />

\##Problem 3

``` r
library(p8105.datasets)
data("ny_noaa")
View(ny_noaa)
```

``` r
data("ny_noaa")

ny_noaa = 
  ny_noaa %>% 
  as_tibble(ny_noaa)
```

This dataset contains 2595176 rows and 7 columns, with each row
representing a data from one of the 100,000 weather stations. Variables
include identifiers for weather station id, date of observation,
precipitation, snowfall, snow depth, maximum temperature and minimum
temperature. Precipitation, snowfall, and snow depth are measured in
millimeters, while temperature is measured using Celsius to the tenths
of a degree.

Below is a table summarizing the number of different weather stations
that have collected data. In total, there are 747 weather stations
represented, with US1NYCN0011, US1NYCES0005, and USNYSF0030 with the
least amount of data entries (days of weather information) all three
having 30 days of entries.

``` r
ny_noaa %>% 
  count(id) %>% 
  arrange(desc(n))
```

    ## # A tibble: 747 × 2
    ##    id              n
    ##    <chr>       <int>
    ##  1 USC00300055 10957
    ##  2 USC00300343 10957
    ##  3 USC00300889 10957
    ##  4 USC00304772 10957
    ##  5 USC00304836 10957
    ##  6 USC00304912 10957
    ##  7 USC00305426 10957
    ##  8 USC00305714 10957
    ##  9 USC00306196 10957
    ## 10 USC00309425 10957
    ## # … with 737 more rows

Do some data cleaning. Create separate variables for year, month, and
day.

``` r
 ny_noaa_tidy1=
  ny_noaa %>% 
  janitor::clean_names() %>% 
   separate(date, into = c("year", "month", "day")) %>% 
  mutate(year = as.integer(year), month = as.integer(month), day = as.integer(day)) %>% 
  mutate(month = month.name[as.integer(month)])
ny_noaa_tidy1
```

    ## # A tibble: 2,595,176 × 9
    ##    id           year month      day  prcp  snow  snwd tmax  tmin 
    ##    <chr>       <int> <chr>    <int> <int> <int> <int> <chr> <chr>
    ##  1 US1NYAB0001  2007 November     1    NA    NA    NA <NA>  <NA> 
    ##  2 US1NYAB0001  2007 November     2    NA    NA    NA <NA>  <NA> 
    ##  3 US1NYAB0001  2007 November     3    NA    NA    NA <NA>  <NA> 
    ##  4 US1NYAB0001  2007 November     4    NA    NA    NA <NA>  <NA> 
    ##  5 US1NYAB0001  2007 November     5    NA    NA    NA <NA>  <NA> 
    ##  6 US1NYAB0001  2007 November     6    NA    NA    NA <NA>  <NA> 
    ##  7 US1NYAB0001  2007 November     7    NA    NA    NA <NA>  <NA> 
    ##  8 US1NYAB0001  2007 November     8    NA    NA    NA <NA>  <NA> 
    ##  9 US1NYAB0001  2007 November     9    NA    NA    NA <NA>  <NA> 
    ## 10 US1NYAB0001  2007 November    10    NA    NA    NA <NA>  <NA> 
    ## # … with 2,595,166 more rows

Ensure observations for temperature are in degrees Celsuis not “tenths
of degrees C”. Ensure precipitation and snowfall are given in reasonable
units. Since snowfall is already in mm it does not need to be changed
but precipitation is in tenths of a mm so that needs to be correct in a
similar fashion to the minimum and maximum temperature.

``` r
ny_noaa_tidy2 =  
  ny_noaa %>% 
  sample_n(2000) %>% 
  janitor::clean_names() %>% 
   separate(date, into = c("year", "month", "day")) %>% 
  mutate(year = as.integer(year), month = as.integer(month), day = as.integer(day)) %>% 
  mutate(month = month.name[as.integer(month)]) %>% 
  mutate(tmin= as.numeric(tmin)) %>% 
  mutate(tmin = as.numeric(tmin / 10)) %>%
  mutate(tmax= as.numeric(tmax)) %>% 
  mutate(tmax = as.numeric(tmax / 10)) %>%
  mutate(prcp= as.numeric(prcp)) %>% 
  mutate(prcp = as.numeric(prcp / 10)) %>% 
 select(id, year, month, day, prcp, snow, snwd, tmax, tmin)
ny_noaa_tidy2
```

    ## # A tibble: 2,000 × 9
    ##    id           year month      day  prcp  snow  snwd  tmax  tmin
    ##    <chr>       <int> <chr>    <int> <dbl> <int> <int> <dbl> <dbl>
    ##  1 USC00309303  1995 October      3   0       0     0  NA    NA  
    ##  2 USW00094790  1989 January      8   2       0     0   7.2  -2.8
    ##  3 USC00309544  1992 August       9  19.8     0     0  NA    NA  
    ##  4 USC00305032  1986 May          4   0.3     0     0  NA    NA  
    ##  5 USC00300452  1992 January     15  16.5     0     0  NA    NA  
    ##  6 USW00014743  2008 November    27   2      76    76   1.1  -3.3
    ##  7 US1NYCR0003  2005 November    12   0       0    NA  NA    NA  
    ##  8 US1NYTM0021  2008 July        16  NA      NA    NA  NA    NA  
    ##  9 US1NYCY0003  2006 July        24   0      NA    NA  NA    NA  
    ## 10 USC00309507  1990 October     28   5.6     0     0  NA    NA  
    ## # … with 1,990 more rows

For snowfall what are the most commonly observed values. Why? –\>We see
that`0` was observed 2008508 times, `25` was observed 31022 times, and
`13` was observed 23095 times. There was no data for data entries 381221
times. I would assume `0` was measured so many times because on those
days no snow fell, therefore the measured snowfall would be 0.

``` r
ny_noaa_tidy2 %>% 
  count(snow) %>% 
arrange(desc(n))
```

    ## # A tibble: 46 × 2
    ##     snow     n
    ##    <int> <int>
    ##  1     0  1539
    ##  2    NA   294
    ##  3    25    28
    ##  4    13    18
    ##  5    51    16
    ##  6    76    10
    ##  7     8     9
    ##  8     5     6
    ##  9    64     6
    ## 10     3     5
    ## # … with 36 more rows

Trying to get the tmax mean to work. After multiple attempts I am
successful.

``` r
mean(pull(ny_noaa_tidy2, tmax), na.rm = TRUE)
```

    ## [1] 13.72583

``` r
pull(ny_noaa_tidy2, tmax) %>% 
  mean(na.rm = TRUE)
```

    ## [1] 13.72583

Make a two-panel plot showing the average max temperature in January and
in July in each station across years. Is there any observable /
interpretable structure? Any outliers?

In the graph we see that July had on average higher maximum temperatures
than in January. I do not see any outliers for the separate months on
average data.

``` r
avg_max_temp_JanJuly = ny_noaa_tidy2 %>% 
  group_by(month, year, id) %>% 
  filter(month == "January" | month== "July") %>%
  summarize(tmax_mean = mean(tmax, na.rm = TRUE)) 
```

    ## `summarise()` has grouped output by 'month', 'year'. You can override using the
    ## `.groups` argument.

``` r
  ggplot(avg_max_temp_JanJuly, aes(x = year, y = tmax_mean)) +
  geom_point()+
    geom_path()+
    facet_grid(.~month)+
  labs(
    title = "Average Maximum Temperature vs. Year by Weather Station in January and July",
    x = "Year",
    y = "Average Max Temperature (C)")
```

<img src="Homework03_files/figure-gfm/unnamed-chunk-19-1.png" width="90%" />

Make a two-panel plot showing (i) tmax vs tmin for the full dataset
(note that a scatterplot may not be the best option); and (ii) make a
plot showing the distribution of snowfall values greater than 0 and less
than 100 separately by year.

``` r
tmax_tmin = ny_noaa_tidy2 %>% 
drop_na() %>% 
  ggplot(aes(x = tmax, y=tmin)) + geom_hex() +
  labs(title = "Hexplot showing max vs. min temperature",
       x= "Maximum temperature (C)",
       y= "Minimum temperature (C")

snowfall = ny_noaa_tidy2 %>% 
  drop_na() %>% 
  filter(snow > 0 & snow < 100) %>% 
  ggplot(aes(x =snow, y = factor(year))) + geom_density_ridges() +
  labs(title = "Density Ridge Line Showing the Distribution of Snowfall by Year",
       x= "Snowfall (mm)",
       y= "Year")

tmax_tmin/snowfall
```

    ## Picking joint bandwidth of 9.37

<img src="Homework03_files/figure-gfm/unnamed-chunk-20-1.png" width="90%" />
