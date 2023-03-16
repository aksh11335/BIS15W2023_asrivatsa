---
output: 
  html_document: 
    keep_md: yes
---
Based on 2022 paper, "https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0278328" Does culture moderate relationships between rumination and symptoms of depression and PTSD? 
Brooding/RRS rates & culture with: 

Akshanth:      religion, gender, education

JKA:           depression, anxiety, age, trauma type for PTSD


----------------------------------------------------------------------------------------------

Data codes for variables of interest from paper:

For both: First 109 participant ids are Euro-Australian, next 144 are Malaysian

For depression data:

2 Cultures:  0,1 = Euro-Australian, Malaysian

Gender:              
 M = 1
 F = 2
Declined to state = 5  

Education (Highest level achieved):
Secondary = 1
Post-secondary = 2
Undergrad degree = 3
Graduate degree = 4
Other = 5

Religion codes:
None = 1
Christian = 2
Muslim = 3
Buddhist/Taoist = 4
Hindu = 5
Other (incl Sikh, Agnostic) = 6

PTSD dataset:

Culture = Groups 1,2  = Euro-Australian, Malaysian (change to factors)

Index trauma type (what the PTSD is thought to be in response to):

Accident/serious injury/illness = 1
Non-sexual assault/abuse = 2
Sexual assault/abuse = 3
Witnessing death = 4
War/natural disaster = 5
Other = 6       

HADS = Hospital Anxiety and Depression Scale: 

had_sdep = depression subscale total
had_sanx = anxiety subscale total
had_stotal = overall HADS total

pts_dtotal = overall score on PTSD assessment scale

RRS-B = Brooding scale - want sum of all RRS scores


```r
library(here)
```

```
## here() starts at /Users/jolaneabrams/Desktop/BIS15W2023_jabrams
```

```r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.0     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ ggplot2   3.4.1     ✔ tibble    3.1.8
## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
## ✔ purrr     1.0.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors
```

```r
library(naniar)
library(janitor)
```

```
## 
## Attaching package: 'janitor'
## 
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(skimr)
```

```
## 
## Attaching package: 'skimr'
## 
## The following object is masked from 'package:naniar':
## 
##     n_complete
```

```r
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(paletteer)
library(tidyr)
```


```r
my_palette <- paletteer::paletteer_d(("ggprism::colors"))
```


```r
dep<-read_csv(here("jka_r_project", "rum_depress_jka.csv")) %>% 
              clean_names
```

```
## Rows: 253 Columns: 11
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl (11): ID, Age, Gender, Education, Ethnicity, culturalgroup, Culture, Rel...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
names(dep)
```

```
##  [1] "id"               "age"              "gender"           "education"       
##  [5] "ethnicity"        "culturalgroup"    "culture"          "religion_coded"  
##  [9] "depression_total" "anxiety_total"    "brooding_total"
```



```r
ptsd<- read_csv(here("jka_r_project", "rum_ptsd_jka.csv")) %>%     
  clean_names
```

```
## Rows: 257 Columns: 58
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (1): relgion
## dbl (57): participantnumber, group, traumacoding, timecode, Pcl1, pcl2, pcl3...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


```r
names(ptsd)
```

```
##  [1] "participantnumber" "group"             "traumacoding"     
##  [4] "timecode"          "pcl1"              "pcl2"             
##  [7] "pcl3"              "pcl4"              "pcl5"             
## [10] "pcl6"              "pcl7"              "pcl8"             
## [13] "pcl9"              "pcl10"             "pcl11"            
## [16] "pcl12"             "pcl13"             "pcl14"            
## [19] "pcl15"             "pcl16"             "pcl17"            
## [22] "pcl18"             "pcl19"             "pcl20"            
## [25] "pts_dtotal"        "hads1"             "hads2"            
## [28] "hads3"             "hads4"             "hads5"            
## [31] "hads6"             "hads7"             "hads8"            
## [34] "hads9"             "hads10"            "hads11"           
## [37] "hads12"            "hads13"            "hads14"           
## [40] "had_sdep"          "had_sanx"          "had_stotal"       
## [43] "rrs1"              "rrs2"              "rrs3"             
## [46] "rrs4"              "rrs5"              "rrs6"             
## [49] "rrs7"              "rrs8"              "rrs9"             
## [52] "rrs10"             "age"               "gender"           
## [55] "education"         "ethnicity"         "relgion"          
## [58] "religioncode"
```
Tidying ptsd data


```r
glimpse(ptsd)
```

```
## Rows: 257
## Columns: 58
## $ participantnumber <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1…
## $ group             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
## $ traumacoding      <dbl> 4, 1, 1, 7, 3, 6, 1, 6, 3, 1, 3, 6, 3, 1, 8, 5, 6, 1…
## $ timecode          <dbl> 3.0, 2.0, 5.0, 1.0, 0.5, 3.0, 0.5, 24.0, 4.0, 3.0, 2…
## $ pcl1              <dbl> 1, 3, 4, 3, 3, 1, 1, 0, 3, 0, 1, 4, 0, 0, 2, 1, 2, 3…
## $ pcl2              <dbl> 0, 3, 2, 0, 0, 1, 2, 0, 2, 0, 0, 3, 0, 0, 0, 0, 0, 0…
## $ pcl3              <dbl> 0, 2, 2, 3, 0, 0, 3, 0, 4, 1, 0, 2, 1, 0, 1, 0, 1, 1…
## $ pcl4              <dbl> 1, 3, 4, 2, 3, 1, 2, 1, 4, 1, 2, 4, 0, 1, 2, 0, 2, 3…
## $ pcl5              <dbl> 1, 3, 4, 3, 2, 1, 3, 0, 3, 0, 1, 4, 1, 0, 2, 0, 1, 0…
## $ pcl6              <dbl> 3, 4, 4, 3, 3, 3, 3, 1, 4, 1, 1, 4, 0, 0, 3, 0, 3, 1…
## $ pcl7              <dbl> 3, 3, 3, 4, 2, 3, 2, 2, 4, 1, 3, 3, 0, 0, 3, 1, 3, 1…
## $ pcl8              <dbl> 1, 2, 4, 0, 1, 0, 0, 0, 2, 1, 0, 2, 1, 0, 2, 0, 1, 0…
## $ pcl9              <dbl> 1, 1, 4, 1, 0, 0, 1, 2, 4, 0, 0, 3, 2, 0, 3, 0, 2, 0…
## $ pcl10             <dbl> 1, 2, 4, 0, 3, 0, 1, 0, 4, 1, 1, 4, 0, 0, 3, 0, 1, 1…
## $ pcl11             <dbl> 1, 2, 4, 4, 3, 0, 1, 0, 4, 0, 0, 3, 0, 0, 4, 0, 2, 2…
## $ pcl12             <dbl> 1, 3, 0, 0, 0, 1, 2, 1, 4, 0, 1, 3, 0, 0, 2, 0, 2, 0…
## $ pcl13             <dbl> 1, 3, 0, 3, 1, 1, 2, 3, 4, 0, 0, 3, 0, 0, 4, 0, 2, 1…
## $ pcl14             <dbl> 1, 2, 0, 3, 2, 1, 0, 1, 3, 0, 1, 2, 1, 0, 3, 0, 2, 0…
## $ pcl15             <dbl> 1, 2, 0, 2, 0, 3, 2, 0, 4, 0, 3, 4, 0, 0, 3, 0, 1, 0…
## $ pcl16             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 1, 2, 0, 1, 0, 0, 0…
## $ pcl17             <dbl> 2, 0, 4, 2, 0, 4, 4, 0, 4, 0, 1, 4, 1, 0, 1, 0, 1, 1…
## $ pcl18             <dbl> 2, 1, 2, 0, 0, 4, 3, 0, 4, 0, 1, 4, 1, 0, 1, 0, 1, 0…
## $ pcl19             <dbl> 1, 3, 4, 1, 2, 4, 3, 1, 3, 1, 0, 4, 2, 0, 2, 0, 2, 0…
## $ pcl20             <dbl> 1, 4, 4, 0, 0, 0, 2, 0, 3, 1, 1, 2, 1, 0, 3, 0, 2, 0…
## $ pts_dtotal        <dbl> 23, 46, 53, 34, 25, 28, 37, 12, 68, 8, 19, 63, 13, 1…
## $ hads1             <dbl> 3, 3, 4, 4, 3, 3, 3, 3, 2, 3, 4, 2, 4, 4, 3, 3, 3, 4…
## $ hads2             <dbl> 1, 2, 0, 0, 1, 1, 1, 1, 2, 0, 1, 1, 2, 1, 2, 1, 1, 0…
## $ hads3             <dbl> 3, 3, 3, 0, 0, 2, 3, 3, 3, 3, 0, 4, 0, 3, 4, 0, 3, 0…
## $ hads4             <dbl> 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0…
## $ hads5             <dbl> 3, 2, 1, 1, 2, 2, 2, 1, 2, 3, 1, 2, 1, 1, 2, 0, 2, 1…
## $ hads6             <dbl> 4, 4, 0, 4, 0, 4, 4, 4, 4, 0, 4, 3, 4, 4, 4, 4, 4, 0…
## $ hads7             <dbl> 1, 2, 2, 1, 1, 1, 2, 3, 2, 2, 2, 2, 2, 2, 1, 1, 2, 0…
## $ hads8             <dbl> 4, 4, 0, 0, 4, 2, 3, 3, 2, 2, 4, 3, 4, 3, 3, 3, 4, 0…
## $ hads9             <dbl> 0, 4, 2, 4, 0, 3, 4, 0, 3, 4, 4, 3, 0, 4, 4, 0, 4, 0…
## $ hads10            <dbl> 4, 4, 2, 0, 4, 4, 4, 0, 2, 4, 0, 3, 2, 4, 3, 3, 4, 0…
## $ hads11            <dbl> 1, 2, 3, 1, 1, 2, 0, 3, 2, 2, 2, 2, 0, 1, 1, 0, 1, 0…
## $ hads12            <dbl> 4, 3, 0, 0, 0, 4, 4, 4, 3, 4, 3, 2, 4, 3, 3, 0, 4, 0…
## $ hads13            <dbl> 1, 3, 2, 0, 1, 2, 3, 0, 2, 3, 1, 2, 1, 1, 1, 2, 2, 0…
## $ hads14            <dbl> 2, 1, 2, 0, 0, 1, 0, 2, 2, 1, 1, 2, 0, 0, 1, 0, 1, 0…
## $ had_sdep          <dbl> 12, 19, 17, 11, 8, 15, 17, 13, 16, 20, 14, 17, 8, 16…
## $ had_sanx          <dbl> 19, 19, 4, 4, 9, 16, 17, 14, 16, 12, 14, 15, 17, 15,…
## $ had_stotal        <dbl> 31, 38, 21, 15, 17, 31, 34, 27, 32, 32, 28, 32, 25, …
## $ rrs1              <dbl> 2, 4, 1, 2, 3, 1, 2, 4, 4, 1, 1, 4, 1, 2, 2, 1, 3, 1…
## $ rrs2              <dbl> 2, 3, 1, 2, 1, 4, 3, 1, 4, 2, 4, 4, 3, 3, 2, 3, 2, 3…
## $ rrs3              <dbl> 2, 3, 1, 2, 3, 4, 3, 1, 4, 4, 1, 4, 2, 2, 2, 3, 2, 2…
## $ rrs4              <dbl> 2, 3, 2, 3, 3, 4, 3, 1, 4, 2, 1, 4, 3, 2, 3, 3, 3, 2…
## $ rrs5              <dbl> 1, 4, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1…
## $ rrs6              <dbl> 2, 4, 4, 2, 4, 4, 2, 1, 4, 3, 4, 3, 2, 2, 3, 2, 3, 2…
## $ rrs7              <dbl> 1, 4, 1, 1, 3, 4, 3, 4, 4, 1, 1, 4, 2, 3, 3, 1, 3, 1…
## $ rrs8              <dbl> 3, 4, 2, 1, 3, 4, 4, 1, 4, 4, 1, 4, 1, 2, 3, 3, 3, 1…
## $ rrs9              <dbl> 1, 3, 2, 2, 1, 4, 2, 1, 4, 3, 2, 4, 2, 2, 3, 3, 2, 2…
## $ rrs10             <dbl> 2, 3, 2, 2, 3, 4, 3, 1, 4, 1, 1, 3, 3, 2, 3, 3, 2, 2…
## $ age               <dbl> 22, 21, 28, 18, 20, 22, 21, 41, 24, 22, 22, 23, 55, …
## $ gender            <dbl> 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1…
## $ education         <dbl> 2, 1, 5, 1, 1, 1, 1, 3, 3, 2, 1, 1, 2, 4, 3, 2, 3, 4…
## $ ethnicity         <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4…
## $ relgion           <chr> "None", "Muslim", "Catholic", "Catholic", "Catholic …
## $ religioncode      <dbl> 0, 3, 1, 1, 1, 3, 3, 1, 3, 0, 0, 3, 1, 5, 5, 0, 5, 0…
```


```r
ptsd_long <- ptsd %>%
  pivot_longer(cols = -c(participantnumber, timecode, had_stotal, age, gender, education, ethnicity, relgion, religioncode, group, traumacoding, pts_dtotal, had_sdep, had_sanx),
    names_to = "scale",
    values_to = "score",
    names_pattern = "(.*)"
  )
```



```r
ptsd_long <- ptsd_long %>%
  separate(scale, into = c("test", "q"), sep = "(?<=\\D)(?=\\d)", convert = TRUE)
ptsd_long
```

```
## # A tibble: 11,308 × 17
##    particip…¹ group traum…² timec…³ pts_d…⁴ had_s…⁵ had_s…⁶ had_s…⁷   age gender
##         <dbl> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <dbl>  <dbl>
##  1          1     1       4       3      23      12      19      31    22      2
##  2          1     1       4       3      23      12      19      31    22      2
##  3          1     1       4       3      23      12      19      31    22      2
##  4          1     1       4       3      23      12      19      31    22      2
##  5          1     1       4       3      23      12      19      31    22      2
##  6          1     1       4       3      23      12      19      31    22      2
##  7          1     1       4       3      23      12      19      31    22      2
##  8          1     1       4       3      23      12      19      31    22      2
##  9          1     1       4       3      23      12      19      31    22      2
## 10          1     1       4       3      23      12      19      31    22      2
## # … with 11,298 more rows, 7 more variables: education <dbl>, ethnicity <dbl>,
## #   relgion <chr>, religioncode <dbl>, test <chr>, q <int>, score <dbl>, and
## #   abbreviated variable names ¹​participantnumber, ²​traumacoding, ³​timecode,
## #   ⁴​pts_dtotal, ⁵​had_sdep, ⁶​had_sanx, ⁷​had_stotal
```
Preparing brooding/age/culture data for depression dataset


```r
dep$culture <- as.factor(dep$culture)
class(dep$culture)
```

```
## [1] "factor"
```

Making sure the "culture" code is actually 0 for Euro-Australian and 1 for Malaysian - 
checking average ages against table in the paper


```r
dep %>% 
  filter(culture==0) %>% 
  summarize(max_age = max(age, na.rm =T),
            min_age = min(age,na.rm =T),
            avg_age = mean(age,na.rm =T),
            total = n())
```

```
## # A tibble: 1 × 4
##   max_age min_age avg_age total
##     <dbl>   <dbl>   <dbl> <int>
## 1      64      18    31.5   109
```


```r
dep %>% 
  filter(culture==1) %>% 
  summarize(max_age = max(age, na.rm =T),
            min_age = min(age,na.rm =T),
            avg_age = mean(age,na.rm =T),
            total = n())
```

```
## # A tibble: 1 × 4
##   max_age min_age avg_age total
##     <dbl>   <dbl>   <dbl> <int>
## 1      64      18    25.9   142
```

Making age categories

```r
dep <- dep %>% 
  mutate(age_cat = case_when(age < 21 ~ "18 - 21",
                             age >= 21 & age < 25 ~ "21 - 25",
                             age >= 25 & age < 30 ~ "25 - 30",
                             age >= 30 & age < 35 ~ "30 - 35",
                             age >= 35 & age < 40 ~ "35 - 40",
                             age >= 40 & age < 45 ~ "40 - 45",
                             age >= 45 & age < 50 ~ "45 - 50",
                             age >= 50 & age < 55 ~ "50 - 55",
                             age >= 55 & age < 60 ~ "55 - 60",
                             age >= 60 ~ "Over 60"))
```



```r
class(dep$age_cat)
```

```
## [1] "character"
```

Checking the data before plotting

```r
dpage <- dep %>%
  group_by(culture, age_cat) %>%
  summarize(avg_brood = mean(brooding_total, na.rm = T), .groups ="keep") %>%
  select(culture, age_cat, avg_brood)
  
dpage$age_cat <- factor(dpage$age_cat, levels=c(
    "18 - 21",
    "21 - 25",
    "25 - 30",
    "30 - 35",
    "35 - 40",
    "40 - 45",
    "45 - 50",
    "50 - 55",
    "55 - 60",
    "Over 60"))

dpage
```

```
## # A tibble: 22 × 3
## # Groups:   culture, age_cat [22]
##    culture age_cat avg_brood
##    <fct>   <fct>       <dbl>
##  1 0       18 - 21     12.2 
##  2 0       21 - 25      9.92
##  3 0       25 - 30      7.47
##  4 0       30 - 35      9.78
##  5 0       35 - 40      9.75
##  6 0       40 - 45      7.25
##  7 0       45 - 50      6.33
##  8 0       50 - 55      6.25
##  9 0       55 - 60      5.83
## 10 0       Over 60      8   
## # … with 12 more rows
```

Reordering column order by age, 
and make them the same scale (0-14)
before plotting


```r
dpage %>% 
  filter(!is.na(age_cat)) %>% 
  ggplot(aes(x = age_cat, y = avg_brood, fill = age_cat)) +
  geom_col(alpha = 0.5, color = "black")+
  facet_grid(culture ~ .,
             scales = "free_y", 
             space = "free", 
             labeller = labeller(culture = c(`0` = "Euro-Australian", `1` = "Malaysian"))) +
  scale_x_discrete(limits=c(
    "18 - 21",
    "21 - 25",
    "25 - 30",
    "30 - 35",
    "35 - 40",
    "40 - 45",
    "45 - 50",
    "50 - 55",
    "55 - 60",
    "Over 60"))+
  ylim(0,14)+
  labs(title = "Brooding by Age Category and Culture",
      x = NULL,
      y = "Average Brooding Score",
      fill = "Age Category") +
  theme(plot.title = element_text(size = 12, face = "bold"),
  plot.subtitle = element_text(size = 10),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(angle = 45, hjust = 1))
```

![](15Mar_project_JKA_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
Preparing depression/brooding/culture data from depression dataset


```r
dp2 <- dep %>%
group_by(culture, depression_total) %>%
  summarize(avg_brood = mean(brooding_total, na.rm = T), .groups ="keep") %>%
  select(culture, depression_total, avg_brood)
  
dp2$depression_total <- factor(dp2$depression_total)
```


```r
dp2 %>% 
  filter(!is.na(depression_total)) %>% 
  ggplot(aes(x = depression_total, y = avg_brood, fill = depression_total)) +
  geom_col(alpha = 0.5, color = "black")+
  facet_grid(culture ~ .,
             scales = "free_y", 
             space = "free", 
             labeller = labeller(culture = c(`0` = "Euro-Australian", `1` = "Malaysian"))) 
```

![](15Mar_project_JKA_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
Fixing labels

```r
dp2 %>% 
  filter(!is.na(depression_total)) %>% 
  ggplot(aes(x = depression_total, y = avg_brood, fill = depression_total)) +
  geom_col(alpha = 0.5, color = "black")+
  facet_grid(culture ~ .,
             scales = "free_y", 
             space = "free", 
             labeller = labeller(culture = c(`0` = "Euro-Australian", `1` = "Malaysian")))+ 
  ylim(0,18)+
labs(title = "Brooding by Depression Level and Culture",
       x = "Total Depression Score",
       y = "Average Brooding Score",
       fill = "Total Depression Score") + 
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8))
```

![](15Mar_project_JKA_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
Preparing brooding/anxiety/culture data from depression dataset


```r
danx <- dep %>% 
  group_by(culture, anxiety_total) %>% 
  summarize(avg_brood = mean(brooding_total, na.rm = T), .groups ="keep")

danx$anxiety_total <- factor(danx$anxiety_total)
```


```r
danx <- danx %>% 
  filter(!is.na(anxiety_total) & 
         !is.na(avg_brood) & 
         !is.na(culture))

danx <- danx[-35,]

ggplot(danx, aes(x = anxiety_total, y = avg_brood, fill = anxiety_total)) +
  geom_col(alpha = 0.5, color = "black") +
  facet_grid(culture ~ .,
             scales = "free_y", 
             space = "free", 
             labeller = labeller(culture = c(`0` = "Euro-Australian", `1` = "Malaysian")))+ 
  scale_y_continuous(limits = c(0, 20))+
   labs(title = "Brooding by Anxiety Level and Culture",
  x = "Total Anxiety Score",
  y = "Average Brooding Score",
  fill = "Total Anxiety Score") + 
  theme(plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8))
```

![](15Mar_project_JKA_files/figure-html/unnamed-chunk-21-1.png)<!-- -->
Preparing brooding/depression/culture data from ptsd dataset: 

brooding_total = total sum of rrs (repetitive rumination scores) for each participant
had_sdep = total depression score for each (Hospital Anxiety & Depression Scale)
had_sanx = total anxiety score for each
pts_dtotal= total score on PTSD scale  
traumacoding = code for initial trauma leading to PTSD
group = culture: 1 for Euro-Australian, 2 for Malaysian


```r
glimpse(ptsd_long)
```

```
## Rows: 11,308
## Columns: 17
## $ participantnumber <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
## $ group             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
## $ traumacoding      <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4…
## $ timecode          <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3…
## $ pts_dtotal        <dbl> 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, …
## $ had_sdep          <dbl> 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, …
## $ had_sanx          <dbl> 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, …
## $ had_stotal        <dbl> 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, …
## $ age               <dbl> 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, …
## $ gender            <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2…
## $ education         <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2…
## $ ethnicity         <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4…
## $ relgion           <chr> "None", "None", "None", "None", "None", "None", "Non…
## $ religioncode      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ test              <chr> "pcl", "pcl", "pcl", "pcl", "pcl", "pcl", "pcl", "pc…
## $ q                 <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1…
## $ score             <dbl> 1, 0, 0, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 0, 2, 2…
```

Computing the sums of rrs scores (total brooding) for each participant, ptsd dataset 


Comibine culture(group) and participant number into a single "id"

```r
ptsd_long$id <- paste(ptsd_long$group, ptsd_long$participantnumber, sep = "_")
```


```r
glimpse(ptsd_long)
```

```
## Rows: 11,308
## Columns: 18
## $ participantnumber <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
## $ group             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
## $ traumacoding      <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4…
## $ timecode          <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3…
## $ pts_dtotal        <dbl> 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, …
## $ had_sdep          <dbl> 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, …
## $ had_sanx          <dbl> 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, …
## $ had_stotal        <dbl> 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, …
## $ age               <dbl> 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, …
## $ gender            <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2…
## $ education         <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2…
## $ ethnicity         <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4…
## $ relgion           <chr> "None", "None", "None", "None", "None", "None", "Non…
## $ religioncode      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
## $ test              <chr> "pcl", "pcl", "pcl", "pcl", "pcl", "pcl", "pcl", "pc…
## $ q                 <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1…
## $ score             <dbl> 1, 0, 0, 1, 1, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 0, 2, 2…
## $ id                <chr> "1_1", "1_1", "1_1", "1_1", "1_1", "1_1", "1_1", "1_…
```


Computing sum of rrs scores

```r
ptsd_rrs_sum <- ptsd_long %>%
  filter(test == "rrs") %>%
 filter(!is.na(score)) %>%
 group_by(id) %>%
 summarise(rrs_sum = sum(score))
ptsd_rrs_sum
```

```
## # A tibble: 228 × 2
##    id    rrs_sum
##    <chr>   <dbl>
##  1 1_1        18
##  2 1_10       22
##  3 1_100      31
##  4 1_101      18
##  5 1_102      31
##  6 1_104      29
##  7 1_105      22
##  8 1_106      17
##  9 1_107      34
## 10 1_108      33
## # … with 218 more rows
```

Separating the id back into participant & culture for plotting


```r
ptsd_sum_sep <- ptsd_rrs_sum %>% 
  separate (id, into = c("group", "participantnumber"), sep = "_", convert = TRUE) 
ptsd_sum_sep  
```

```
## # A tibble: 228 × 3
##    group participantnumber rrs_sum
##    <int>             <int>   <dbl>
##  1     1                 1      18
##  2     1                10      22
##  3     1               100      31
##  4     1               101      18
##  5     1               102      31
##  6     1               104      29
##  7     1               105      22
##  8     1               106      17
##  9     1               107      34
## 10     1               108      33
## # … with 218 more rows
```


```r
ptsd_sum_sep$participantnumber <- as.factor(ptsd_sum_sep$participantnumber)
ptsd_sum_sep$rrs_sum <- as.factor(ptsd_sum_sep$rrs_sum)
ptsd_long$participantnumber <- as.factor(ptsd_long$participantnumber)
```


```r
glimpse(ptsd_sum_sep)
```

```
## Rows: 228
## Columns: 3
## $ group             <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
## $ participantnumber <fct> 1, 10, 100, 101, 102, 104, 105, 106, 107, 108, 109, …
## $ rrs_sum           <fct> 18, 22, 31, 18, 31, 29, 22, 17, 34, 33, 28, 18, 36, …
```
Adding "had_sdep" to ptsd_sum_sep


```r
# remove duplicates from ptsd_sum_sep
ptsd_sum_sep <- distinct(ptsd_sum_sep, participantnumber, .keep_all = TRUE)
```


```r
# remove duplicates from ptsd_long
ptsd_long <- distinct(ptsd_long, participantnumber, had_sdep, .keep_all = TRUE)
glimpse(ptsd_long)
```

```
## Rows: 220
## Columns: 18
## $ participantnumber <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1…
## $ group             <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
## $ traumacoding      <dbl> 4, 1, 1, 7, 3, 6, 1, 6, 3, 1, 3, 6, 3, 1, 8, 5, 6, 1…
## $ timecode          <dbl> 3.0, 2.0, 5.0, 1.0, 0.5, 3.0, 0.5, 24.0, 4.0, 3.0, 2…
## $ pts_dtotal        <dbl> 23, 46, 53, 34, 25, 28, 37, 12, 68, 8, 19, 63, 13, 1…
## $ had_sdep          <dbl> 12, 19, 17, 11, 8, 15, 17, 13, 16, 20, 14, 17, 8, 16…
## $ had_sanx          <dbl> 19, 19, 4, 4, 9, 16, 17, 14, 16, 12, 14, 15, 17, 15,…
## $ had_stotal        <dbl> 31, 38, 21, 15, 17, 31, 34, 27, 32, 32, 28, 32, 25, …
## $ age               <dbl> 22, 21, 28, 18, 20, 22, 21, 41, 24, 22, 22, 23, 55, …
## $ gender            <dbl> 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1…
## $ education         <dbl> 2, 1, 5, 1, 1, 1, 1, 3, 3, 2, 1, 1, 2, 4, 3, 2, 3, 4…
## $ ethnicity         <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4…
## $ relgion           <chr> "None", "Muslim", "Catholic", "Catholic", "Catholic …
## $ religioncode      <dbl> 0, 3, 1, 1, 1, 3, 3, 1, 3, 0, 0, 3, 1, 5, 5, 0, 5, 0…
## $ test              <chr> "pcl", "pcl", "pcl", "pcl", "pcl", "pcl", "pcl", "pc…
## $ q                 <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
## $ score             <dbl> 1, 3, 4, 3, 3, 1, 1, 0, 3, 0, 1, 4, 0, 0, 2, 1, 2, 3…
## $ id                <chr> "1_1", "1_2", "1_3", "1_4", "1_5", "1_6", "1_7", "1_…
```


```r
glimpse(ptsd_sum_sep)
```

```
## Rows: 140
## Columns: 3
## $ group             <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
## $ participantnumber <fct> 1, 10, 100, 101, 102, 104, 105, 106, 107, 108, 109, …
## $ rrs_sum           <fct> 18, 22, 31, 18, 31, 29, 22, 17, 34, 33, 28, 18, 36, …
```




```r
# join the datasets
ptsd_sum_sep <- left_join(ptsd_long, ptsd_sum_sep, by = "participantnumber")
```



```r
str(ptsd_sum_sep$group.x)
```

```
##  num [1:220] 1 1 1 1 1 1 1 1 1 1 ...
```


```r
ptsd_sum_sep$group.x <- factor(ptsd_sum_sep$group.x)
class(ptsd_sum_sep$group.x)
```

```
## [1] "factor"
```
Removing extra columns for ptsd_sum_sep after the join


```r
ptsd_sum_sep <- subset(ptsd_sum_sep, select = -c(group.y))
```


```r
ptsd_sum_sep <- ptsd_sum_sep %>% 
  rename(group = group.x) %>% 
   slice(-220)
```


Plotting the scatterplot


```r
# Plot the scatter plot
ggplot(ptsd_sum_sep, aes(x = as.numeric(had_sdep), y = as.numeric(rrs_sum), color = group)) +
  geom_point(size = 0.75) +
  scale_color_manual(values = my_palette) +
  facet_grid(group ~ .,
             scales = "free_y", 
             space = "free", 
             labeller = labeller(group.x = c(`1` = "Euro-Australian", `2` = "Malaysian")))+ 
  ylim(0,40)+
  labs(title = "Brooding by Depression Score and Culture, PTSD",
  x = "Total Depression Score",
  y = "Total Brooding Score",
  fill = "Culture") + 
  theme(plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.position = "none")
```

![](15Mar_project_JKA_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

Converting trauma numbers to abbreviated codes for initial trauma:

AI = accident/injury/serious illness
NSA = non-sexual abuse
SA = sexual abuse
CD = witnessing death
WND = war/natural disaster
OTH = other


```r
ptsd_t1 <- ptsd_long %>% 
  filter(between(as.numeric(as.character(traumacoding)), 1, 6)) %>% 
  mutate(index_t = case_when (traumacoding == 1 ~ "AI",
                              traumacoding == 2 ~ "NSA",
                              traumacoding == 3 ~ "SA",
                              traumacoding == 4 ~ "CD",
                              traumacoding == 5 ~ "WND",
                              traumacoding == 6 ~ "OTH"))
```



```r
ptsd_sum_sep$participantnumber <- as.factor(ptsd_sum_sep$participantnumber)
class(ptsd_sum_sep$participantnumber)
```

```
## [1] "factor"
```


Adding traumacoding to ptsd_sum_sep


```r
ptsd_sum_sep <- distinct(ptsd_sum_sep, participantnumber, .keep_all = TRUE)
ptsd_t1 <- distinct(ptsd_t1, participantnumber, .keep_all = TRUE)
```



```r
ptsd_sum_sep <- left_join(ptsd_sum_sep, ptsd_t1 %>% 
                             select(participantnumber, index_t), by = "participantnumber")
```



```r
ptsd_sum_sep %>% 
  filter(!is.na(rrs_sum) & !is.na(index_t))%>%
  ggplot(aes(x = index_t, y = as.numeric(rrs_sum),  color = index_t)) +
  geom_point(na.rm = TRUE)+
  facet_grid(group ~ .,
             scales = "free_y", 
             space = "free", 
             labeller = labeller(group = c(`1` = "Euro-Australian", `2` = "Malaysian"))) +
  ylim(0,40)+
  labs(title = "Brooding by Initial Trauma and Culture, PTSD",
      x = "Initial Trauma",
      y = "Brooding Score",
      color = "Initial Trauma",
      fill = "Initial Trauma") +
  theme(plot.title = element_text(size = 12, face = "bold"),
  plot.subtitle = element_text(size = 10),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_discrete(limits = c("AI", "CD", "NSA", "SA", "WND", "OTH")) +
  scale_color_discrete(limits = c("AI", "CD", "NSA", "SA", "WND", "OTH"),
                       labels = c("Accident/Injury", "Witnessing Death", "Non-Sexual Abuse", "Sexual Abuse", "War/Natural Disaster", "Other"))
```

![](15Mar_project_JKA_files/figure-html/unnamed-chunk-42-1.png)<!-- -->



There were 10 participant numbers with no index trauma information, 5 Euro-Australian and 5 Malaysian. 



Preparing data for plotting brooding/PTSD level/culture

Making PTSD level categories


```r
ptsd_sum_sep <- ptsd_sum_sep %>% 
  mutate(ptsdlev = case_when(pts_dtotal <= 10 ~ "Unnoticeable",
                             pts_dtotal >= 11 & pts_dtotal < 20 ~ "Very Mild",
                             pts_dtotal >= 21 & pts_dtotal < 30 ~ "Mild",
                             pts_dtotal >= 31 & pts_dtotal < 40 ~ "Significant",
                             pts_dtotal >= 41 & pts_dtotal < 50 ~ "Marked",
                             pts_dtotal >= 51 & pts_dtotal < 60 ~ "Very Marked",
                             pts_dtotal >= 60 ~ "Severe"))
```



```r
ptsd_sum_sep$ptsdlev <- factor(ptsd_sum_sep$ptsdlev, levels = c("Unnoticeable", "Very Mild", "Mild", "Moderate", "Significant", "Marked", "Severe"))
ggplot(ptsd_sum_sep[!is.na(ptsd_sum_sep$ptsdlev),], aes(x = ptsdlev, y = as.numeric(rrs_sum), color = factor(ptsdlev))) +
  geom_point(size = 1) +
  scale_color_manual(values = my_palette) +
 scale_color_discrete(breaks = c(
  "Unnoticeable",
  "Very Mild",
  "Mild",
  "Moderate",
  "Significant",
  "Marked",
  "Severe"),
    labels = c(
  "Unnoticeable",
  "Very Mild",
  "Mild",
  "Moderate",
  "Significant",
  "Marked",
  "Severe"
))+
   facet_grid(group ~ ., scales = "free_y", space = "free", 
             labeller = labeller(group = c(`1` = "Euro-Australian", `2` = "Malaysian"))) + 
   ylim(0, 45) +
  labs(title = "Brooding by PTSD Level and Culture, PTSD",
       x = "PTSD Level",
       y = "Total Brooding Score",
       color = "PTSD Level",
       fill = "PTSD Level") + 
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 8))
```

```
## Scale for colour is already present.
## Adding another scale for colour, which will replace the existing scale.
```

![](15Mar_project_JKA_files/figure-html/unnamed-chunk-44-1.png)<!-- -->

Plotting Brooding/Anxiety by Culture


```r
 ggplot(ptsd_sum_sep, aes(x = as.numeric(had_sanx), y = as.numeric(rrs_sum), color = factor(group))) +
  geom_point(size = 0.75) +
  scale_color_manual(values = my_palette) +
  facet_grid(group ~ .,
             scales = "free_y", 
             space = "free", 
             labeller = labeller(group = c(`1` = "Euro-Australian", `2` = "Malaysian")))+ 
  ylim(0,40)+
  labs(title = "Brooding by Anxiety Score and Culture, PTSD",
  x = "Total Anxiety Score",
  y = "Total Brooding Score",
  fill = "Culture") + 
  theme(plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.position = "none")
```

![](15Mar_project_JKA_files/figure-html/unnamed-chunk-45-1.png)<!-- -->
Preparing data for brooding/age category/culture, PTSD


Making age categories

```r
ptsd_sum_sep <- ptsd_sum_sep %>% 
  mutate(age_cat = case_when(age < 21 ~ "18 - 21",
                             age >= 21 & age < 25 ~ "21 - 25",
                             age >= 25 & age < 30 ~ "25 - 30",
                             age >= 30 & age < 35 ~ "30 - 35",
                             age >= 35 & age < 40 ~ "35 - 40",
                             age >= 40 & age < 45 ~ "40 - 45",
                             age >= 45 & age < 50 ~ "45 - 50",
                             age >= 50 & age < 55 ~ "50 - 55",
                             age >= 55 & age < 60 ~ "55 - 60",
                             age >= 60 ~ "Over 60"))
```


```r
glimpse(ptsd_sum_sep)
```

```
## Rows: 140
## Columns: 22
## $ participantnumber <fct> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1…
## $ group             <fct> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
## $ traumacoding      <dbl> 4, 1, 1, 7, 3, 6, 1, 6, 3, 1, 3, 6, 3, 1, 8, 5, 6, 1…
## $ timecode          <dbl> 3.0, 2.0, 5.0, 1.0, 0.5, 3.0, 0.5, 24.0, 4.0, 3.0, 2…
## $ pts_dtotal        <dbl> 23, 46, 53, 34, 25, 28, 37, 12, 68, 8, 19, 63, 13, 1…
## $ had_sdep          <dbl> 12, 19, 17, 11, 8, 15, 17, 13, 16, 20, 14, 17, 8, 16…
## $ had_sanx          <dbl> 19, 19, 4, 4, 9, 16, 17, 14, 16, 12, 14, 15, 17, 15,…
## $ had_stotal        <dbl> 31, 38, 21, 15, 17, 31, 34, 27, 32, 32, 28, 32, 25, …
## $ age               <dbl> 22, 21, 28, 18, 20, 22, 21, 41, 24, 22, 22, 23, 55, …
## $ gender            <dbl> 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1…
## $ education         <dbl> 2, 1, 5, 1, 1, 1, 1, 3, 3, 2, 1, 1, 2, 4, 3, 2, 3, 4…
## $ ethnicity         <dbl> 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4…
## $ relgion           <chr> "None", "Muslim", "Catholic", "Catholic", "Catholic …
## $ religioncode      <dbl> 0, 3, 1, 1, 1, 3, 3, 1, 3, 0, 0, 3, 1, 5, 5, 0, 5, 0…
## $ test              <chr> "pcl", "pcl", "pcl", "pcl", "pcl", "pcl", "pcl", "pc…
## $ q                 <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
## $ score             <dbl> 1, 3, 4, 3, 3, 1, 1, 0, 3, 0, 1, 4, 0, 0, 2, 1, 2, 3…
## $ id                <chr> "1_1", "1_2", "1_3", "1_4", "1_5", "1_6", "1_7", "1_…
## $ rrs_sum           <fct> 18, 35, 17, 18, 25, 34, 26, 16, 38, 22, 18, 36, 20, …
## $ index_t           <chr> "CD", "AI", "AI", "AI", "SA", "OTH", "AI", "OTH", "S…
## $ ptsdlev           <fct> Mild, Marked, NA, Significant, Mild, Mild, Significa…
## $ age_cat           <chr> "21 - 25", "21 - 25", "25 - 30", "18 - 21", "18 - 21…
```


```r
ptsd_sum_sep$age_cat <- factor(ptsd_sum_sep$age_cat, levels=c(
    "18 - 21",
    "21 - 25",
    "25 - 30",
    "30 - 35",
    "35 - 40",
    "40 - 45",
    "45 - 50",
    "50 - 55",
    "55 - 60",
    "Over 60"))
```

Reordering column order by age, 
and make them the same scale (0-40)
before plotting


```r
ptsd_sum_sep %>% 
  filter(!is.na(age_cat)) %>% 
  ggplot(aes(x = age_cat, y = as.numeric(rrs_sum), color = factor(age_cat))) +
  geom_point(na.rm = TRUE)+
  facet_grid(group ~ .,
             scales = "free_y", 
             space = "free", 
             labeller = labeller(culture = c(`1` = "Euro-Australian", `2` = "Malaysian"))) +
  scale_x_discrete(limits=c(
    "18 - 21",
    "21 - 25",
    "25 - 30",
    "30 - 35",
    "35 - 40",
    "40 - 45",
    "45 - 50",
    "50 - 55",
    "55 - 60",
    "Over 60"))+
  ylim(0,40)+
  labs(title = "Brooding by Age Category and Culture, PTSD",
      x = NULL,
      y = "Brooding Score",
      fill = "Age Category",
      color = "Age Category") +
  theme(plot.title = element_text(size = 12, face = "bold"),
  plot.subtitle = element_text(size = 10),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(angle = 45, hjust = 1))
```

![](15Mar_project_JKA_files/figure-html/unnamed-chunk-49-1.png)<!-- -->

