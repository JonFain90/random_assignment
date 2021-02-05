# How to Generate a Disproportionate Stratified Random Assignment in R

## Introduction

I was recently working on the design and implementation of a two-armed Randomized Control Trial (RCT). Because this study had been proven in other contexts, the study team decided to maximize the benefit of the intervention by reducing the size of the control group (12.5%), distributing the majority of the sample to the proven intervention group (T1 -50%) and allocating the rest of the sample to the unproven intervention group (T2 - 37.5%). The team also chose to stratify the sample by gender and age of the children involved to ensure equal allocation of these particular subgroups to control, treatment 1 and treatment 2. I found that the best way to conduct this type of assignment was with a combination of the randomizr and experiment packages. I'll walk you through my process below. 

## Data 

I generated a sample file of only the age and gender of those included in the trial. The intervention targeted children ages 3 -5 years old. The Sample data can be found on github [here](https://github.com/JonFain90/random_assignment/blob/master/Tips_Groups_sample.xlsx). 


```{r import packages, message=FALSE}

#install these packages
library(tidyverse)
library(readxl)
library(kableExtra)
library(randomizr)
library(experiment)

#set your working directory
setwd("C:/Users/Jonfa/OneDrive/Documents/R/projects/random assignment/data")

#read in the data
sample <- read_excel("Tips_Groups_sample.xlsx")

```

## Quick Data Exploration

The data contains 3511 observations of children of both genders ages 3 -5. As you can see from the tables below, the gender proportions were roughly the same, but there was a much larger proportion of five-year-olds than of three and four-year-olds. Because this was administrative data and we did not have access to any other data that represented the population, we assumed these proportions were representative of the particular population with which we were working. 

```{r explore}

gender_table <- table(sample$`child_gender`)
gender_table <- round(prop.table(gender_table),2)

kable(gender_table, col.names = c("Gender", "Prop")) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "center")

```

| Gender | Prop |                       
| -----| ------|
| F  | .51  |
| M | .49  |



```{r explore}
age_table <- table(sample$`child_age`)
age_table <- round(prop.table(age_table),2)

kable(age_table, col.names = c("Age", "Prop")) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "center")


```
| Age | Prop |       
| ---- | ---- |
| 3  | 0.03  |
| 4 | 0.14  |
| 5 | 0.82 |

## Random Assignment 

Given the importance of random assignment and randomization in experimental design, I decided to first generate a test table of what a random disproportionate stratified assignment _should_ look like. I used the randomizr package for this, as I found it to be the most functional package for this purpose:

```{r random assignment test}

#set your seed for reproducibility
set.seed(241990)

#assign strata 
strata <- with(sample, paste(sample$child_age, sample$child_gender, sep = "_"))


#generate proportional (prob_each) groups (conditions) by strata (blocks)
#note - prob_each should follow the same order as the conditions in order to generate desired proportions
test_groups <- block_ra(blocks = strata,
                        conditions = c("C", "T1", "T2"),
                        prob_each = c(.125, .5, .375))

#display table 
test_table <- table(strata, test_groups)
kable(test_table)%>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "center")

```

Strata| C | T1 | 	T2
| --| -- | -- | -- |
3_F	| 8| 	30| 	23
3_M	| 7	| 29| 	22
4_F | 33| 132| 	99
4_M	| 30| 120| 	90
5_F	| 183|731| 	548
5_M	| 178|713| 535


I wanted the actual table to look similar to the table above with regard to the proportions of strata in each of the three groups. So, to randomly assign each observation into groups randomly on the basis of my desired proportions and strata, I used the the experiment package's randomize function:

```{r random assignment}


randomize <- randomize(sample, group = c("C", "T1", "T2"), ratio = c(.125, .5, .375),
                    indx = NULL, block = c("child_gender", "child_age"), n.block = NULL, match = NULL,
                    complete = TRUE)

group <- randomize[["treatment"]]

sample_groups <- sample %>%
  mutate(group = group)

strata_table <- table(strata, sample_groups$group)

kable(strata_table) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "center")


```


Strata| C | T1 | 	T2
| --| -- | -- | -- |
3_F |	6 |	35 |	20 |
3_M	| 7	| 33 |	18 |
4_F |	46|	119| 99  |
4_M	|16	| 124| 100 |
5_F	|171|	743 |	548|
5_M	|193|	702	|531 |


Unlike the test generated with the randomizr package, which produced only a table, I used the experiment package to create a new dataset with an added "group" column. The second table shows the proportion of strata assigned to each group, which is very similar to the test table created with the randomizr package, thus validating the operation. Furthermore, a quick check of the percent distribution of groups showed that our operation was successful in generating our desired proportions within each group. 

```{r group percent, message=FALSE}

perc <- sample_groups %>% 
  group_by(group) %>%
summarize(total = n()) %>%
  arrange(desc(total)) %>%
  mutate(total_perc = paste0(round(per_total = total/sum(total)*100), "%"))


kable(perc) %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "center")


```
group | total	| total_perc | 
| --| -- | -- | 
T1	| 1756 | 50%
T2	| 1316 | 37%
C	  | 439	 | 13%

The last check was to test if these groups were balanced. For gender, you can dichotomize the data and run t-tests between control and treatment 1, control and treatment 2, and treatment 1 and 2 to assess whether or not there is balance between each of these groups. Note that you can also run a chi-squared test as well to check whether the proportion of men and women is approximately the same in both groups. Because our p values are well above .05, we can conclude that randomization has succeeded in generating balance between these groups. 

```{r balance gender }

gender_test <- sample_groups %>%
  mutate(child_gender = ifelse(child_gender == "M", 0, 1))

# Control vs T1
gender_test1 <- gender_test %>%
  filter(group != "T2")

gender_test1 <- t.test(gender_test1$child_gender ~ gender_test1$group, var.equal = TRUE)

# T1 vs T2
gender_test2 <- gender_test %>%
  filter(group != "T1")

gender_test2 <- t.test(gender_test2$child_gender ~ gender_test2$group, var.equal = TRUE)


# Control vs T2
gender_test3 <- gender_test %>%
  filter(group != "C")

gender_test3 <- t.test(gender_test3$child_gender ~ gender_test3$group, var.equal = TRUE)


# Create a table to display p-values 

gt <- tibble(
  `Variable` = c("Gender"),
  `C vs T1` = "",
  `T1 vs T2` = "",
  `C vs T2` = ""
)

gt$`C vs T1`  <- round(as.numeric(gender_test1$p.value), 2)
gt$`T1 vs T2` <- round(as.numeric(gender_test2$p.value), 2)
gt$`C vs T2`  <- round(as.numeric(gender_test3$p.value), 2)


  gt$`C vs T1` = cell_spec(gt$`C vs T1`,
                                   color = ifelse(gt$`C vs T1` <= 0.1 , "red", "blue"),
                                   align = "c")
    gt$`C vs T2` = cell_spec(gt$`C vs T2`,
                                     color = ifelse(gt$`C vs T2` <= 0.1 , "red", "blue"),
                                     align = "c")
  gt$`T1 vs T2` = cell_spec(gt$`T1 vs T2`,
                                    color = ifelse(gt$`T1 vs T2` <= 0.1 , "red", "blue"),
                                    align = "c")
  gt$`Variable` = cell_spec(gt$`Variable`,
                                   align = "c")
  
  
  kable(gt,
    caption = "<center><strong>Balance Test P-Values: Gender</strong></center>",
    escape = F,
    align = "c") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "center")


```

Variable |	C vs T1	|T1 vs T2 |	C vs T2
| --| -- | -- | -- | 
Gender | 0.92 |	0.97 |0.83

For age, you can generate two by two tables between groups and run chi squared tables on each. Normally you would use a t-test for age, but because we added parameters to our age group, we treated this data as ordinal. Thus, a non-parametric test like the chi-squared test is more appropriate here. Because our p values are well above .05, we can conclude that randomization has succeeded in generating balance between these groups.

```{r balance age}
sample_groups$child_age <- as.factor(sample_groups$child_age)

# Control vs T1
age_test1 <- sample_groups %>%
  filter(group != "T2")

age_test1 <- chisq.test(table(age_test1$child_age, age_test1$group))

# T1 vs T2
age_test2 <- sample_groups %>%
  filter(group != "T1")

age_test2 <- chisq.test(table(age_test2$child_age, age_test2$group))


# Control vs T2
age_test3 <- sample_groups %>%
  filter(group != "C")

age_test3 <- chisq.test(table(age_test3$child_age, age_test3$group))


# Create a table to display p-values 

at <- tibble(
  `Variable` = c("Age"),
  `C vs T1` = "",
  `T1 vs T2` = "",
  `C vs T2` = ""
)

at$`C vs T1`  <- round(as.numeric(age_test1$p.value), 2)
at$`T1 vs T2` <- round(as.numeric(age_test2$p.value), 2)
at$`C vs T2`  <- round(as.numeric(age_test3$p.value), 2)


  at$`C vs T1` = cell_spec(at$`C vs T1`,
                                   color = ifelse(at$`C vs T1` <= 0.1 , "red", "blue"),
                                   align = "c")
    at$`C vs T2` = cell_spec(at$`C vs T2`,
                                     color = ifelse(at$`C vs T2` <= 0.1 , "red", "blue"),
                                     align = "c")
  at$`T1 vs T2` = cell_spec(at$`T1 vs T2`,
                                    color = ifelse(at$`T1 vs T2` <= 0.1 , "red", "blue"),
                                    align = "c")
  at$`Variable` = cell_spec(at$`Variable`,
                                   align = "c")
  
  
  kable(at,
    caption = "<center><strong>Balance Test P-Values: Age</strong></center>",
    escape = F,
    align = "c") %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                position = "center") 



```

Variable |C vs T1 |	T1 vs T2 |	C vs T2
| --| -- | -- | -- | 
Age | 0.66 | 0.88 | 0.22

I should mention here that balance tests can be a bit controversial.For more information on this I would recommend reading Jan Vanhoves post on "Silly significance tests" [here](https://janhove.github.io/reporting/2014/09/26/balance-tests).
