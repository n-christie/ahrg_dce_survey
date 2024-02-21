
library(pacman)

p_load(tidyverse, here, stringr, formr, logitr, cbcTools, texreg, likert, tidyr, haven, kableExtra, sjlabelled, summarytools)

# load data


survey_design <- readRDS("data/survey_design.rds")

survey_df <- readRDS("data/ahrg_test.rds")

# Sum stats ---- 

dfSum <- survey_df %>%
  # dplyr::select(plantorelocate,
  #               monthcost,
  #               screenout,
  #               income,
  #               gender,
  #               own,
  #               education,
  #               house,
  #               feedback11, 
  #               rating_button01,
  #               rating_button0122
# ) |> 
mutate(plantorelocate = factor(plantorelocate, levels = 1:2,
                               labels = c("Yes","No")),
       screenout = factor(screenout, levels = 1:3,
                          labels = c("Only house","Only apt", "Both")),
       gender = factor(gender, levels = 1:2,
                       labels = c("Male","Female")),
       education = factor(education, levels = 1:3,
                          labels = c("Elementary","High School", "University")),
       own = factor(own, levels = 1:3,
                    labels = c("Hyresrätt","Bostadsrätt", "Äganderätt")),
       house = factor(house, levels = 1:4,
                      labels = c("Småhus","Flerbostadshus", "Övriga hus","Specialbostäder")),
       feedback11 = factor(feedback11, levels = 1:3,
                           labels = c("Mobile","Desktop", "Tablet"))
       
       
) %>% 
  as.data.frame()

label(dfSum$plantorelocate) <- "Plan to relocate?"
label(dfSum$monthcost) <- "Monthly costs"
label(dfSum$income) <- "Monthly household income"
label(dfSum$own) <- "Own house"
label(dfSum$house) <- "Kind of house"
label(dfSum$feedback11) <- "Kind of device"

# generate the desc table
t1 <- table1::table1(~ plantorelocate + monthcost + income + own + house + feedback11 ,
                     data = dfSum,
                     na.rm = TRUE,
                     digits = 1,
                     format.number = TRUE,
                     # extra.col=list(`P-value`=pvalue),
                     caption = "Sample description") 

table1::t1flex(t1) |> 
  flextable::fontsize(size = 11) |> 
  flextable::padding(padding = 1, part = "all") 


## plots ----

# How would you rate the overall experience of taking this survey (nativigating the survey, clicking buttons, entering text, etc.)?

like1 <- dfSum %>% 
  select(rating_button01) %>% 
  transmute("Overall Experience" = factor(rating_button01, levels = 1:5,
                                          labels = c("Not the best",
                                                     "2",
                                                     "3",
                                                     "4",
                                                     "Seemless and enjoyable")
  )
  ) %>% 
  as.data.frame() 
library(plyr)

plot(likert(like1),
     legend.position = 'bottom',
     text.size = 3,
     plot.percents = TRUE) +  
  theme(legend.text = element_text(size = rel(0.6)))

# How was your experience with the number of choice sets (the 9 housing alternatives) to answer?

like2 <- dfSum %>% 
  select(rating_button0122) %>% 
  transmute("No. of questions" =  factor(rating_button0122, levels = 1:5,
                                         labels = c("too many",
                                                    "many but ok",
                                                    "could have done more",
                                                    "didn't think bout it",
                                                    "many more ok")
  )
  ) %>% 
  as.data.frame() 


plot(likert(like2),
     legend.position = 'bottom',
     text.size = 3,
     plot.percents = TRUE) +  
  theme(legend.text = element_text(size = rel(0.6)))

# How would you rate the relevancy of the attributes in a potential housing choice?

like3 <- survey_df %>% 
  transmute(dist_to_green = factor(rating_button02, levels = 1:5, labels = c("Not relevant","2","3","4","Highly relevant")),
            dist_to_trans = factor(rating_button02_trans, levels = 1:5, labels = c("Not relevant","2","3","4","Highly relevant")),
            dist_to_shops = factor(rating_button02_shops, levels = 1:5, labels = c("Not relevant","2","3","4","Highly relevant")),
            cost = factor(rating_button02_price, levels = 1:5, labels = c("Not relevant","2","3","4","Highly relevant")),
            parking = factor(rating_button02_park, levels = 1:5, labels = c("Not relevant","2","3","4","Highly relevant"))
  ) %>% 
  as.data.frame() 


plot(likert(like3),
     legend.position = 'bottom',
     text.size = 3,
     plot.percents = TRUE) +  
  theme(legend.text = element_text(size = rel(0.6))) + 
  labs(title = "")

# How interpretable were the attributes?

like4 <- survey_df %>% 
  transmute(dist_to_green = factor(mc_dis, levels = 1:5, labels = c("Difficult","2","3","4","Easy")),
            dist_to_trans = factor(mc_dis_trans, levels = 1:5, labels = c("Difficult","2","3","4","Easy")),
            dist_to_shops = factor(mc_dis_shopw, levels = 1:5, labels = c("Difficult","2","3","4","Easy")),
            cost = factor(mc_dis_price, levels = 1:5, labels = c("Difficult","2","3","4","Easy")),
            parking = factor(mc_dis_park, levels = 1:5, labels = c("Difficult","2","3","4","Easy"))
  ) %>% 
  as.data.frame() 


plot(likert(like4),
     legend.position = 'bottom',
     text.size = 3,
     plot.percents = TRUE) +  
  theme(legend.text = element_text(size = rel(0.6))) + 
  labs(title = "")

# Distance in time vs. distance in km?

like5 <- survey_df %>% 
  transmute(dist_to_green = factor(alt_distance_green, levels = 1:4,
                                   labels = c("Dist in m/km fine",
                                              "Time better (walking)",
                                              "Time better (cycling)",
                                              "Time better (driving)")),
            dist_to_trans = factor(alt_distance_trans, levels = 1:4,
                                   labels = c("Dist in m/km fine",
                                              "Time better (walking)",
                                              "Time better (cycling)",
                                              "Time better (driving)")),
            dist_to_shops = factor(alt_distance_green_shops, levels = 1:4, labels = c("Dist in m/km fine","Time better (walking)","Time better (cycling)","Time better (driving)"))
  ) %>% 
  as.data.frame() 


plot(likert(like5),
     legend.position = 'bottom',
     text.size = 3,
     plot.percents = TRUE) +  
  theme(legend.text = element_text(size = rel(0.6))) + 
  labs(title = "")

# Cost questions

like6 <- survey_df %>% 
  transmute(cost_better = factor(alt_distance_green_price, levels = 1:2, labels = c("Providing an estimate based on my initial input was good.",
                                                                                    "Strictly relative in percentage terms better")),
            cost_comfort = factor(price_goo, levels = 1:4, labels = c("Not at all","Indifferent","A little uncomfortable","Very uncomforatble")) ) %>% 
  as.data.frame() 

like6a <- like6 %>% 
  select(cost_better)

like6b <- like6 %>% 
  select(cost_comfort)

plot(likert(like6a),
     legend.position = 'bottom',
     text.size = 3,
     plot.percents = TRUE) +  
  theme(legend.text = element_text(size = rel(0.6))) + 
  labs(title = "Which cost option would be best?")


plot(likert(like6b),
     legend.position = 'bottom',
     text.size = 3,
     plot.percents = TRUE) +  
  theme(legend.text = element_text(size = rel(0.6))) + 
  labs(title = "Cost questions made me uncomfortable")



