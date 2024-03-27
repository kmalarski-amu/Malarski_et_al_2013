# This is the R code used for data analysis in Malarski et al. (2023) #
# Orientation towards the vernacular and style-shifting as language #
# behaviours in speech of first-generation Polish migrant communities speaking Norwegian in Norway" #

###### General dialect scores ######

##### Library
library(ggplot2)
library(dplyr)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()


##### Import data
##### set.seed(1000)
data <- data.frame(
  read.csv("~/Downloads/gen_dialect_scores.csv", header=FALSE)
)

#### use the data frame below without uploading a file
data <- data.frame(
  x <- c("AD4407AR", "HH4519IK", "LF3524AL", "TK7710ER", "BB1234JA", "MG6611AG", "TS8008UZ", "KK6310OA", "MW5613AM", "WL3725AC", "AJ4708RZ", "MM5719AR", "MW7015AR", "JP6912AR", "AK5927RZ", "KJ6814OA", "DD6822AG", "ZS6219NN", "WM6413OA", "JB5610NE", "MG4723AG", "TR6620AN", "WB5810GA", "DK7316AB", "MZ6724AG", "MZ5021NE", "TROMSL1MED", "BD5701AG", "JS5412UL", "MS5129NN", "BH7231LG", "RB6114OR", "TROMSL1M", "JM5321AR", "AK7817SK", "AK6923IC", "LS5416LI", "AS6503AU"),
  y <- c(0, 0.2, 0.2, 0.4, 0.4, 0.4, 0.6, 0.8, 0.8, 0.9, 1, 1, 1, 1.1, 1.6, 1.7, 2.2, 2.4, 2.5, 3, 3, 3, 3, 3.1, 3.5, 3.5, 4, 4, 4, 4.0, 4.1, 4.1, 4.5, 4.5, 4.7, 4.7, 4.9, 5.0)
)

##### Reorder the data
data <- data %>%
  arrange(y) %>%
  mutate(x=factor(x,x))

##### Plotting
p <- ggplot(data, aes(x=x, y=y)) +
  geom_segment(
    aes(x=x, xend=x, y=0, yend=y), 
    color=ifelse(data$x %in% c("TROMSL1M","TROMSL1MED"), "orange", "grey"), 
    size=ifelse(data$x %in% c("TROMSL1M","TROMSL1MED"), 1.3, 0.7)
  ) +
  geom_point(
    color=ifelse(data$x %in% c("TROMSL1M","TROMSL1MED"), "orange", "grey"), 
    size=ifelse(data$x %in% c("TROMSL1M","TROMSL1MED"), 5, 2)
  ) +
  theme_ipsum() +
  coord_flip() +
  theme(
    legend.position="none"
  ) +
  xlab("") +
  ylab("dialect score") +
  ggtitle("General dialect scores for Oslo and Tromsø speakers")

##### Graph annotation
p + annotate("text", x=grep("TROMSL1MED", data$x), y=data$y[which(data$x=="TROMSL1MED")]*1.2, 
             label="L1 median = 4", 
             color="orange", size=4 , angle=0, fontface="bold", hjust=0.8) + 
  
  annotate("text", x = grep("TROMSL1M$", data$x), y = data$y[which(data$x=="TROMSL1M")]*1.2, 
           label ="L1 mean = 4.5", 
           color="orange", size=4 , angle=0, fontface="bold", hjust=0.8)



####### Socio #######

###### dialect score vs. NO prof ######

name <- c("AK5927RZ", "AK7817SK", "BH7231LG", "DD6822AG", "DK7316AB", "HH4519IK", "JM5321AR", "JP6912AR", "KK6310OA", "LF3524AL", "MG6611AG", "MW5613AM", "WL3725AC", "WM6413OA", "AK6923IC", "TK7710ER", "KJ6814OA", "TS8008UZ", "AD4407AR", "AJ4708RZ", "BD5701AG", "JB5610NE", "JS5412UL", "MG4723AG", "MM5719AR", "MS5129NN", "MW7015AR", "MZ6724AG", "TR6620AN", "WB5810GA", "BB1234JA", "ZS6219NN", "RB6114OR", "LS5416LI", "AS6503AU", "MZ5021NE")
dialect_score <- c(1.6, 4.7, 4.1, 2.2, 3.1, 0.2, 4.5, 1.1, 0.8, 0.2, 0.4, 0.8, 0.9, 2.5, 4.7, 5.5, 1.7, 5.5, 0, 1, 4, 3, 4, 3, 1, 4, 1, 3.5, 3, 3, 0.4, 2.4, 4.1, 4.9, 5.0, 3.5)
NO_prof <- c(25, 28, 28, 28, 28, 24, 28, 27, 26, 14, 23, 20, 12, 26, 28, 28, 23, 28, 27, 24, 27, 26, 28, 26, 22, 27, 21, 28, 27, 28, 21, 23, 27, 28, 28, 23)

rates <- data.frame(name, NO_prof, dialect_score)

library(ggplot2)
library(hrbrthemes)


### basic scatter plot ###
(basic_mm_scatter <- ggplot(rates, aes(NO_prof, dialect_score)) +
    geom_point() +
    theme_bw())

### scatter plot with trendline ###
(basic_mm_scatter_line <- ggplot(rates, aes(NO_prof, dialect_score)) +
    geom_point() +
    theme_bw() +
    stat_smooth(method = "lm"))

### improved scatter plot ###
(improved_mm_scat <- ggplot(rates, aes(NO_prof, dialect_score, colour = NO_prof)) +
    geom_point() +
    theme_bw() +
    ylab("dialect score") +
    xlab("NO proficiency") +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))


###### dialect score vs. length of stay in Norway ######

name <- c("AK5927RZ", "AK7817SK", "BH7231LG", "DD6822AG", "DK7316AB", "HH4519IK", "JM5321AR", "JP6912AR", "KK6310OA", "LF3524AL", "MG6611AG", "MW5613AM", "WL3725AC", "WM6413OA", "AK6923IC", "TK7710ER", "KJ6814OA", "TS8008UZ", "AD4407AR", "AJ4708RZ", "BD5701AG", "JB5610NE", "JS5412UL", "MG4723AG", "MM5719AR", "MS5129NN", "MW7015AR", "MZ6724AG", "TR6620AN", "WB5810GA", "BB1234JA", "ZS6219NN", "RB6114OR", "LS5416LI", "AS6503AU", "MZ5021NE")
dialect_score <- c(1.6, 4.7, 4.1, 2.2, 3.1, 0.2, 4.5, 1.1, 0.8, 0.2, 0.4, 0.8, 0.9, 2.5, 4.7, 5.5, 1.7, 5.5, 0, 1, 4, 3, 4, 3, 1, 4, 1, 3.5, 3, 3, 0.4, 2.4, 4.1, 4.9, 5.0, 3.5)
residence <- c(2, 12.5, 20, 2.75, 15.75, 4, 9, 1.75, 6.7, 15, 2, 6.65, 16, 5, 5, 17, 9, 15, 1, 15, 13, 14.75, 32, 19, 7, 16, 8, 10, 7.25, 9, 20, 15.5, 12, 7, 7, 14)

corr2 <- data.frame(name, residence, dialect_score)

library(ggplot2)
library(hrbrthemes)


### basic scatter plot ###
(basic_mm_scatter <- ggplot(corr2, aes(residence, dialect_score)) +
    geom_point() +
    theme_bw())

### scatter plot with trendline ###
(basic_mm_scatter_line <- ggplot(corr2, aes(residence, dialect_score)) +
    geom_point() +
    theme_bw() +
    stat_smooth(method = "lm"))

### improved scatter plot ###
(improved_mm_scat <- ggplot(corr2, aes(residence, dialect_score, colour = residence)) +
    geom_point() +
    theme_bw() +
    ylab("dialect score") +
    xlab("length of stay in Norway") +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))


###### dialect score vs. % Norwegian friends ######

name <- c("AK5927RZ", "AK7817SK", "BH7231LG", "DD6822AG", "DK7316AB", "HH4519IK", "JM5321AR", "JP6912AR", "KK6310OA", "LF3524AL", "MG6611AG", "MW5613AM", "WL3725AC", "WM6413OA", "AK6923IC", "TK7710ER", "KJ6814OA", "TS8008UZ", "AD4407AR", "AJ4708RZ", "BD5701AG", "JB5610NE", "JS5412UL", "MG4723AG", "MM5719AR", "MS5129NN", "MW7015AR", "MZ6724AG", "TR6620AN", "WB5810GA", "BB1234JA", "ZS6219NN", "RB6114OR", "LS5416LI", "AS6503AU", "MZ5021NE")
dialect_score <- c(1.6, 4.7, 4.1, 2.2, 3.1, 0.2, 4.5, 1.1, 0.8, 0.2, 0.4, 0.8, 0.9, 2.5, 4.7, 5.5, 1.7, 5.5, 0, 1, 4, 3, 4, 3, 1, 4, 1, 3.5, 3, 3, 0.4, 2.4, 4.1, 4.9, 5.0, 3.5)
friends <- c(10, 75, 99, 10, 95, 2, 95, 30, 40, 20, 20, 33, 50, 20, 90, 95, 80, 90, 20, 0, 80, 70, 80, 50, 10, 10, 30, 10, 20, 20, 20, 30, 80, 30, 60, 30)

corr3 <- data.frame(name, friends, dialect_score)

library(ggplot2)
library(hrbrthemes)


### basic scatter plot ###
(basic_mm_scatter <- ggplot(corr3, aes(friends, dialect_score)) +
    geom_point() +
    theme_bw())

### scatter plot with trendline ###
(basic_mm_scatter_line <- ggplot(corr3, aes(friends, dialect_score)) +
    geom_point() +
    theme_bw() +
    stat_smooth(method = "lm"))

### improved scatter plot ###
(improved_mm_scat <- ggplot(corr3, aes(friends, dialect_score, colour = friends)) +
    geom_point() +
    theme_bw() +
    ylab("dialect score") +
    xlab("% Norwegian friends") +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))

###### dialect score vs. age ######

name <- c("AK5927RZ", "AK7817SK", "BH7231LG", "DD6822AG", "DK7316AB", "HH4519IK", "JM5321AR", "JP6912AR", "KK6310OA", "LF3524AL", "MG6611AG", "MW5613AM", "WL3725AC", "WM6413OA", "AK6923IC", "TK7710ER", "KJ6814OA", "TS8008UZ", "AD4407AR", "AJ4708RZ", "BD5701AG", "JB5610NE", "JS5412UL", "MG4723AG", "MM5719AR", "MS5129NN", "MW7015AR", "MZ6724AG", "TR6620AN", "WB5810GA", "BB1234JA", "ZS6219NN", "RB6114OR", "LS5416LI", "AS6503AU", "MZ5021NE")
dialect_score <- c(1.6, 4.7, 4.1, 2.2, 3.1, 0.2, 4.5, 1.1, 0.8, 0.2, 0.4, 0.8, 0.9, 2.5, 4.7, 5.5, 1.7, 5.5, 0, 1, 4, 3, 4, 3, 1, 4, 1, 3.5, 3, 3, 0.4, 2.4, 4.1, 4.9, 5.0, 3.5)
age <- c(30, 22, 23, 25, 23, 47, 34, 27, 29, 58, 27, 31, 59, 32, 29, 20, 35, 20, 49, 43, 42, 40, 32, 46, 39, 45, 36, 34, 33, 35, 53, 43, 37, 40, 31, 36)

corr4 <- data.frame(name, age, dialect_score)

library(ggplot2)
library(hrbrthemes)


### basic scatter plot ###
(basic_mm_scatter <- ggplot(corr4, aes(age, dialect_score)) +
    geom_point() +
    theme_bw())

### scatter plot with trendline ###
(basic_mm_scatter_line <- ggplot(corr4, aes(age, dialect_score)) +
    geom_point() +
    theme_bw() +
    stat_smooth(method = "lm"))

### improved scatter plot ###
(improved_mm_scat <- ggplot(corr4, aes(age, dialect_score, colour = age)) +
    geom_point() +
    theme_bw() +
    ylab("dialect score") +
    xlab("age") +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2)))



###### Style-shifting #####

# oslo --------------------------------------------------------------------

oslo <- read_csv("L3 dialect study (dialect index) 0–no dialect 1–dialect - Oslo (Wit.).csv") |> 
  janitor::clean_names()

oslo_scores <- oslo |> 
  select("participant_id", "nord_score", "pic_score", "fritid_score", "frokost_score",
         "daglige_rutine_score", "af_2", "tone_2_dialect_score") |> 
  mutate_if(is.numeric, scale) |> 
  mutate_if(is.numeric, as.numeric)

oslo_scores <- oslo_scores[1:20,]

oslo_scores[is.na(oslo_scores)] <- 0

oslo_wide <- oslo_scores |> 
  pivot_longer(cols = -participant_id,
               names_to = "task",
               values_to = "score")

oslo_wide$task <- str_remove_all(oslo_wide$task, "_score")

oslo_wide$task <- factor(oslo_wide$task, levels = c("tone_2_dialect", "af_2", "nord", "pic","fritid", "frokost", "daglige_rutine"))


ggplot(oslo_wide, aes(task, score, group = participant_id)) +
  geom_line() +
  facet_wrap(~participant_id)
ggsave("plots/oslo.pdf", width = 8, height = 8,device = cairo_pdf)

# tromso ------------------------------------------------------------------

tromso <- read_csv("L3 dialect study (dialect index) 0–no dialect 1–dialect - Tromsø.csv") |> 
  janitor::clean_names()

tromso_scores <- tromso |> 
  select("participant_id", "nord_score_0_7", "pic_score_0_4", "fritid_score_0_5",
         "frokost_score_0_5", "daglige_rutine_score_0_5", 
         wordlist) |> 
  mutate_if(is.numeric, scale) |> 
  mutate_if(is.numeric, as.numeric)


tromso_scores[is.na(tromso_scores)] <- 0

tromso_wide <- tromso_scores |> 
  pivot_longer(cols = -participant_id,
               names_to = "task",
               values_to = "score")


tromso_wide$task <- str_extract(tromso_wide$task, "^[a-z]+")

tromso_wide$task <- factor(tromso_wide$task, 
                           levels = c("wordlist", "nord", "pic", "fritid", "frokost", "daglige"))


ggplot(tromso_wide, aes(task, score, group = participant_id)) +
  geom_line() +
  facet_wrap(~participant_id)

ggsave("plots/tromso_6.pdf", height = 7, width = 7, device = cairo_pdf)

tromso_scores2 <- tromso_scores

tromso_scores2 <- tromso_scores2 |> 
  rowwise() |> 
  mutate(spoken = mean(fritid_score_0_5, frokost_score_0_5, daglige_rutine_score_0_5)) |> 
  select(-fritid_score_0_5, -frokost_score_0_5, -daglige_rutine_score_0_5)

tromso_wide2 <- tromso_scores2 |> 
  pivot_longer(cols = -participant_id,
               names_to = "task",
               values_to = "score")


tromso_wide2$task <- str_extract(tromso_wide2$task, "^[a-z]+")

tromso_wide2$task <- factor(tromso_wide2$task, 
                            levels = c("wordlist", "nord", "pic", "spoken"))



ggplot(tromso_wide2, aes(task, score, group = participant_id)) +
  geom_line() +
  facet_wrap(~participant_id)

ggsave("plots/tromso_4.pdf", width = 7, height = 7, device = cairo_pdf)

