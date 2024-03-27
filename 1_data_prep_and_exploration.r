library(tidyverse)
library(hrbrthemes)
theme_set(theme_ipsum_tw())

# oslo --------------------------------------------------------------------

oslo <- read_csv("raw/L3 dialect study (dialect index) 0–no dialect 1–dialect - Oslo (Wit.).csv") |> 
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

tromso <- read_csv("raw/L3 dialect study (dialect index) 0–no dialect 1–dialect - Tromsø.csv") |> 
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
