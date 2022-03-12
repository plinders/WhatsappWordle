library(tidyverse)
library(lubridate)
library(rwhatsapp)


chat <- rwa_read(zipfile)
chat <- chat %>% filter(!is.na(author))
chat <- chat %>% select(time, author, text, source)

df <- chat %>% filter(str_detect(text, "Wordle \\d"))
df <- df %>% mutate(WordleGame = str_extract(text, "(?!Wordle )(\\d+)[^/\\ ]"),
                    Guess = str_extract(text, "(X|\\d)\\/\\d"))

df <- df %>% mutate(Score = case_when(
        Guess == "1/6" ~ 1,
        Guess == "2/6" ~ 2,
        Guess == "3/6" ~ 3,
        Guess == "4/6" ~ 4,
        Guess == "5/6" ~ 5,
        Guess == "6/6" ~ 6,
        Guess == "X/6" ~ NA_real_
))

options(pillar.sigfig=9)
df %>% replace(is.na(.), 10) %>% group_by(author) %>% summarise(avg = mean(Score, na.rm = TRUE)) %>% arrange((avg))

df$WordleGame <- factor(df$WordleGame)

ggplot(df, aes(x = author, y = Score, color = author, fill = author)) +
  geom_jitter()

ggplot(df, aes(x = time, y = (Score), color = author, fill = author)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_reverse() +
  theme_minimal()

df %>% group_by(author) %>% summarise(n = n(), fail = sum(is.na(Score))) %>% arrange(fail) %>% mutate(Percent_win = (1-fail / n) * 100)

ggplot(df, aes(x = Score, color = author, fill = author)) + 
  geom_density(alpha = 0.3) +
  facet_wrap(~author) +
  theme_minimal()

