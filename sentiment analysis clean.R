library(tidyverse)
#devtools::install_github("Jimbilben/jimbilben")
library(jimbilben)
library(tidystats)
library(glue)
#library(ggtext)
library(readxl)
library(irr)

theme_set(theme_jimbilben(10))
set_colors()

sentim <- read_csv("data/claude and gpt codings.csv")

#### Exploring ratings ####
sentim_long <-
  sentim %>%
  pivot_longer(6:ncol(sentim),
               names_to = "prompt",
               values_to = "rating") %>%
  mutate(claude_rating = str_extract(rating, "(?<=<rating>).*?(?=</rating>)"),
         claude_rating_numeric = case_when(claude_rating == "Neutral" ~ 0,
                                           claude_rating == "Positive" ~ 1,
                                           claude_rating == "Negative" ~ -1),
         correct = case_when(claude_rating_numeric == gold_standard ~ 1,
                             is.na(gold_standard) | is.na(claude_rating_numeric) ~ as.numeric(NA),
                             TRUE ~ 0)) %>%
  mutate(dutch = case_when(str_detect(prompt, "nl_") ~ .5,
                           TRUE ~ -.5),
         has_examples = case_when(str_detect(prompt, "noexamples") ~ -.5,
                                  str_detect(prompt, "minimal") ~ -.5,
                                  TRUE ~ .5),
         less_explicit = case_when(str_detect(prompt, "lessexplicit") ~ .5,
                                   TRUE ~ -.5),
         is_minimal = case_when(str_detect(prompt, "minimal") ~ .5,
                                TRUE ~ -.5),
         is_gpt = case_when(str_detect(prompt, "gpt") ~ .5,
                            TRUE ~ -.5))

sentim_long %>%
  group_by(prompt) %>%
  summarise(mean = mean(correct, na.rm = TRUE)) %>%
  ggplot(aes(x = mean, y = fct_reorder(prompt, mean))) +
  geom_col()

sentim_long %>%
  group_by(prompt, gold_standard) %>%
  summarise(mean = mean(correct, na.rm = TRUE)) %>%
  ggplot(aes(x = mean, y = fct_reorder(prompt, mean))) +
  geom_col() +
  facet_wrap(~as.factor(gold_standard))

#### Krippendorff's alpha ####
krippendorf <- function (data, a, b, prompt = NULL, method = c("nominal", "ordinal", "interval", "ratio")) {

  method <- match.arg(method)

  data <- na.omit(data)

  levx <- (levels(as.factor(data[,a] %>% pull())))

  nval <- length(levx)

  cm <-
    table(data[,a] %>% pull(), data[,b] %>% pull())

  diag(cm) <- diag(cm) * 2

  # Summing and replacing off-diagonal elements
  for (i in 1:(nrow(cm) - 1)) {
    for (j in (i + 1):ncol(cm)) {
      sum_val <- cm[i, j] + cm[j, i]
      cm[i, j] <- sum_val
      cm[j, i] <- sum_val
    }
  }

  rmv = sum(cm)

  subjects <- nrow(data)

  raters <- ncol(data)

  data.values <- levx

  nmatchval <- rmv
  dimcm <- dim(cm)
  utcm <- as.vector(cm[upper.tri(cm)])
  diagcm <- diag(cm)
  occ <- sum(diagcm)
  nc <- apply(cm, 1, sum)
  ncnc <- sum(nc * (nc - 1))
  dv <- as.numeric(data.values)
  diff2 <- rep(0, length(utcm))
  ncnk <- rep(0, length(utcm))
  ck <- 1
  if (dimcm[2] < 2)
    value <- 1
  else {
    for (k in 2:dimcm[2]) {
      for (c in 1:(k - 1)) {
        ncnk[ck] <- nc[c] * nc[k]
        if (match(method[1], "nominal", 0))
          diff2[ck] <- 1
        if (match(method[1], "ordinal", 0)) {
          diff2[ck] <- nc[c]/2
          if (k > (c + 1))
            for (g in (c + 1):(k - 1)) diff2[ck] <- diff2[ck] +
                nc[g]
          diff2[ck] <- diff2[ck] + nc[k]/2
          diff2[ck] <- diff2[ck]^2
        }
        if (match(method[1], "interval", 0))
          diff2[ck] <- (dv[c] - dv[k])^2
        if (match(method[1], "ratio", 0))
          diff2[ck] <- (dv[c] - dv[k])^2/(dv[c] + dv[k])^2
        ck <- ck + 1
      }
    }

    value <- 1 - (nmatchval - 1) * sum(utcm * diff2)/sum(ncnk *
                                                           diff2)
  }

  output <-
    if(is.null(prompt)) {
      tibble(krippendorff_alpha = value,
             type = method,
             n_raters = raters,
             n_ratings = subjects)
    } else {
      tibble(krippendorff_alpha = value,
             prompt = prompt,
             type = method,
             n_raters = raters,
             n_ratings = subjects)
    }


  return(output)

}

standard_kripp <-
  pmap_df(.l = list(data = sentim_long %>%
                      group_by(prompt) %>%
                      group_split(),
                    prompt = unique(sentim_long$prompt) %>% sort()),
          a = "gold_standard",
          b = "claude_rating_numeric",
          method = "ordinal",
          .f = krippendorf)

#### Precision, Recall, and F1 ####
# Precision = true_pos / (true_pos + false_pos)
# Recall = true_pos / (true_pos + false_neg))
# F1 = 2 * ( (Precision * Recall) / (Precision + Recall) )

sentim_long %>%
  filter(is.na(claude_rating_numeric)) %>%
  pull(rating)

accuracy <-
  sentim_long %>%
  group_by(prompt) %>%
  summarise(accuracy = mean(correct, na.rm = TRUE))

precision <-
  sentim_long %>%
  filter(!is.na(claude_rating_numeric)) %>%
  group_by(prompt, claude_rating_numeric) %>%
  summarise(true_pos = sum(correct),
            false_pos = n() - true_pos,
            precision = true_pos / (true_pos + false_pos))

recall <-
  sentim_long %>%
  filter(!is.na(claude_rating_numeric)) %>%
  group_by(prompt, gold_standard) %>%
  summarise(true_pos = sum(correct),
            false_neg = n() - true_pos,
            recall = true_pos / (true_pos + false_neg))

performance_summary <-
  precision %>%
  select(-true_pos) %>%
  rename(outcome = claude_rating_numeric) %>%
  left_join(recall %>% rename(outcome = gold_standard),
            by = c("prompt", "outcome")) %>%
  mutate(f1 = 2 * ( (precision * recall) / (precision + recall) )) %>%
  left_join(standard_kripp,
            by = "prompt") %>%
  left_join(accuracy,
            by = "prompt") %>%
  mutate(full_prompt = prompt,
         prompt = case_match(prompt,
                             "eng_main_prompt" ~ "Main - *Claude 2*",
                             "eng_main_prompt_gpt4" ~ "Main - *ChatGPT-4*",
                             "eng_main_prompt_less_explicit_gpt4" ~ "Less explicit - *ChatGPT-4*",
                             "eng_main_prompt_lessexplicit" ~ "Less explicit - *Claude 2*",
                             "eng_prompt_minimal" ~ "Minimal - *Claude 2*",
                             "eng_prompt_noexamples" ~ "No examples - *Claude 2*",
                             "nl_main_prompt" ~ "NL Main - *Claude 2*",
                             "nl_main_prompt_lessexplicit" ~ "NL Less explicit - *Claude 2*",
                             "nl_prompt_minimal" ~ "NL Minimal - *Claude 2*",
                             "nl_prompt_noexamples" ~ "NL No examples - *Claude 2*"),
         krip = krippendorff_alpha)

performance_summary_single <-
  performance_summary %>%
  filter(outcome == 0) %>%
  pivot_longer(cols = c(krippendorff_alpha, accuracy),
               names_to = "measure",
               values_to = "value")

j_png("krip.png",
      height = 3)
performance_summary_single %>%
  mutate(measure = factor(measure,
                          levels = c("krippendorff_alpha", "accuracy"),
                          labels = c("Krippendorff's alpha", "Proportion correct"))) %>%
  ggplot(aes(x = value, y = fct_reorder(prompt, krip))) +
  coord_cartesian(xlim = c(.35, 1.15)) +
  scale_x_continuous(breaks = seq(.4, 1, .1), expand = expansion(c(0)), labels = nice_num(seq(.4, 1, .1), 1)) +
  geom_col(width = .75, fill = "#193a62", alpha = .85) +
  geom_rect(aes(xmin = 1, xmax = 1.15, ymin = -Inf, ymax = Inf), fill = "grey95", color = "grey90", linewidth = .33) +
  geom_text(aes(x = 1.075, label = nice_num(value, 2)), family = "Jost", size = 3) +
  facet_wrap(~measure) +
  theme(
    panel.border = element_rect(color = "grey90", linewidth = .33),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = .33),
    axis.title = element_blank()
  )
dev.off()

performance_summary_tri <-
  performance_summary %>%
  pivot_longer(cols = c(precision, recall, f1),
               names_to = "measure",
               values_to = "value")

j_png("f1.png",
      height = 6)
performance_summary_tri %>%
  mutate(outcome = factor(outcome,
                          levels = c(-1, 0, 1),
                          labels = c("Negative", "Neutral", "Positive")),
         measure = factor(measure,
                          levels = c("precision", "recall", "f1"),
                          labels = c("Precision", "Recall", "F1 score"))) %>%
  ggplot(aes(x = value, y = fct_reorder(prompt, krippendorff_alpha))) +
  coord_cartesian(xlim = c(.35, 1.15)) +
  scale_x_continuous(breaks = seq(.4, 1, .1), expand = expansion(c(0)), labels = c(".4", NA, ".6", NA, ".8", NA, "1")) +
  geom_col(width = .75, fill = "#193a62", alpha = .85) +
  geom_rect(aes(xmin = 1, xmax = 1.15, ymin = -Inf, ymax = Inf), fill = "grey95", color = "grey90", linewidth = .33) +
  geom_text(aes(x = 1.075, label = nice_num(value, 2)), family = "Jost", size = 2.8) +
  facet_grid(vars(outcome), vars(measure)) +
  theme(
    panel.border = element_rect(color = "grey90", linewidth = .33),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey90", linewidth = .33),
    axis.title = element_blank()
  )
dev.off()