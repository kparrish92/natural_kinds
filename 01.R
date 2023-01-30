library(here)
library(tidyverse)
library(janitor)
library(brms)
library(bayestestR)

d1 = read.csv(here("data", "data_1.csv"), header = FALSE) %>% 
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  pivot_longer(c(3:53), names_to = "prolific_id", values_to = "response") %>% 
  rename(kind = 1, spec = 2) %>% 
  mutate(response_corrected = case_when(
    response == "Is not" ~ "is_not",
    response == "Is" ~ "is",
    response == "Both" ~ "both",
    response == "Both" ~ "Both "
  )) %>% 
  filter(!is.na(response_corrected))

d1 %>% 
  filter(kind == "abstract concept" | 
           kind == "artifact" | 
           kind == "natural kind") %>%
  ggplot(aes(response_corrected, fill = response_corrected)) +
  geom_histogram(stat = "count", color = "black") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~kind) +
  ggsave(here("plots", "multinom_plot_hist.png"))


d1 %>% 
  write.csv(here("data", "multinom_data_tidy.csv"))

b2 <- brm(response_corrected ~ kind + (1 | prolific_id) + (1 | spec), 
           data=d1,
           family="categorical")


b2 %>% 
  write_rds(here("models", "multinom_mod.rds"))

cond_df = conditional_effects(b2, categorical = TRUE)[["kind:cats__"]]

conditional_effects(b2, 
                    method = "posterior_linpred",
                    categorical = TRUE)[["kind:cats__"]]


cond_df %>% 
  filter(kind == "abstract concept" | 
           kind == "artifact" | 
           kind == "natural kind") %>% 
ggplot(aes(x = effect1__, y = estimate__, fill = effect2__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), shape = 21, 
                  position = position_dodge(width = .15)) +
  theme_minimal() + ylab("Probability") + xlab("Condition") + 
  labs(fill = "Answer") + 
  theme(axis.text.x = element_text(angle = 90)) +
  ggsave(here("plots", "multinom_plot.png"))


### make the plots a table with estimates and put in appendix

desc_table = cond_df %>% 
  select(estimate__, 
         lower__, upper__, effect1__, effect2__) %>% 
  rename(Kind = "effect1__",
         Choice = "effect2__") %>% 
  mutate(Probability = paste0(round(estimate__, digits = 2), " [",
                              round(lower__, digits = 2), " - ",
                              round(upper__, digits = 2), "]")) %>% 
  select(Kind, Choice, Probability) %>% 
  pivot_wider(names_from = "Choice", values_from = "Probability") %>% 
  write.csv(here("data", "tidy", "multinom_table.csv"))
  





