
rating_data = read.csv(here("data", "rating_data.csv"), header = FALSE) %>% 
  t() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1) %>% 
  rename(kind = 1, spec = 2) %>% 
  pivot_longer(c(3:49), names_to = "prolific_id", values_to = "rating") 



ord_mod <- brm(as.integer(rating) ~ kind + (1 | prolific_id) + (1 | spec),
                                data = rating_data,
                                family = cumulative(),
                                cores = 4
)

ord_mod %>% 
  write_rds(here("models", "ord_mod.rds"))


cond_df_ord = conditional_effects(ord_mod, categorical = TRUE)[["kind:cats__"]]

cond_df_ord %>% 
  filter(kind == "abstract concept" | 
           kind == "artifact" | 
           kind == "natural kind") %>% 
  ggplot(aes(x = effect1__, y = estimate__, fill = effect2__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), shape = 21, 
                  position = position_dodge(width = .4)) +
  theme_minimal() + ylab("Probability") + xlab("Condition") + 
  labs(fill = "Rating") + 
  theme(axis.text.x = element_text(angle = 90)) +
  ggsave(here("plots", "ord_model.png"))

cond_df_ord %>% 
  filter(kind == "abstract concept" | 
           kind == "artifact" | 
           kind == "natural kind") %>% 
  ggplot(aes(y = effect1__, x = estimate__, fill = effect2__)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), shape = 21, 
                  position = position_dodge(width = .4)) +
  theme_minimal() + xlab("Probability") + ylab("Condition") + 
  labs(fill = "Rating") + 
  theme(axis.text.x = element_text(angle = 90))  +
  ggsave(here("plots", "ord_model_2.png"))



rating_data %>% 
  filter(kind == "abstract concept" | 
           kind == "artifact" | 
           kind == "natural kind") %>% 
  ggplot(aes(rating, fill = rating)) +
  geom_histogram(stat = "count", color = "black") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~kind) +
  ggsave(here("plots", "ord_hist.png"))
  

### make the plots a table with estimates and put in appendix

desc_table_ord = cond_df_ord %>% 
  select(estimate__, 
         lower__, upper__, effect1__, effect2__) %>% 
  rename(Kind = "effect1__",
         Rating = "effect2__") %>% 
  mutate(Probability = paste0(round(estimate__, digits = 2), " [",
                              round(lower__, digits = 2), " - ",
                              round(upper__, digits = 2), "]")) %>% 
  select(Kind, Rating, Probability) %>% 
  pivot_wider(names_from = "Rating", values_from = "Probability") %>% 
  write.csv(here("data", "tidy", "ord_table.csv"))

