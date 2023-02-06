
#################GA TURNOUT ANALYSIS##################


tot <- ga22 %>% 
  filter(!is.na(pred.asi)) %>% 
  summarize(across(starts_with("pred"), sum, na.rm = T)) %>% 
  pivot_longer(starts_with("pred"), names_to = "race",
               values_to = "votes") %>% 
  mutate(race = case_when(race == "pred.asi" ~ "Asian",
                          race == "pred.whi" ~ "White",
                          race == "pred.his" ~ "Latino",
                          race == "pred.bla" ~ "Black",
                          race == "pred.oth" ~ "Other"))


tot2 <- ga22 %>% 
  filter(is.na(pred.asi)) %>% 
  mutate(race = RACE_DESC,
         race = case_when(race == "Hispanic" ~ "Latino",
                          race == "White not of Hispanic Origin" ~ "White",
                          race == "Black not of Hispanic Origin" ~ "Black",
                          race == "Asian or Pacific Islander" ~ "Asian",
                          race %in% c("Other", "American Indian or Alaskan Native") ~ "Other")) %>% 
  group_by(race) %>% 
  summarize(votes = n())


tot3 <- left_join(tot2, tot, by = "race") %>% 
  mutate(votes.y = replace_na(votes.y, 0)) %>% 
  mutate(votes = votes.x + votes.y) %>% 
  select(c(race, votes)) %>% 
  adorn_totals()

tot_poc <- sum(filter(t0t3, race %in% c("Black", "Latino", "Asian", "Other"))$votes)

tot3[nrow(ga22) + 1,] = list("poc", tot_poc)



