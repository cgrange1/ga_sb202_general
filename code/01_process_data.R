

#2022 GA Voter File
ga <- readRDS("M:/Democracy & Justice/democracy/voter_file_data/georgia/Georgia_Daily_VoterBase_10312022.rds")



ed22 <- read_fwf("M:/Democracy & Justice/democracy/ga_sb202_general/raw_data/12_29_2022_gen.txt",
                 col_positions = fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1)))
colnames(ed22) <- c("county", "voter_id", "election_date", "election_type", "party", "absentee",
                    "provisional", "supplemental")


ed22 <- ed22 %>% 
  mutate(voter_id = as.integer(voter_id)) %>% 
  left_join(ga, by = c("voter_id" = "REGISTRATION_NUMBER"))



#race match
race_sub22 <- ed22 %>% 
  filter(is.na(RACE_DESC) | RACE_DESC %in% c("Unknown")) 

#L2
l2_12.29 <- fread("M:/Democracy & Justice/democracy/ga_sb202_general/raw_data/12_23_22_GA_L2.csv")
l2_12.29 <- left_join(race_sub22, l2_12.29, by = c("voter_id" = "Voters_StateVoterID")) %>% 
  select(-c(County, county)) %>% 
  mutate(GEOID = paste0("13", str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                        str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                        Residence_Addresses_CensusBlockGroup)) %>% 
  rename(surname = Voters_LastName,
         county = Voters_FIPS,
         tract = Residence_Addresses_CensusTract,
         vuid = voter_id) %>%
  mutate(county = str_pad(county, width = 3, side = "left", pad = "0"),
         tract = str_pad(tract, width = 6, side = "left", pad = "0")) %>% 
  mutate(state = "GA") 


blocks <- blocks(state = "GA", class = "sp", year = 2020)

l2_12.29 <- l2_12.29 %>% 
  filter(!is.na(Residence_Addresses_Longitude),
         !is.na(Residence_Addresses_Latitude))
pings  <- SpatialPoints(l2_12.29[,c('Residence_Addresses_Longitude',
                                    'Residence_Addresses_Latitude')],proj4string = blocks@proj4string)
l2_12.29$block <- over(pings, blocks)$GEOID

#census block info
wru_cens <- readRDS("M:/Democracy & Justice/democracy/regular_data/wrucensus_block_07_13_2022_GA.rds")

k2 <- filter(l2_12.29, block %in% paste0("13", wru_cens$GA$block$county, wru_cens$GA$block$tract, wru_cens$GA$block$block)) 
k2$block_full  <- k2$block
k2$block <- str_sub(k2$block_full, start= -4)
k2$tract <- str_sub(k2$block_full, 6, 11)
k2$county <- str_sub(k2$block_full, 3, 5)

k2 <- predict_race(voter.file = k2, census.geo = "block", census.data = wru_cens, year = "2020", surname.year = "2020", 
                   model = "fBISG", census.key = "4e9d153c1ffead503d41620f242a20e96bc14b66")


wru_k2 <- k2 %>% 
  summarize(across(starts_with("pred"), sum, na.rm = T)) %>% 
  pivot_longer(starts_with("pred"), names_to = "race",
               values_to = "votes") %>% 
  mutate(race =
           case_when(race == "pred.asi" ~ "Asian",
                     race == "pred.whi" ~ "White",
                     race == "pred.his" ~ "Latino",
                     race == "pred.bla" ~ "Black",
                     race == "pred.oth" ~ "Other"))


ga22 <- ed22 %>% 
  filter(!is.na(RACE_DESC),
         RACE_DESC != "Unknown") %>% 
  mutate(race = RACE_DESC,
         race = case_when(race == "Hispanic" ~ "Latino",
                          race == "White not of Hispanic Origin" ~ "White",
                          race == "Black not of Hispanic Origin" ~ "Black",
                          race == "Asian or Pacific Islander" ~ "Asian",
                          race %in% c("Other", "American Indian or Alaskan Native") ~ "Other")) %>% 
  group_by(race) %>% 
  summarize(votes = n())


ga22 <- left_join(wru_k2, ga22, by = "race") %>% 
  mutate(votes = votes.x + votes.y) %>% 
  select(c(race, votes))

tot_poc <- sum(filter(ga22, race %in% c("Black", "Latino", "Asian", "Other"))$votes)

ga22[nrow(ga22) + 1,] = list("poc", tot_poc)



