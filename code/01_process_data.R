
################PREP DATA###########################


#2018 GA voter roll
vr18 <- fread("M:/Democracy & Justice/democracy/voter_file_data/georgia/Georgia_Daily_VoterBase_2018.txt", quote = "")

#2022 GA Voter roll
vr22 <- readRDS("M:/Democracy & Justice/democracy/voter_file_data/georgia/Georgia_Daily_VoterBase_10312022.rds")


#most recent SoS GA voter data from the 22 general election is data 12-29-2022
#prep data
ga_data_prep <- function(year) {
  
  abs <- fread(paste0("M:/Democracy & Justice/democracy/ga_sb202_general/raw_data/STATEWIDE_", year, ".csv")) %>% 
    clean_names() %>% 
    mutate(application_date = as.Date(application_date, "%m/%d/%Y"),
           ballot_return_date = as.Date(ballot_return_date, "%m/%d/%Y")) %>% 
    mutate(a = ballot_status == "A") %>% 
    group_by(voter_registration_number) %>% 
    arrange(desc(a)) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    rename(voter_id = voter_registration_number,
           county_name = county) %>% 
    select(-c(party, a))
  
  
  ed <- read_fwf(paste0("M:/Democracy & Justice/democracy/ga_sb202_general/raw_data/election_day_", year, ".txt"),
                 col_positions = fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1)))
  colnames(ed) <- c("county", "voter_id", "election_date",
                    "election_type", "party", "absentee", "provisional", "supplemental")
  
  
  ed <- ed %>%  
    mutate(voter_id = as.integer(voter_id)) %>% 
    left_join(abs, by = "voter_id") %>% 
    mutate(vote_type = case_when(absentee == "N" ~ "Poll Vote",
                                 absentee == "Y" & ballot_style == "MAILED" ~ "Mailed",
                                 absentee == "Y" & ballot_style == "IN PERSON" ~ "Early In Person",
                                 TRUE ~ "Other Absentee")) %>%
    mutate(year = paste0("20", year))
  
  if (year == "18") {
    ga <- left_join(ed, vr18, by = c("voter_id" = "REGISTRATION_NUMBER")) 
  }
  
  if (year == "22") {
    ga <- left_join(ed, vr22, by = c("voter_id" = "REGISTRATION_NUMBER")) 
  }
  
  assign(paste0("ga", year), ga, envir=parent.frame())
  
  #for race imputation
  race_sub <- ga %>% 
    rename(county_number = county) %>% 
    filter(is.na(RACE_DESC) | RACE_DESC %in% c("Unknown")) #319,802 missing race values in 2022 and 277,805 in 2018
  print(paste0("There are ", nrow(race_sub), " missing race values that need to be imputed."))
  
  assign(paste0("race_sub", year), race_sub, envir=parent.frame())
  
}

ga_data_prep(year = "18") #write the last 2 numbers of the year (e.g., 18, 20)
ga_data_prep(year = "22")



###############WRU PREDICTIONS#############################


#census block information
blocks <- blocks(state = "GA", class = "sp", year = 2020)

#census block info
wru_cens <- readRDS("M:/Democracy & Justice/democracy/regular_data/wru_2023_update/census_block_01_06_2023_GA.rds")



#wru data for voters without self-reported race or "unknown"
wru_prep <- function(year) {
  
  if (year == "18") {
    db18 <- dbConnect(SQLite(), "M:/Democracy & Justice/democracy/voter_file_data/sql_dbs/national_file_post18.db")
    l2 <- dbGetQuery(db18, "SELECT * from GA")
    dbDisconnect(db18)
    l2 <- left_join(race_sub18, l2, by = c("voter_id" = "Voters_StateVoterID")) %>% 
      rename(county2 = County)
  }
  
  if (year == "22") {
    l2 <- fread("M:/Democracy & Justice/democracy/ga_sb202_general/raw_data/12_23_22_GA_L2.csv")
    l2 <- left_join(race_sub22, l2, by = c("voter_id" = "Voters_StateVoterID"))
  }
  
  
  l2 <- l2 %>% 
    mutate(GEOID = paste0("13", str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                          str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                          Residence_Addresses_CensusBlockGroup)) %>% 
    rename(surname = Voters_LastName,
           county = Voters_FIPS,
           tract = Residence_Addresses_CensusTract) %>%
    mutate(county = str_pad(county, width = 3, side = "left", pad = "0"),
           tract = str_pad(tract, width = 6, side = "left", pad = "0")) %>% 
    mutate(state = "GA") %>% 
    filter(!is.na(Residence_Addresses_Longitude),
           !is.na(Residence_Addresses_Latitude))
  
  pings  <- SpatialPoints(l2[,c('Residence_Addresses_Longitude', 
                                'Residence_Addresses_Latitude')],proj4string = blocks@proj4string)
  l2$block <- over(pings, blocks)$GEOID
  
  wru2 <- filter(l2, block %in% paste0("13", wru_cens$GA$block$county, wru_cens$GA$block$tract, wru_cens$GA$block$block)) 
  wru2$block_full  <- wru2$block
  wru2$block <- str_sub(wru2$block_full, start= -4)
  wru2$tract <- str_sub(wru2$block_full, 6, 11)
  wru2$county <- str_sub(wru2$block_full, 3, 5)
  
  wru2 <- predict_race(voter.file = wru2, census.geo = "block", census.data = wru_cens, year = "2020", surname.year = "2020", 
                     model = "fBISG", census.key = "4e9d153c1ffead503d41620f242a20e96bc14b66") #312,927 predictions imputed in 22
  #16460 (6.5%) individual names not matched in 2018
  
  wru2$max_race <- colnames(wru2[,c("pred.whi","pred.bla", "pred.his", "pred.asi", "pred.oth")])[max.col(wru2[,c("pred.whi","pred.bla", "pred.his", "pred.asi", "pred.oth")], 
                                                                                                         ties.method = "first")]
  wru2 <- wru2 %>% 
    select(voter_id, pred.whi, pred.bla, pred.his, pred.asi, pred.oth, max_race)
  
  print(paste0("There are ", nrow(wru2), " race values that have been imputed in 20", year))
  assign(paste0("wru", year), wru2, envir=parent.frame())
  
}


wru_prep(year = "18") #write the last 2 numbers of the year (e.g., 18, 20)
wru_prep(year = "22")


ga18 <- left_join(ga18, wru18, by = "voter_id")
#saveRDS(ga18, "M:/Democracy & Justice/democracy/ga_sb202_general/temp/ga18_wru.rds")

ga22 <- left_join(ga22, wru22, by = "voter_id")
saveRDS(ga22, "M:/Democracy & Justice/democracy/ga_sb202_general/temp/ga22_wru.rds")








#ignore below



ed22 <- read_fwf("M:/Democracy & Justice/democracy/ga_sb202_general/raw_data/12_29_2022_gen.txt",
                 col_positions = fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1)))
colnames(ed22) <- c("county", "voter_id", "election_date", "election_type", "party", "absentee",
                    "provisional", "supplemental")


ed22 <- ed22 %>% 
  mutate(voter_id = as.integer(voter_id)) %>% 
  left_join(ga, by = c("voter_id" = "REGISTRATION_NUMBER"))



#race match
race_sub22 <- ed22 %>% 
  filter(is.na(RACE_DESC) | RACE_DESC %in% c("Unknown")) #319,802 missing race values

#L2
l2_12.29 <- fread("M:/Democracy & Justice/democracy/ga_sb202_general/raw_data/12_23_22_GA_L2.csv")
l2_12.29 <- left_join(race_sub22, l2_12.29, by = c("voter_id" = "Voters_StateVoterID")) %>% 
  mutate(GEOID = paste0("13", str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                        str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                        Residence_Addresses_CensusBlockGroup)) %>% 
  rename(county_old = county,
         surname = Voters_LastName,
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
wru_cens <- readRDS("M:/Democracy & Justice/democracy/regular_data/wru_2023_update/census_block_01_06_2023_GA.rds")

k2 <- filter(l2_12.29, block %in% paste0("13", wru_cens$GA$block$county, wru_cens$GA$block$tract, wru_cens$GA$block$block)) 
k2$block_full  <- k2$block
k2$block <- str_sub(k2$block_full, start= -4)
k2$tract <- str_sub(k2$block_full, 6, 11)
k2$county <- str_sub(k2$block_full, 3, 5)

k2 <- predict_race(voter.file = k2, census.geo = "block", census.data = wru_cens, year = "2020", surname.year = "2020", 
                   model = "fBISG", census.key = "4e9d153c1ffead503d41620f242a20e96bc14b66") #312,927 predictions imputed


k2$max_race <- colnames(k2[,c("pred.whi","pred.bla", "pred.his", "pred.asi", "pred.oth")])[max.col(k2[,c("pred.whi","pred.bla", "pred.his", "pred.asi", "pred.oth")], ties.method = "first")]
k2 <- k2 %>% 
  select(vuid, pred.whi, pred.bla, pred.his, pred.asi, pred.oth)

#ed22$race_wru <- k2$race[match(ed22$voter_id, k2$vuid)]

ed22 <- left_join(ed22, k2, by= c("voter_id" = "vuid"))

#saveRDS(ed22, "M:/Democracy & Justice/democracy/ga_sb202_general/temp/ed22_wru.rds")



wru_k2 <- k2 %>% 
  summarize(across(starts_with("pred"), sum, na.rm = T)) %>% 
  pivot_longer(starts_with("pred"), names_to = "race",
               values_to = "votes") %>% 
  mutate(race = case_when(race == "pred.asi" ~ "Asian",
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




####2018####

#2018 voter file
ga18 <- fread("M:/Democracy & Justice/democracy/voter_file_data/georgia/Georgia_Daily_VoterBase_2018.txt", quote = "")


abs18 <- fread("M:/Democracy & Justice/democracy/ga_sb202_general/raw_data/STATEWIDE_18.csv") %>% 
  clean_names() %>% 
  mutate(application_date = as.Date(application_date, "%m/%d/%Y"),
         ballot_return_date = as.Date(ballot_return_date, "%m/%d/%Y")) %>% 
  mutate(a = ballot_status == "A") %>% 
  group_by(voter_registration_number) %>% 
  arrange(desc(a)) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  rename(voter_id = voter_registration_number,
         county_name = county) %>% 
  select(-c(party, a))


ed18 <- read_fwf("M:/Democracy & Justice/democracy/ga_sb202_general/raw_data/election_day_18.txt",
                 col_positions = fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1)))
colnames(ed18) <- c("county", "voter_id", "election_date",
                    "election_type", "party", "absentee", "provisional", "supplemental")


ed18 <- ed18 %>%  
  mutate(voter_id = as.integer(voter_id)) %>% 
  left_join(abs18, by = "voter_id") %>% 
  mutate(vote_type = case_when(absentee == "N" ~ "Poll Vote",
                               absentee == "Y" & ballot_style == "MAILED" ~ "Mailed",
                               absentee == "Y" & ballot_style == "IN PERSON" ~ "Early In Person",
                               TRUE ~ "Other Absentee")) %>%
  mutate(year = 2018)


ga18 <- left_join(ed18, ga18, by = c("voter_id" = "REGISTRATION_NUMBER")) 


  

