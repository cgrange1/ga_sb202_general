
################PREP DATA###########################

####ALABAMA####
#2018 AL voter roll
al18_ <- fread("M:/Democracy & Justice/democracy/voter_file_data/national/post_2018/voter-files-by-state/VM2--AL--2019-01-27/VM2--AL--2019-01-27-DEMOGRAPHIC.tab", sep = "\t",
      select = c("LALVOTERID", "Voters_Active",
                 "Voters_StateVoterID", "Residence_Addresses_State",
                 "Residence_Addresses_CensusTract",
                 "Residence_Addresses_CensusBlockGroup",
                 "Residence_Addresses_Latitude",
                 "Residence_Addresses_Longitude", "Precinct",
                 "Voters_Gender", "Voters_Age", "Parties_Description",
                 "EthnicGroups_EthnicGroup1Desc",
                 "Voters_OfficialRegDate", "US_Congressional_District",
                 "Voters_FIPS", "Voters_LastName",
                 "CommercialData_EstimatedHHIncome",
                 "CommercialData_Education"))

al18 <- fread("M:/Democracy & Justice/democracy/voter_file_data/national/post_2018/voter-files-by-state/VM2--AL--2019-01-27/VM2--AL--2019-01-27-VOTEHISTORY.tab", sep = "\t",
              select = c("LALVOTERID", "BallotType_General_2018_11_06", "General_2018_11_06")) %>% 
                left_join(al18_, by = "LALVOTERID")
rm(al18_)
              


#2022 AL voter roll
al22 <- fread("M:/Democracy & Justice/democracy/voter_file_data/national/post_2022/1-20-23_AL.csv")


####FLORIDA####
#2018 FL voter roll
fl18_ <- fread("M:/Democracy & Justice/democracy/voter_file_data/national/post_2018/voter-files-by-state/VM2--FL--2019-02-08/VM2--FL--2019-02-08-DEMOGRAPHIC.tab", sep = "\t",
              select = c("LALVOTERID", "Voters_Active",
                         "Voters_StateVoterID", "Residence_Addresses_State",
                         "Residence_Addresses_CensusTract",
                         "Residence_Addresses_CensusBlockGroup",
                         "Residence_Addresses_Latitude",
                         "Residence_Addresses_Longitude", "Precinct",
                         "Voters_Gender", "Voters_Age", "Parties_Description",
                         "EthnicGroups_EthnicGroup1Desc",
                         "Voters_OfficialRegDate", "US_Congressional_District",
                         "Voters_FIPS", "Voters_LastName",
                         "CommercialData_EstimatedHHIncome",
                         "CommercialData_Education"))

fl18 <- fread("M:/Democracy & Justice/democracy/voter_file_data/national/post_2018/voter-files-by-state/VM2--FL--2019-02-08/VM2--FL--2019-02-08-VOTEHISTORY.tab", sep = "\t",
              select = c("LALVOTERID", "BallotType_General_2018_11_06","General_2018_11_06")) %>% 
                left_join(fl18_, by = "LALVOTERID")
rm(fl18_)

              
#2022 FL voter roll
fl22 <- readRDS("M:/Democracy & Justice/democracy/voter_file_data/florida/flvotehistory2022.rds")
fl22_l2 <- fread("M:/Democracy & Justice/democracy/voter_file_data/national/pre_2022/10-10-22_FL.csv")

fl22 <- full_join(fl22_l2, fl22, by = c("Voters_StateVoterID" = "VoterID"))



####TENNESSEE####
#2018 TN voter roll
tn18_ <- fread("M:/Democracy & Justice/democracy/voter_file_data/national/post_2018/voter-files-by-state/VM2--TN--2019-01-30/VM2--TN--2019-01-30-DEMOGRAPHIC.tab", sep = "\t",
               select = c("LALVOTERID", "Voters_Active",
                          "Voters_StateVoterID", "Residence_Addresses_State",
                          "Residence_Addresses_CensusTract",
                          "Residence_Addresses_CensusBlockGroup",
                          "Residence_Addresses_Latitude",
                          "Residence_Addresses_Longitude", "Precinct",
                          "Voters_Gender", "Voters_Age", "Parties_Description",
                          "EthnicGroups_EthnicGroup1Desc",
                          "Voters_OfficialRegDate", "US_Congressional_District",
                          "Voters_FIPS", "Voters_LastName",
                          "CommercialData_EstimatedHHIncome",
                          "CommercialData_Education"))

tn18 <- fread("M:/Democracy & Justice/democracy/voter_file_data/national/post_2018/voter-files-by-state/VM2--TN--2019-01-30/VM2--TN--2019-01-30-VOTEHISTORY.tab", sep = "\t",
              select = c("LALVOTERID", "BallotType_General_2018_11_06", "General_2018_11_06")) %>% 
  left_join(tn18_, by = "LALVOTERID")
rm(tn18_)


#2022 TN voter roll
tn22 <- fread("M:/Democracy & Justice/democracy/voter_file_data/national/post_2022/2-04-23_TN.csv")






###############WRU - fBISG PREDICTIONS#############################


####census block information####

cen_prep_state <- function(state_2) {
  
  #df_block <- tigris::blocks(state = state_2, class = "sp", year = 2020)
  #assign(paste0("blocks_", tolower(state_2)), df_block, envir=parent.frame())
  
  df_cens <- readRDS(paste0("M:/Democracy & Justice/democracy/regular_data/wru_2023_update/census_block_01_06_2023_", state_2, ".rds"))
  assign(paste0("wru_cens_", tolower(state_2)), df_cens, envir=parent.frame())
  
}

cen_prep_state(state_2 = "AL")
cen_prep_state(state_2 = "FL")
cen_prep_state(state_2 = "TN")



####wru data for voters without self-reported race or "unknown"####

wru_prep_state <- function(state_2, year) {
  
  start_time <- Sys.time()

  df <- get(paste0(tolower(state_2), year))
  
  df2 <- df %>% 
    filter(EthnicGroups_EthnicGroup1Desc == "") %>% 
    mutate(GEOID = paste0("13", str_pad(Voters_FIPS, width = 3, side = "left", pad = "0"),
                          str_pad(Residence_Addresses_CensusTract, width = 6, side = "left", pad = "0"),
                          Residence_Addresses_CensusBlockGroup)) %>% 
    rename(surname = Voters_LastName,
           county = Voters_FIPS,
           tract = Residence_Addresses_CensusTract,
           state = Residence_Addresses_State) %>%
    mutate(county = str_pad(county, width = 3, side = "left", pad = "0"),
           tract = str_pad(tract, width = 6, side = "left", pad = "0")) %>% 
    filter(!is.na(Residence_Addresses_Longitude),
           !is.na(Residence_Addresses_Latitude))
  
  if (state_2 == "AL") {
  pings  <- SpatialPoints(df2[,c('Residence_Addresses_Longitude', 
                                   'Residence_Addresses_Latitude')],proj4string = blocks_al@proj4string)
    
  df2$block <- over(pings, blocks_al)$GEOID
  
  wru2 <- filter(df2, block %in% paste0("01", wru_cens_al$AL$block$county, wru_cens_al$AL$block$tract, wru_cens_al$AL$block$block)) 
  
  cens <- wru_cens_al
  }
  
  if (state_2 == "FL") {
    pings  <- SpatialPoints(df2[,c('Residence_Addresses_Longitude', 
                                   'Residence_Addresses_Latitude')],proj4string = blocks_fl@proj4string)
    
    df2$block <- over(pings, blocks_fl)$GEOID
    
    wru2 <- filter(df2, block %in% paste0("12", wru_cens_fl$FL$block$county, wru_cens_fl$FL$block$tract, wru_cens_fl$FL$block$block)) 
    
    cens <- wru_cens_fl
  }
  
  wru2$block_full  <- wru2$block
  wru2$block <- str_sub(wru2$block_full, start= -4)
  wru2$tract <- str_sub(wru2$block_full, 6, 11)
  wru2$county <- str_sub(wru2$block_full, 3, 5)
  
  wru2 <- predict_race(voter.file = wru2, census.geo = "block", census.data = cens, year = "2020", surname.year = "2020", 
                       model = "fBISG", census.key = "4e9d153c1ffead503d41620f242a20e96bc14b66")
  #36,380 (29.3%) individuals' last names were not matched AL18; 43,123 (28%) individuals' last names were not matched AL22
  
  wru2$max_race <- colnames(wru2[,c("pred.whi","pred.bla", "pred.his", "pred.asi", "pred.oth")])[max.col(wru2[,c("pred.whi","pred.bla", "pred.his", "pred.asi", "pred.oth")], 
                                                                                                         ties.method = "first")]
  wru2 <- wru2 %>% 
    select(LALVOTERID, pred.whi, pred.bla, pred.his, pred.asi, pred.oth, max_race)
  
  df <- left_join(df, wru2, by = "LALVOTERID")
  assign(paste0(tolower(state_2), year), df, envir=parent.frame())
  
  print(paste0("There are ", nrow(wru2), " race values that have been imputed in 20", year))
  assign(paste0("wru", "_", tolower(state_2), year), wru2, envir=parent.frame())
  
  saveRDS(df, paste0("M:/Democracy & Justice/democracy/ga_sb202_general/temp/", tolower(state_2), year, "_wru", ".rds"))
  
  end_time <- Sys.time()
  print(end_time - start_time)
  
}

wru_prep_state(state_2 = "AL", year = "18")
wru_prep_state(state_2 = "AL", year = "22")
wru_prep_state(state_2 = "FL", year = "18")
wru_prep_state(state_2 = "FL", year = "22")
wru_prep_state(state_2 = "TN", year = "18")
wru_prep_state(state_2 = "TN", year = "22")





