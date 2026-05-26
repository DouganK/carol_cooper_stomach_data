library(dplyr)
library(odbc)
library(readxl)
library(tidyr)
library(lubridate)
library(stringr)
library(openxlsx)

#upload datafile to be submitted to db
#data set that has volumes rather than weights for prey items:
df_vol<-read_excel(
  "C:/Users/dougank/Documents/03_data/04_CC_data_entry/carol_cooper_archived_stomach_data_2025-01-14.xlsx", sheet=1)

#################clean up data##################
#clean up first:
#ensure trip IDs match those in db;
df_vol <- df_vol %>%
  mutate(
    `CRUISE NUMBER` = recode(
      `CRUISE NUMBER`,
      "2017-42"   = "IPES2017-42",
      "BCSI201893" = "BCSI-201893",
      "COS201731"  = "BCSI-201731",
      "COS201732"  = "BCSI-201732",
      "COS201733"  = "BCSI-201733",
      "COS201734"  = "BCSI-201734"
    )
  )

#pad station names that lost their leading zero
df_vol <- df_vol %>%
  mutate(
    STATION = as.character(STATION),
    STATION = if_else(
      nchar(STATION) == 2,
      paste0("0", STATION),
      STATION
    )
  )

#where prey is empty M002, digestion station should be 10
df_vol <- df_vol %>%
  mutate(
    `1-D` = if_else(`1-PREY` == "M002", 10L, `1-D`)
  )

#create UNIQUE_CATCH & UNIQUE_FISH codes:
#UNIQUE_CATCH;
df_vol <- df_vol %>% 
  mutate(
    UNIQUE_CATCH = paste(
      `CRUISE NUMBER`,
      STATION,
      `SPECIES CODE`,
      sep = "-"
    )
  )
#some of these didn't have A/J age indicators so we might have to circle back to this problem

#UNIQUE_FISH (doesn't have A or J);
df_vol <- df_vol %>% 
  mutate(
    UNIQUE_FISH = paste(
      `CRUISE NUMBER`,
      STATION,
      str_remove(`SPECIES CODE`, "[AJ]$"),
      `FISH NUMBER`,
      sep = "-"
    )
  )
##############################################################################

##########troubleshooting missing catch & specimen to not much avail############
#step 1 QA/QC:
#do these samples actually exist in catch/specimen and hopefully not stomach in the DB?
con <- dbConnect(odbc::odbc(), "Salmon_DB")
specimen_db <- tbl(con, "3_SPECIMEN")%>%
  collect()

con <- dbConnect(odbc::odbc(), "Salmon_DB")
catch_db <- tbl(con, "2_CATCH")%>%
  collect()


#filter for unique_fish not in the SPECIMEN table in BCSI DB
specimen_not_in_db <- df_vol %>%
  filter(!UNIQUE_FISH %in% specimen_db$UNIQUE_FISH)
#over half...weird amount. would expect a lot less or a lot more

#filter for unique_catch not in the catch table in BCSI DB
catch_not_in_db <- df_vol %>%
  filter(!UNIQUE_CATCH %in% catch_db$UNIQUE_CATCH)


#friggers okay lets flag what's not in the db to get a better idea what's going on;
df_vol <- df_vol %>%
  mutate(
    catch_missing = if_else(
      UNIQUE_CATCH %in% catch_not_in_db$UNIQUE_CATCH,
      "YES",
      NA_character_
    ),
    spec_missing = if_else(
      UNIQUE_FISH %in% specimen_not_in_db$UNIQUE_FISH,
      "YES",
      NA_character_
    )
  )

df_vol <- df_vol %>%
  relocate(UNIQUE_CATCH, UNIQUE_FISH, .after = catch_date) %>%
  relocate(catch_missing, spec_missing, .after = UNIQUE_FISH)

#take a look at those records that are missing age (A/J):
df_age_missing <- df_vol %>%
  filter(!str_detect(`SPECIES CODE`, "[AJ]$"))

df_age_friends <- specimen_db %>%
  filter(UNIQUE_FISH %in% df_age_missing$UNIQUE_FISH)

df_age_mysteries <- df_age_missing %>%
  anti_join(df_age_friends, by = "UNIQUE_FISH")


###Add J into df_vol for samples that appeared in df_age_friends    
df_vol <- df_vol %>%
  mutate(
    is_age_friend = UNIQUE_FISH %in% df_age_friends$UNIQUE_FISH,

    SPECIES_CODE = if_else(
      is_age_friend & !str_detect(`SPECIES CODE`, "J$"),
      paste0(`SPECIES CODE`, "J"),
      `SPECIES CODE`
    ),
    
    UNIQUE_CATCH = if_else(
      is_age_friend & !str_detect(UNIQUE_CATCH, "J$"),
      paste0(UNIQUE_CATCH, "J"),
      UNIQUE_CATCH
    )
  ) %>%
  select(-is_age_friend)

##rerun missing catch/specimen troubleshooting;
specimen_not_in_db2 <- df_vol %>%
  filter(!UNIQUE_FISH %in% specimen_db$UNIQUE_FISH)
#over half...weird amount. would expect a lot less or a lot more

#filter for unique_catch not in the catch table in BCSI DB
catch_not_in_db2 <- df_vol %>%
  filter(!UNIQUE_CATCH %in% catch_db$UNIQUE_CATCH)

#redo flags;
df_vol <- df_vol %>%
  mutate(
    catch_missing = if_else(
      UNIQUE_CATCH %in% catch_not_in_db$UNIQUE_CATCH,
      "YES",
      NA_character_
    ),
    spec_missing = if_else(
      UNIQUE_FISH %in% specimen_not_in_db$UNIQUE_FISH,
      "YES",
      NA_character_
    )
  )

df_vol <- df_vol %>%
  relocate(UNIQUE_CATCH, UNIQUE_FISH, .after = catch_date) %>%
  relocate(catch_missing, spec_missing, .after = UNIQUE_FISH)

#before we get wasting our time with manual things lets do a event cross reference
#to see if we can weed out a bit more:

#create unique event;
df_vol <- df_vol %>% 
  mutate(
    UNIQUE_EVENT = paste(
      `CRUISE NUMBER`,
      STATION,
      sep = "-"
    )
  )

#call in bridge from db for cross reference;
con <- dbConnect(odbc::odbc(), "Salmon_DB")
bridge_db <- tbl(con, "1_BRIDGE")%>%
  collect()

event_not_in_db <- df_vol %>%
  filter(!UNIQUE_EVENT %in% bridge_db$UNIQUE_EVENT)

#########################reformat table for stomach submission####################
#Stomach table
df_vol <- df_vol %>%
  rename(`1-MATURITY` = `LIFE-STAGE...11`,
         `2-MATURITY` = `LIFE-STAGE...15`,
         `3-MATURITY` = `LIFE-STAGE...19`)

#non m012 fish only:
stom_voldf<-df_vol %>%
  filter(!is.na(`1-PREY`) & `1-PREY` != "" & `1-PREY` !="M012")

#--Pivot table into long format--#   
prey_slots <- c("1", "2", "3")  

long_list <- lapply(prey_slots, function(slot) {
  stom_voldf %>%
    select(#DATA_SOURCE="CAROL",
           UNIQUE_FISH,
           paste0(slot, "-PREY"),
           paste0(slot, "-MATURITY"),
           paste0(slot, "-VOL (cc)"),
           paste0(slot, "-D"),
           SPECIMEN_STOMACH_COMMENT=COMMENTS) %>%
    rename(
      PREY_SPECIES_CODE = paste0(slot, "-PREY"),
      PREY_MATURITY = paste0(slot, "-MATURITY"),
      PREY_VOLUME = paste0(slot, "-VOL (cc)"),
      DIGESTION_STATE_CODE = paste0(slot, "-D")
    ) %>%
    filter(!is.na(PREY_SPECIES_CODE) & PREY_SPECIES_CODE != "")
})

# Combine all prey slots into one long table
stomach <- bind_rows(long_list)

stomach<-stomach%>%
  mutate(PREY_LENGTH_MM="",
         PROCESS_LOCATION="LAB",
         SPECIMEN_ID="",
         DIGESTION_CODE_ORIG="",
         DATA_SOURCE="CAROL",
         DIGESTION_STATE_DESC="",
         MATURITY_CODE="")%>%

  
  #dataclass type coerce;
  mutate(across(c(PREY_LENGTH_MM, 
                  SPECIMEN_ID, 
                  DIGESTION_CODE_ORIG,
                  MATURITY_CODE),
                as.numeric
  )
  )


#call in digestion state desc & maturity code tables:
#call in bridge from db for cross reference;
con <- dbConnect(odbc::odbc(), "Salmon_DB")
GFB_digestion_state <- tbl(con, "GFB DIGESTION_STATE")%>%
  collect()

con <- dbConnect(odbc::odbc(), "Salmon_DB")
GFB_maturity <- tbl(con, "GFB PREY_MATURITY")%>%
  collect()

# Make join columns same type
stomach$DIGESTION_STATE_CODE <- as.character(stomach$DIGESTION_STATE_CODE)
GFB_digestion_state$DIGESTION_STATE_CODE <- as.character(GFB_digestion_state$DIGESTION_STATE_CODE)

# Digestion state description
stomach <- stomach %>%
  left_join(
    GFB_digestion_state %>%
      select(DIGESTION_STATE_CODE, DIGESTION_STATE_DESC_new = DIGESTION_STATE_DESC),
    by = "DIGESTION_STATE_CODE"
  ) %>%
  mutate(
    DIGESTION_STATE_DESC = ifelse(
      is.na(DIGESTION_STATE_DESC) | DIGESTION_STATE_DESC == "",
      DIGESTION_STATE_DESC_new,
      DIGESTION_STATE_DESC
    )
  ) %>%
  select(-DIGESTION_STATE_DESC_new)

# Maturity code
stomach <- stomach %>%
  left_join(
    GFB_maturity_codes %>%
      select(PREY_MATURITY_CODE, MATURITY_CODE_new = MATURITY_CODE),
    by = c("PREY_MATURITY" = "PREY_MATURITY_CODE")
  ) %>%
  mutate(
    MATURITY_CODE = coalesce(MATURITY_CODE, MATURITY_CODE_new)
  ) %>%
  select(-MATURITY_CODE_new)


#volume column seems like it has way too many decimals;
stomach$PREY_VOLUME <- round(stomach$PREY_VOLUME, 3)


#############################################################
############################################################
#weights table:

#upload datafile to be submitted to db
#data set that has weights rather than volumes for prey items:
df_wts<-read_excel(
  "C:/Users/dougank/Documents/03_data/04_CC_data_entry/carol_cooper_archived_stomach_data_2025-01-14.xlsx", sheet=2)

#################clean up data##################
#clean up first:
#ensure trip IDs match those in db;
df_wts <- df_wts %>%
  mutate(
    `CRUISE NUMBER` = recode(
      `CRUISE NUMBER`,
      "2017-42"   = "IPES2017-42",
      "2018-73" = "IPES2018-73",
      "2019-124"  = "IPES2019-124",
    )
  )


#where prey is empty M002, digestion station should be 10
df_wts <- df_wts %>%
  mutate(
    `1-D` = if_else(`1-PREY` == "M002", 10L, `1-D`)
  )

#pad station names that lost their leading zero
df_wts <- df_wts %>%
  mutate(
    STATION = sprintf("%03d", as.numeric(STATION))
  )

#create UNIQUE_CATCH & UNIQUE_FISH codes:
#UNIQUE_CATCH;
df_wts <- df_wts %>% 
  mutate(
    UNIQUE_CATCH = paste(
      `CRUISE NUMBER`,
      STATION,
      `SPECIES CODE`,
      sep = "-"
    )
  )


#some of these didn't have A/J age indicators so we might have to circle back to this problem

#UNIQUE_FISH (doesn't have A or J);
df_wts <- df_wts %>% 
  mutate(
    UNIQUE_FISH = paste(
      `CRUISE NUMBER`,
      STATION,
      str_remove(`SPECIES CODE`, "[AJ]$"),
      `FISH NUMBER`,
      sep = "-"
    )
  )
##############################################################################

##########troubleshooting missing catch & specimen to not much avail############
#step 1 QA/QC:
#do these samples actually exist in catch/specimen and hopefully not stomach in the DB?
con <- dbConnect(odbc::odbc(), "Salmon_DB")
specimen_db <- tbl(con, "3_SPECIMEN")%>%
  collect()

con <- dbConnect(odbc::odbc(), "Salmon_DB")
catch_db <- tbl(con, "2_CATCH")%>%
  collect()


#filter for unique_fish not in the SPECIMEN table in BCSI DB
wts_specimen_not_in_db <- df_wts %>%
  filter(!UNIQUE_FISH %in% specimen_db$UNIQUE_FISH)
#over half...weird amount. would expect a lot less or a lot more

#filter for unique_catch not in the catch table in BCSI DB
wts_catch_not_in_db <- df_wts %>%
  filter(!UNIQUE_CATCH %in% catch_db$UNIQUE_CATCH)


#before we get wasting our time with manual things lets do a event cross reference
#to see if we can weed out a bit more:

#create unique event;
df_wts <- df_wts %>% 
  mutate(
    UNIQUE_EVENT = paste(
      `CRUISE NUMBER`,
      STATION,
      sep = "-"
    )
  )

#call in bridge from db for cross reference;
con <- dbConnect(odbc::odbc(), "Salmon_DB")
bridge_db <- tbl(con, "1_BRIDGE")%>%
  collect()

wts_event_not_in_db <- df_wts %>%
  filter(!UNIQUE_EVENT %in% bridge_db$UNIQUE_EVENT)


#########stomach contents QC:
con <- dbConnect(odbc::odbc(), "Salmon_DB")
GFB_species_db <- tbl(con, "GFB SPECIES")%>%
  collect()

#verify all prey species codes exist
wrong_species_codes <- anti_join(
  df_wts %>% distinct(`SPECIES CODE`),
  GFB_species_codes %>% distinct(SPECIES_CODE),
  by = c("SPECIES CODE" = "SPECIES_CODE")
)


####verify digesiton and wts fields are filled out appropriately:
# Function to check one prey slot
check_prey <- function(df, prey_col, weight_col, d_col) {
  
  df %>%
    filter(
      # prey exists and is not M002 or blank
      !is.na(.data[[prey_col]]),
      .data[[prey_col]] != "",
      .data[[prey_col]] != "M002",
      
      # invalid if weight missing
      (
        is.na(.data[[weight_col]]) |
          
          # or D missing / not allowed
          is.na(.data[[d_col]]) |
          !(.data[[d_col]] %in% c(1, 5, 11))
      )
    ) %>%
    select(
      UNIQUE_FISH,
      all_of(c(prey_col, weight_col, d_col))
    )
}

# Check each prey slot
issues_1 <- check_prey(df_wts, "1-PREY", "1-Weight (mg)", "1-D")
issues_2 <- check_prey(df_wts, "2-PREY", "2-Weight (mg)", "2-D")
issues_3 <- check_prey(df_wts, "3-PREY", "3-Weight (mg)", "3-D")

# Combine all issues
all_issues <- bind_rows(
  issues_1,
  issues_2,
  issues_3
)

all_issues

#verify all maturity codes are conformative:
# Valid maturity codes
valid_codes <- GFB_maturity_codes %>%
  distinct(PREY_MATURITY_CODE)

# Check all lifestage columns
invalid_lifestages <- bind_rows(
  
  df_wts %>%
    filter(
      !is.na(`1-LIFE-STAGE`),
      `1-LIFE-STAGE` != ""
    ) %>%
    anti_join(
      valid_codes,
      by = c("1-LIFE-STAGE" = "PREY_MATURITY_CODE")
    ) %>%
    mutate(COLUMN = "1-LIFE-STAGE"),
  
  df_wts %>%
    filter(
      !is.na(`2-LIFE-STAGE`),
      `2-LIFE-STAGE` != ""
    ) %>%
    anti_join(
      valid_codes,
      by = c("2-LIFE-STAGE" = "PREY_MATURITY_CODE")
    ) %>%
    mutate(COLUMN = "2-LIFE-STAGE"),
  
  df_wts %>%
    filter(
      !is.na(`3-LIFE-STAGE`),
      `3-LIFE-STAGE` != ""
    ) %>%
    anti_join(
      valid_codes,
      by = c("3-LIFE-STAGE" = "PREY_MATURITY_CODE")
    ) %>%
    mutate(COLUMN = "3-LIFE-STAGE")
  
)

invalid_lifestages

#########################reformat table for stomach submission####################
#Stomach table
df_wts <- df_wts %>%
  rename(`1-MATURITY` = `1-LIFE-STAGE`,
         `2-MATURITY` = `2-LIFE-STAGE`,
         `3-MATURITY` = `3-LIFE-STAGE`)

#non m012 fish only:
stomach_wts<-df_wts %>%
  filter(!is.na(`1-PREY`) & `1-PREY` != "" & `1-PREY` !="M012")

#--Pivot table into long format--#   
prey_slots <- c("1", "2", "3")  

long_list <- lapply(prey_slots, function(slot) {
  stomach_wts %>%
    select(#DATA_SOURCE="CAROL",
      UNIQUE_FISH,
      paste0(slot, "-PREY"),
      paste0(slot, "-MATURITY"),
      paste0(slot, "-Weight (mg)"),
      paste0(slot, "-D"),
      SPECIMEN_STOMACH_COMMENT=COMMENTS) %>%
    rename(
      PREY_SPECIES_CODE = paste0(slot, "-PREY"),
      PREY_MATURITY = paste0(slot, "-MATURITY"),
      PREY_WEIGHT_MG = paste0(slot, "-Weight (mg)"),
      DIGESTION_STATE_CODE = paste0(slot, "-D")
    ) %>%
    filter(!is.na(PREY_SPECIES_CODE) & PREY_SPECIES_CODE != "")
})

# Combine all prey slots into one long table
stomach_wts <- bind_rows(long_list)

stomach_wts<-stomach_wts%>%
  mutate(PREY_LENGTH_MM="",
         PROCESS_LOCATION="LAB",
         SPECIMEN_ID="",
         DIGESTION_CODE_ORIG="",
         DATA_SOURCE="CAROL",
         DIGESTION_STATE_DESC="",
         MATURITY_CODE="")%>%
  
  
  #dataclass type coerce;
  mutate(across(c(PREY_LENGTH_MM, 
                  SPECIMEN_ID, 
                  DIGESTION_CODE_ORIG,
                  MATURITY_CODE),
                as.numeric
  )
  )


#call in digestion state desc & maturity code tables:
#call in bridge from db for cross reference;
con <- dbConnect(odbc::odbc(), "Salmon_DB")
GFB_digestion_state <- tbl(con, "GFB DIGESTION_STATE")%>%
  collect()

con <- dbConnect(odbc::odbc(), "Salmon_DB")
GFB_maturity <- tbl(con, "GFB PREY_MATURITY")%>%
  collect()

# Make join columns same type
stomach_wts$DIGESTION_STATE_CODE <- as.character(stomach$DIGESTION_STATE_CODE)
GFB_digestion_state$DIGESTION_STATE_CODE <- as.character(GFB_digestion_state$DIGESTION_STATE_CODE)

# Digestion state description
stomach_wts <- stomach_wts %>%
  left_join(
    GFB_digestion_state %>%
      select(DIGESTION_STATE_CODE, DIGESTION_STATE_DESC_new = DIGESTION_STATE_DESC),
    by = "DIGESTION_STATE_CODE"
  ) %>%
  mutate(
    DIGESTION_STATE_DESC = ifelse(
      is.na(DIGESTION_STATE_DESC) | DIGESTION_STATE_DESC == "",
      DIGESTION_STATE_DESC_new,
      DIGESTION_STATE_DESC
    )
  ) %>%
  select(-DIGESTION_STATE_DESC_new)

# Maturity code
stomach_wts <- stomach_wts %>%
  left_join(
    GFB_maturity_codes %>%
      select(PREY_MATURITY_CODE, MATURITY_CODE_new = MATURITY_CODE),
    by = c("PREY_MATURITY" = "PREY_MATURITY_CODE")
  ) %>%
  mutate(
    MATURITY_CODE = coalesce(MATURITY_CODE, MATURITY_CODE_new)
  ) %>%
  select(-MATURITY_CODE_new)


#volume column seems like it has way too many decimals;
stomach_wts$PREY_VOLUME <- round(stomach_wts$PREY_VOLUME, 3)