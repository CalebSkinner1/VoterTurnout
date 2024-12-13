# Caleb Skinner
# Last Updated: Oct 15

library("tidyverse")
library("janitor")
library("usdata")
library("here")
library("data.table")

data_path <- "Project/Data"
# read in data on presidential elections
pres <- read_csv(here(data_path, "1976-2020-president.csv")) %>%
  clean_names() %>%
  rename(total_votes = totalvotes)

# acquire results from previous year's election
prev_year <- pres %>%
  select(year, state, party_simplified, candidatevotes, total_votes) %>%
  mutate(
    vote_margin = candidatevotes/total_votes) %>%
  filter(party_simplified %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  group_by(state, year) %>%
  mutate(
    rank = rank(-vote_margin)) %>%
  ungroup() %>%
  filter(rank < 3) %>%
  select(-candidatevotes, -total_votes, -rank) %>%
  pivot_wider(names_from = party_simplified, values_from = vote_margin) %>%
  mutate(
    state_prev_margin = DEMOCRAT - REPUBLICAN,
    state = str_to_lower(state),
    next_election = year + 4) %>%
  select(next_election, state, state_prev_margin)

# look at national polls before election day
nat_polls <- read_csv(here(data_path, "national_presidential_polls.csv"), skip = 1, col_select = c(2,3,4,5)) %>%
  clean_names() %>%
  mutate(
    party = case_when(
      nominee == "Kamala Harris" ~ "D",
      nominee == "Joe Biden" ~ "D",
      nominee == "Donald Trump" ~ "R",
      str_detect(nominee, "Clinton") ~ "D",
      nominee == "Barack Obama" ~ "D",
      nominee == "Mitt Romney" ~ "R",
      nominee == "John McCain" ~ "R",
      str_detect(nominee, "Bush") ~ "R",
      str_detect(nominee, "Kerry") ~ "D",
      str_detect(nominee, "Gore") ~ "D",
      str_detect(nominee, "Dole") ~ "R",
      str_detect(nominee, "Dukakis") ~ "D",
      str_detect(nominee, "Reagan") ~ "R",
      str_detect(nominee, "Mondale") ~ "D",
      str_detect(nominee, "Carter") ~ "D",
      str_detect(nominee, "Ford") ~ "R",
      str_detect(nominee, "Nixon") ~ "R",
      str_detect(nominee, "McGovern") ~ "D",
      str_detect(nominee, "Humphrey") ~ "D",
      str_detect(nominee, "Lyndon B. Johnson") ~ "D",
      str_detect(nominee, "Goldwater") ~ "R",
      str_detect(nominee, "Kennedy") ~ "D",
      str_detect(nominee, "Eisenhower") ~ "R",
      str_detect(nominee, "Adlai Stevenson") ~ "D",
      str_detect(nominee, "Truman") ~ "D",
      str_detect(nominee, "Dewey") ~ "R",
      str_detect(nominee, "Roosevelt") ~ "D",
      str_detect(nominee, "Wilkie") ~ "R",
      str_detect(nominee, "Landon") ~ "R",
      .default = "Other")) %>%
  select(-nominee) %>%
  pivot_wider(names_from = party, values_from = final_estimate) %>%
  mutate(
    nat_poll_margin = D - R) %>%
  filter(poll == "RCP"| poll == "Gallup" & year < 2007) %>%
  select(year, nat_poll_margin)

# 2024 voter turnout

turnout_2024 <- read_csv(here(data_path, "Turnout_2024G_v0.3.csv")) %>%
  clean_names() %>%
  mutate(
    year = 2024,
    state = str_to_lower(state),
    percent_non_citizen = as.numeric(str_remove(noncitizen_pct, "%")),
    total_potential_workers = vap / (.01*(100-percent_non_citizen))) %>%
  rename(total_votes = total_ballots_counted) %>%
  select(state, year, vep, total_votes, total_potential_workers) %>%
  slice(-1)

# voter turnout
turnout <- read_csv(here(data_path, "voter_turnout.csv"), skip = 1, col_names = FALSE) %>%
  slice(-1) %>%
  row_to_names(1) %>%
  clean_names() %>%
  rename(state = "na") %>%
  select(-contains("na")) %>%
  filter(state != "United States") %>%
  select(state, year, voting_eligible_population_vep, voting_age_population_vap,percent_non_citizen, prison, probation, parole, total_ineligible_felon) %>%
  mutate(
    across(everything(), ~str_remove_all(.x, ",")),
    across(everything(), ~str_remove_all(.x, "%")),
    across(year:total_ineligible_felon, ~as.numeric(.x)),
    state = str_to_lower(state),
    total_potential_workers = voting_age_population_vap / (.01*(100-percent_non_citizen))) %>%
  rename(vap = "voting_age_population_vap",
         vep = "voting_eligible_population_vep") %>%
  select(state, year, vep, total_potential_workers) %>%
  bind_rows(turnout_2024 %>% select(-total_votes))


# demographic - employment
farm <- 51227-36900
employment <- read_csv(here(data_path, "BEA - EDDs - Employment.csv")) %>%
  clean_names() %>%
  filter(linecode_description == "Total employment") %>%
  mutate(
    data = case_when(
      # estimates for missing employment data from https://live.laborstats.alaska.gov/labforce/000001/60/ces.html
      district_name == "Southeast Conference" & year == "2008" ~ 36800 + farm,
      district_name == "Southeast Conference" & year == "2009" ~ 36100 + farm,
      district_name == "Southeast Conference" & year == "2010" ~ 36400 + farm,
      district_name == "Southeast Conference" & year == "2011" ~ 36800 + farm,
      district_name == "Southeast Conference" & year == "2012" ~ 37300 + farm,
      district_name == "Southeast Conference" & year == "2013" ~ 37200 + farm,
      district_name == "Southeast Conference" & year == "2014" ~ 37000 + farm,
      district_name == "Southeast Conference" & year == "2015" ~ 37000 + farm,
      district_name == "Southeast Conference" & year == "2016" ~ 36600 + farm,
      district_name == "Southeast Conference" & year == "2017" ~ 36600 + farm,
      district_name == "Southeast Conference" & year == "2018" ~ 36600 + farm,
      district_name == "Southeast Conference" & year == "2019" ~ 37000 + farm,
      district_name == "Southeast Conference" & year == "2020" ~ 32000 + farm,
      district_name == "Southeast Conference" & year == "2021" ~ 33600 + farm,
      district_name == "Southeast Conference" & year == "2022" ~ 35500 + farm,
      district_name == "Southeast Conference" & year == "2023" ~ 33200 + farm,
      .default = data),
    state = str_to_lower(abbr2state(state))) %>%
  group_by(state, year) %>%
  summarize(total_employment = sum(data))

# demographic - income
income <- read_csv(here(data_path, "BEA - EDDs - Personal Income.csv")) %>%
  clean_names() %>%
  filter(linecode_description %in% c("Personal income","Population (persons)")) %>%
  mutate(
    data = case_when(
      # estimates for missing population and income data from By the Numbers https://www.seconference.org/southeast-alaska-by-the-numbers/
      district_name == "Southeast Conference" & year == "2008" & linecode_description == "Personal income" ~ 3242587,
      district_name == "Southeast Conference" & year == "2009" & linecode_description == "Personal income" ~ 71664*42991*.00117*.5 +3242587*.5,
      district_name == "Southeast Conference" & year == "2010" & linecode_description == "Personal income" ~ 71664*42991*.00117,
      district_name == "Southeast Conference" & year == "2011" & linecode_description == "Personal income" ~ 73526*41840*.00117,
      district_name == "Southeast Conference" & year == "2012" & linecode_description == "Personal income" ~ 74363*45618.05*.00117,
      district_name == "Southeast Conference" & year == "2013" & linecode_description == "Personal income" ~ 74382*46508.53*.00117,
      district_name == "Southeast Conference" & year == "2014" & linecode_description == "Personal income" ~ 74518*47593*.00117,
      district_name == "Southeast Conference" & year == "2015" & linecode_description == "Personal income" ~ 74395*47846*.00117,
      district_name == "Southeast Conference" & year == "2016" & linecode_description == "Personal income" ~ 73812*48673*.00117,
      district_name == "Southeast Conference" & year == "2017" & linecode_description == "Personal income" ~ 72915*48113*.00117,
      district_name == "Southeast Conference" & year == "2018" & linecode_description == "Personal income" ~ 72657*50023*.00117,
      district_name == "Southeast Conference" & year == "2019" & linecode_description == "Personal income" ~ 72373*50873*.00117,
      district_name == "Southeast Conference" & year == "2020" & linecode_description == "Personal income" ~ 71946*53635*.00117,
      district_name == "Southeast Conference" & year == "2021" & linecode_description == "Personal income" ~ 72683*56605*.00117,
      district_name == "Southeast Conference" & year == "2022" & linecode_description == "Personal income" ~ 72218*59493*.00117,
      district_name == "Southeast Conference" & year == "2023" & linecode_description == "Personal income" ~ 71077*60257*.00117,
      district_name == "Southeast Conference" & year == "2008" & linecode_description == "Population (persons)" ~ 69163,
      district_name == "Southeast Conference" & year == "2009" & linecode_description == "Population (persons)" ~ 69338,
      district_name == "Southeast Conference" & year == "2010" & linecode_description == "Population (persons)" ~ 71664,
      district_name == "Southeast Conference" & year == "2011" & linecode_description == "Population (persons)" ~ 73526,
      district_name == "Southeast Conference" & year == "2012" & linecode_description == "Population (persons)" ~ 74363,
      district_name == "Southeast Conference" & year == "2013" & linecode_description == "Population (persons)" ~ 74382,
      district_name == "Southeast Conference" & year == "2014" & linecode_description == "Population (persons)" ~ 74518,
      district_name == "Southeast Conference" & year == "2015" & linecode_description == "Population (persons)" ~ 74395,
      district_name == "Southeast Conference" & year == "2016" & linecode_description == "Population (persons)" ~ 73812,
      district_name == "Southeast Conference" & year == "2017" & linecode_description == "Population (persons)" ~ 72915,
      district_name == "Southeast Conference" & year == "2018" & linecode_description == "Population (persons)" ~ 72657,
      district_name == "Southeast Conference" & year == "2019" & linecode_description == "Population (persons)" ~ 72373,
      district_name == "Southeast Conference" & year == "2020" & linecode_description == "Population (persons)" ~ 71946,
      district_name == "Southeast Conference" & year == "2021" & linecode_description == "Population (persons)" ~ 72683,
      district_name == "Southeast Conference" & year == "2022" & linecode_description == "Population (persons)" ~ 72218,
      district_name == "Southeast Conference" & year == "2023" & linecode_description == "Population (persons)" ~ 71077,
      .default = data)) %>%
  group_by(state, year, linecode_description) %>%
  summarize(total = sum(data)) %>%
  pivot_wider(names_from = linecode_description, values_from = total) %>%
  clean_names() %>%
  mutate(
    state = str_to_lower(abbr2state(state)),
    personal_income_capita = personal_income/population_persons) %>%
  select(state, year, personal_income_capita) %>%
  ungroup()

# demographic - age and sex
age_sex <- read_csv(here(data_path, "Population by Age and Sex - EDDs.csv")) %>%
  clean_names() %>%
  select(-ibrc_geo_id) %>%
  group_by(state, year) %>%
  summarize(across(where(is.numeric), ~sum(.x))) %>%
  ungroup() %>% 
  mutate(state = str_to_lower(abbr2state(state))) %>%
  mutate(across(population_0_4:female_population, ~.x/total_population)) %>%
  select(-total_population)

# demographic - race
race <- read_csv(here(data_path, "Population by Race - EDDs.csv")) %>%
  clean_names() %>%
  select(-ibrc_geo_id) %>%
  group_by(state, year) %>%
  summarize(across(where(is.numeric), ~sum(.x))) %>%
  ungroup() %>%
  mutate(state = str_to_lower(abbr2state(state))) %>%
  mutate(across(white_alone:hispanic, ~.x/total_population)) %>%
  select(-total_population) %>%
  filter(year > 1999)


# Estimating 2000, 2020, and 2024 demographic information with linear trend --------

# simple linear filter, grabs closest 5 years and projects linear prediction onto missing year
simple_linear_filter <- function(data, include_2000 = TRUE){
  # grab commodity name
  commodity <- data[,3] %>% colnames()
  
  # convert to list
  list_df <- data %>%
    rename(value = all_of(commodity)) %>%
    group_by(state) %>%
    group_split()
  
  filtered_df <- map(list_df, ~{
    
    if(include_2000 == TRUE){data_2000 <- .x %>% slice(1:5)}
    data_2024 <- .x %>% slice_tail(n = 5)
    
    if(include_2000 == TRUE){coef_2000 <- lm(data_2000$value ~ data_2000$year)$coefficients}
    coef_2024 <- lm(data_2024$value ~ data_2024$year)$coefficients
    
    if(include_2000 == TRUE){pred_2000 <- coef_2000[1] + 2000*coef_2000[2]}
    pred_2020 <- coef_2024[1] + 2020*coef_2024[2]
    pred_2024 <- coef_2024[1] + 2024*coef_2024[2]
    
    if(include_2000 == TRUE){
    .x <- .x %>%
      add_row(state = .x$state[1], year = 2000, value = pred_2000)}
    
    .x %>%
      add_row(state = .x$state[1], year = 2020, value = pred_2020) %>%
      add_row(state = .x$state[1], year = 2024, value = pred_2024) %>%
      arrange(year) %>%
      rename_with(~paste0(commodity), starts_with("value"))
  })
  
  filtered_df %>%
    data.table::rbindlist() %>%
    as_tibble()
}

# for many attributes in same tibble
prop_linear_filter <- function(data){
  # grab commodity name
  commodities <- data[,-c(1,2)] %>% colnames() %>% as.list()
  
  meta_list_df <- map(commodities, ~data %>% select(state, year, all_of(.x)))
  
  map(meta_list_df, ~simple_linear_filter(.x, include_2000 = FALSE)) %>%
    do.call(bind_cols, .) %>%
    rename(state = state...1,
           year = year...2) %>%
    select(-contains("..."))
}

employment <- employment %>% simple_linear_filter()
income <- income %>% simple_linear_filter()

# for proportions
# was going to normalize to 1 as a sanity check, but they already are
as <- age_sex %>% prop_linear_filter() %>%
  select(-population_under_18, -population_18_54, -population_55)
r <- race %>% prop_linear_filter()


full <- pres %>%
  select(year, state, total_votes) %>%
  filter(year != 1976) %>%
  distinct() %>%
  bind_rows(turnout_2024 %>% select(-vep, -total_potential_workers)) %>%
  mutate(state = str_to_lower(state)) %>%
  left_join(nat_polls, by = join_by(year)) %>%
  left_join(prev_year, by = join_by(year == next_election, state)) %>%
  left_join(turnout, by = join_by(year, state)) %>%
  left_join(income, by = join_by(year, state)) %>%
  # left_join(employment, by = join_by(year, state)) %>%
  left_join(as, by = join_by(year, state)) %>%
  left_join(r, by = join_by(year, state)) %>%
  filter(year > 1999) %>%
  drop_na() %>% #lose 6 states without demographic info
  mutate(
    turnout_percentage = total_votes/vep,
    # employment_percentage = total_employment/total_potential_workers
    ) #%>%
  # select(-total_employment, -total_potential_workers)

write_csv(full, here(data_path, "complete_data.csv"))

# Citations ---------------------------------------------------------------
# Election Project - voter turnout
# https://www.electproject.org/election-data/voter-turnout-data

# Florida Election Lab
# 2024 voter turnout
# # https://election.lab.ufl.edu/data-archive/national/

# National Election Polling
# https://www.presidency.ucsb.edu/statistics/data/election-year-presidential-preferences
# gallup, rcp

# Harvard - Election
# MIT Election Data and Science Lab, 2017, "U.S. President 1976–2020", https://doi.org/10.7910/DVN/42MVDX, Harvard Dataverse, V8, UNF:6:F0opd1IRbeYI9QyVfzglUw== [fileUNF]

# Demographic Information - stats America
# https://www.statsamerica.org/downloads/default.aspx
