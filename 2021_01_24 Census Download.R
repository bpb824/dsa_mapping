library(tidycensus)
library(tigris)
library(tidyverse)
library(sf)

dsa_precincts = read_rds('data/dsa_precincts.rds')
or_county_geogs = counties(state="OR",class="sf")
dsa_county_fips_codes = dsa_precincts$county %>% unique()
or_blockgroup_geogs = block_groups(state='OR',county = dsa_county_fips_codes, class='sf')

#census_api_key('04976a4a378107d2fb53acdbde84d0aad121cb10',install = TRUE)

census_vars = load_variables(year=2019,dataset = 'acs5')

total_population = get_acs('block group',variables = 'B01003_001',state='OR',
                     county = dsa_county_fips_codes)
total_households = get_acs('block group',variables = 'B19001_001',state='OR',
                           county = dsa_county_fips_codes)

concepts_of_interest = c(
  'SEX BY AGE',
  'HISPANIC OR LATINO ORIGIN BY RACE',
  'CITIZEN, VOTING-AGE POPULATION BY EDUCATIONAL ATTAINMENT',
  'HOUSEHOLDS BY TYPE', #Children
  'HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS)'
)

variables_of_interest = census_vars %>%
  filter(concept %in% concepts_of_interest)

#Querying data for all variables of interest
big_var_query = get_acs('block group',variables = variables_of_interest$name,
                        state='OR',
                        county = dsa_county_fips_codes)

query_results = big_var_query %>%
  mutate(county_fips = str_sub(GEOID,3,5)) %>%
  select(-NAME) %>%
  left_join(variables_of_interest %>%
              rename(variable = name))

#Getting relationship of block groups to precincts for area-weighted-summation ----------

bg_int = or_blockgroup_geogs %>%
  st_transform(2269) %>%
  select(GEOID,geometry) %>%
  mutate(area_bg_whole_ft2 = st_area(geometry) %>% as.numeric()) %>%
  st_intersection(dsa_precincts %>% select(precinct,geometry) %>%
                    st_transform(2269) %>%
                    st_simplify() %>%
                    mutate(area_precinct_whole_ft2 = st_area(geometry) %>% as.numeric())) %>%
  mutate(area_int_ft2 = st_area(geometry) %>% as.numeric()) %>%
  as_tibble() %>%
  select(-geometry) %>%
  mutate(precinct_coverage = round(area_int_ft2/area_precinct_whole_ft2,6)) 

bg_precinct_ref = bg_int %>%
  select(GEOID,precinct,precinct_coverage)

#Cleaning up individual datasets and summing by precinct by concept of interest ----------------

## Population Reference ---------

clean_pop_ref = total_population %>%
  select(GEOID,estimate) %>%
  rename(people = estimate) %>%
  left_join(total_households %>%
              select(GEOID,estimate) %>%
              rename(households = estimate)) %>%
  left_join(bg_precinct_ref) %>%
  mutate(adj_people = people*precinct_coverage,
         adj_households = households*precinct_coverage) %>%
  group_by(precinct) %>%
  summarise(people = sum(adj_people),
            households = sum(adj_households)) %>%
  mutate(avg_hh_size = people/households)

## Age ------------

clean_age = query_results %>%
  filter(concept=='SEX BY AGE') %>%
  separate(label,sep='!!',into=c('temp_1','temp_2','gender','age_group')) %>%
  select(-temp_1,-temp_2) %>%
  filter(!is.na(age_group)) %>%
  group_by(GEOID,age_group) %>%
  summarise(estimate= sum(estimate)) %>%
  left_join(bg_precinct_ref) %>%
  mutate(rev_est = estimate*precinct_coverage) %>%
  group_by(precinct,age_group) %>%
  summarise(estimate = sum(rev_est)) %>%
  mutate(units = 'People')

## Race --------------

clean_race = query_results %>%
  filter(concept=='HISPANIC OR LATINO ORIGIN BY RACE') %>%
  separate(label,sep='!!',into=c('temp_1','temp_2','latinx_status','race')) %>%
  filter(!is.na(race)) %>%
  mutate(simp_race = ifelse(latinx_status == 'Hispanic or Latino:','Hispanic or Latinx',race)) %>%
  group_by(GEOID,simp_race) %>%
  summarise(estimate= sum(estimate)) %>%
  left_join(bg_precinct_ref) %>%
  mutate(rev_est = estimate*precinct_coverage) %>%
  group_by(precinct,simp_race) %>%
  summarise(estimate = sum(rev_est)) %>%
  mutate(units = 'People')

## Educational Attainment -----------

clean_education = query_results %>% 
  filter(concept == 'CITIZEN, VOTING-AGE POPULATION BY EDUCATIONAL ATTAINMENT') %>%
  separate(label,sep='!!',into=c('temp_1','temp_2','education')) %>%
  filter(!is.na(education)) %>%
  group_by(GEOID,education) %>%
  summarise(estimate= sum(estimate)) %>%
  left_join(bg_precinct_ref) %>%
  mutate(rev_est = estimate*precinct_coverage) %>%
  group_by(precinct,education) %>%
  summarise(estimate = sum(rev_est)) %>%
  mutate(units = 'People')

## Household Type (e.g., with Children or not) ----------

clean_hh_type = query_results %>%
  filter(concept == 'HOUSEHOLDS BY TYPE') %>%
  separate(label,sep='!!',into=c('temp_1','temp_2','hh_type_1','hh_type_2')) %>%
  filter(!is.na(hh_type_2)) %>%
  group_by(GEOID,hh_type_1,hh_type_2) %>%
  summarise(estimate = sum(estimate)) %>%
  left_join(bg_precinct_ref) %>%
  mutate(rev_est = estimate*precinct_coverage) %>%
  group_by(precinct,hh_type_1,hh_type_2) %>%
  summarise(estimate = sum(rev_est)) %>%
  mutate(units = 'Households')

## Household Income ----------

clean_hh_income = query_results %>%
  filter(concept == 'HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS)') %>%
  separate(label,sep='!!',into=c('temp_1','temp_2','hh_income')) %>%
  filter(!is.na(hh_income)) %>%
  group_by(GEOID,hh_income) %>%
  summarise(estimate = sum(estimate)) %>%
  left_join(bg_precinct_ref) %>%
  mutate(rev_est = estimate*precinct_coverage) %>%
  group_by(precinct,hh_income) %>%
  summarise(estimate = sum(rev_est)) %>%
  mutate(units = 'Households')


#Saving out results --------

write_csv(clean_pop_ref,'data/2021_01_24_results/people_household_totals.csv')
write_csv(clean_age,'data/2021_01_24_results/age.csv')
write_csv(clean_race,'data/2021_01_24_results/race.csv')
write_csv(clean_education,'data/2021_01_24_results/educational_attainment.csv')
write_csv(clean_hh_type,'data/2021_01_24_results/hh_type.csv')
write_csv(clean_hh_income,'data/2021_01_24_results/hh_Income.csv')
