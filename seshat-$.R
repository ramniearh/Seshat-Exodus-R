library(here)
library(tidyverse)
library(janitor)
here()

# entry papers:
# complexity  https://www.pnas.org/doi/10.1073/pnas.1708800115
# overview https://ora.ox.ac.uk/objects/uuid:5b10c64e-51ae-4ba0-8766-a1bea932c805
# monetary system+: https://www.researchgate.net/publication/343260679_An_Introduction_to_Seshat_Global_History_Databank

#eqo_test <- filter(equinox_original, NGA.y=="Cambodian Basin", Variable == "Religious levels")

##dirty first test:
#for now, not looking at military and religion - ony general and social complexity
#for now, notonly considering one social complexity variable from each subsection
#equinox <- read.csv("~/GitHub/Seshat-Exodus-R/Equinox2020.05.2023-20240404.csv", header=TRUE) 

db <- read.csv("~/GitHub/Seshat-Exodus-R/SESHAT Equinox-2020 20230425.csv", header=TRUE) %>% 
  select(NGA.y, Date.From, Variable, Value.From) %>% #Value.To, Date.To, ) %>% 
  as_tibble() %>% 
  filter( #money: CHECK Debt and Credit, Store of Wealth not found on Equinox
    Variable == "Articles" | 
      Variable == "Tokens" | 
      Variable == "Paper currency" |
      Variable == "Precious metals" | 
      Variable == "Foreign coins" | 
      Variable == "Indigenous coins" |     # unavaiable on equinox? "Paper currency" | "Debt And Credit Structure" |  
      Variable == "Store of Wealth" | #social complexity:       #communication distance?
      Variable == "Population of the largest settlement"  | 
      Variable == "Polity population"  | 
      Variable == "Polity territory"  | 
      Variable == "Administrative Levels"  |
      Variable == "Military Levels"  |
      Variable == "Religious Levels"  |
      Variable == "Settlement hierarchy"  |
      Variable == "Occupational Complexity"  |
      Variable == "Specialized Government Building"  |
      Variable == "Professional soldiers" | #add military officer, source of support, lawyers?
      Variable == "Professional priesthood" |
      Variable == "Full-time bureaucrats" |
      Variable == "Specialized government buildings" |
      Variable == "Formal legal code" | #add polity-owned specialized buildings? market communal
      Variable == "markets" | #CHECK-REVIEW!? polity owned? other specialized buildings?
      Variable == "irrigation systems" | #add communal buildings? symbolic buildings?
      Variable == "Roads" | # below: information
      Variable == "Mnemonic devices" |
      Variable == "Written records"  |
      Variable == "Script"  |
      Variable == "Calendar"  |
      Variable == "History"  |
      Variable == "Sacred Texts"  |
      Variable == "Practical literature"  |
      Variable == "Postal stations"  |  #General : #Variable == "UTM zone" | 
      Variable == "Peak Date" |
      Variable == "Duration" |
      Variable == "Degree of centralization" |
      Variable == "Religious levels"  #Variable == "production of public goods" |? religious? #Variable == "Supracultural entity" 
    ) %>% clean_names() %>%
  pivot_wider(id_expand = FALSE, names_from = variable, values_from = value_from, values_fn = max) %>% clean_names() %>% # review values_fn  issue with non-unique values: dbf %>%   dplyr::group_by(NGA.y, Date.From, Variable) %>%  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%   dplyr::filter(n > 1L) 
  mutate(
    population_of_the_largest_settlement = as.numeric(population_of_the_largest_settlement),
    date_from = as.numeric(date_from),
    religious_levels = as.numeric(religious_levels),
    settlement_hierarchy = as.numeric(settlement_hierarchy),
    polity_territory = as.numeric(polity_territory)
    ) %>%
  mutate( tokens = case_when(tokens == "present" ~ TRUE,
                        tokens == "inferred present" ~TRUE,
                        tokens == "absent" ~ FALSE,
                        tokens == "inferred absent" ~ FALSE),
          specialized_government_buildings = case_when(specialized_government_buildings == "present" ~ TRUE,
                                                       specialized_government_buildings == "inferred present" ~TRUE,
                                                       specialized_government_buildings == "absent" ~ FALSE,
                                                       specialized_government_buildings == "inferred absent" ~ FALSE),
          script = case_when( script == "present" ~ TRUE,
                              script == "inferred present" ~ TRUE,
                              script == "absent" ~ FALSE,
                              script == "inferred absent" ~ FALSE),
          sacred_texts = case_when( sacred_texts == "present" ~ TRUE,
                                    sacred_texts == "inferred present" ~ TRUE,
                                    sacred_texts == "absent" ~ FALSE,
                                    sacred_texts == "inferred absent" ~ FALSE),
          roads = case_when( roads == "present" ~ TRUE,
                             roads == "inferred present" ~ TRUE,
                             roads == "absent" ~ FALSE,
                             roads == "inferred absent" ~ FALSE),
          articles = case_when( articles == "present" ~ TRUE,
                                articles == "inferred present" ~ TRUE,
                                articles == "absent" ~ FALSE,
                                articles == "inferred absent" ~ FALSE),
          written_records = case_when( written_records == "present" ~ TRUE,
                                       written_records == "inferred present" ~ TRUE,
                                       written_records == "absent" ~ FALSE,
                                       written_records == "inferred absent" ~ FALSE),
          professional_soldiers = case_when( professional_soldiers == "present" ~ TRUE,
                                             professional_soldiers == "inferred present" ~ TRUE,
                                             professional_soldiers == "absent" ~ FALSE,
                                             professional_soldiers == "inferred absent" ~ FALSE),
          professional_priesthood = case_when( professional_priesthood == "present" ~ TRUE,
                                               professional_priesthood == "inferred present" ~ TRUE,
                                               professional_priesthood == "absent" ~ FALSE,
                                               professional_priesthood == "inferred absent" ~ FALSE),
          practical_literature = case_when( practical_literature == "present" ~ TRUE,
                                            practical_literature == "inferred present" ~ TRUE,
                                            practical_literature == "absent" ~ FALSE,
                                            practical_literature == "inferred absent" ~ FALSE),
          postal_stations = case_when( postal_stations == "present" ~ TRUE,
                                       postal_stations == "inferred present" ~ TRUE,
                                       postal_stations == "absent" ~ FALSE,
                                       postal_stations == "inferred absent" ~ FALSE),
          mnemonic_devices = case_when( mnemonic_devices == "present" ~ TRUE,
                                        mnemonic_devices == "inferred present" ~ TRUE,
                                        mnemonic_devices == "absent" ~ FALSE,
                                        mnemonic_devices == "inferred absent" ~ FALSE),
          markets = case_when(markets == "present" ~ TRUE,
                              markets == "inferred present" ~ TRUE,
                              markets == "absent" ~ FALSE,
                              markets == "inferred absent" ~ FALSE),
          irrigation_systems = case_when( irrigation_systems == "present" ~ TRUE,
                                          irrigation_systems == "inferred present" ~ TRUE,
                                          irrigation_systems == "absent" ~ FALSE,
                                          irrigation_systems == "inferred absent" ~ FALSE),
          history = case_when( history == "present" ~ TRUE,
                               history == "inferred present" ~ TRUE,
                               history == "absent" ~ FALSE,
                               history == "inferred absent" ~ FALSE),
          full_time_bureaucrats = case_when( full_time_bureaucrats == "present" ~ TRUE,
                                             full_time_bureaucrats == "inferred present" ~ TRUE,
                                             full_time_bureaucrats == "absent" ~ FALSE,
                                             full_time_bureaucrats == "inferred absent" ~ FALSE),
          formal_legal_code = case_when( formal_legal_code == "present" ~ TRUE,
                                         formal_legal_code == "inferred present" ~ TRUE,
                                         formal_legal_code == "absent" ~ FALSE,
                                         formal_legal_code == "inferred absent" ~ FALSE),
          calendar = case_when( calendar == "present" ~ TRUE,
                                calendar == "inferred present" ~ TRUE,
                                calendar == "absent" ~ FALSE,
                                calendar == "inferred absent" ~ FALSE),
          paper_currency = case_when( paper_currency == "present" ~ TRUE,
                                      paper_currency == "inferred present" ~ TRUE,
                                      paper_currency == "absent" ~ FALSE,
                                      paper_currency == "inferred absent" ~ FALSE),
          foreign_coins = case_when( foreign_coins == "present" ~ TRUE,
                                     foreign_coins == "inferred present" ~ TRUE,
                                     foreign_coins == "absent" ~ FALSE,
                                     foreign_coins == "inferred absent" ~ FALSE),
          indigenous_coins = case_when( indigenous_coins == "present" ~ TRUE,
                                        indigenous_coins == "inferred present" ~ TRUE,
                                        indigenous_coins == "absent" ~ FALSE,
                                        indigenous_coins == "inferred absent" ~ FALSE),
          precious_metals = case_when( precious_metals == "present" ~ TRUE,
                                       precious_metals == "inferred present" ~ TRUE,
                                       precious_metals == "absent" ~ FALSE,
                                       precious_metals == "inferred absent" ~ FALSE)
  ) %>% 
  mutate (###CONFIRM! no definitions found in SESHATDB
    degree_of_centralization = case_when( degree_of_centralization == "loose" ~ 0,
                                          degree_of_centralization == "nominal" ~ 1,
                                          degree_of_centralization == "quasi-polity" ~ 2,
                                          degree_of_centralization == "confederated state" ~ 3,
                                          degree_of_centralization == "unitary state" ~ 4
    )
  ) %>% select(-duration,-peak_date) ###review - removing for convenience
#db %>%  view()
#write.csv(db, "equinox_filtered_money.csv")
db %>%
  ggplot(mapping = aes(x = paper_currency, y = indigenous_coins, label = nga_y)) +
  geom_point() + geom_label()

summary(db)

correlations_db <- db %>% 
  #select(-roads,-script,-full_time_bureaucrats,-irrigation_systems,-practical_literature,-postal_stations,-sacred_texts,-religious_levels) %>% 
  select(-nga_y) %>% cor(use="complete.obs")  
heatmap(as.matrix(correlations_db), symm = TRUE)

money_correlations_db <- db %>% 
  select(
    articles, 
    tokens, 
    paper_currency, 
    foreign_coins, 
    indigenous_coins, 
    precious_metals,
    mnemonic_devices,
    polity_territory,
    population_of_the_largest_settlement,
    formal_legal_code,
    written_records,
    postal_stations,
    degree_of_centralization,
    settlement_hierarchy,
    roads,
    history,
    calendar,
    markets,
    full_time_bureaucrats,
    professional_priesthood,
    professional_soldiers
         ) %>%
  cor(use="complete.obs")  
heatmap(as.matrix(money_correlations_db), symm = TRUE)

write.csv(db, "equinox_filtered_money.csv")
write.csv(correlations_db, "seshat_selected_correlations.csv")

#dyachronic/ diffusion map: tokens

db %>%
  ggplot(mapping = aes(x = date_from, y = tokens)) +
  geom_point() #+ geom_label()

#Precious metals ♣ ♥ non-coined silver, gold, platinum
# ♠ Foreign coins ♣ ♥
# ♠ Indigenous coins ♣ ♥
# ♠ Paper currency ♣ ♥ Or another kind of fiat money. Note that this only refers to indigenously
# produced paper currency. Code absent if colonial money is used.
# ♠ Debt and credit structures ♣ ♥ commercial/market practices that take physical form, e.g. a contract
# on parchment (not just verbal agreements)
# ♠ Store of wealth 




#### To do:
# filter all data to include broader variables: 
# "Religious levels"
# "Settlement hierarchy"
# "Administrative levels"
# "Professional soldiers"
# "Specialized government buildings"
# "Professional priesthood"
# "Formal legal code"
# "Foreign coins"
# "General postal service"
# "Duration"
# "Degree of centralization"
# "Indigenous coins"
# "Paper currency"
# "Capital"
# "Tokens"
# "Calendar"
# "Moralizing enforcement in afterlife"
# "Supra-polity relations"
# "Phonetic alphabetic writing"
# "elite status is hereditary"
# "Rulers are legitimated by gods"
# "Rulers are gods"
# "Ideological reinforcement of equality"
# "Nonwritten records" 
# "Ideology reinforces prosociality"
# "production of public goods"  
# "Sacred Texts"     
# "Practical literature" 
# "History" 
# "irrigation systems"
# "Non-phonetic writing"
# "Philosophy"
# "Bridges"
# "Written records"
# "Script"
# "food storage sites"
# "markets"
# "Scientific literature"  
# "Mines or quarries"
# "Mnemonic devices" 
# "Supracultural entity"
# "Polity Population" 
# "Population of the largest settlement"  
## FROM CODEBOOK:
# Money
# Code the variables below as absent/present/inferred present/inferred absent/uncoded/unknown (we are mainly
#                                                                                              interested to know which is the most sophisticated form of money)
# ♠ Articles ♣ ♥ items that have both a regular use and are used as money (example: axes, cattle, measures
#                                                                          of grain, ingots of non-precious metals)
# ♠ Tokens ♣ ♥ unlike articles, used only for exchange. unlike coins are not manufactured (example:
#                                                                                            cowries)
# ♠ Precious metals ♣ ♥ non-coined silver, gold, platinum
# ♠ Foreign coins ♣ ♥
# ♠ Indigenous coins ♣ ♥
# ♠ Paper currency ♣ ♥ Or another kind of fiat money. Note that this only refers to indigenously
# produced paper currency. Code absent if colonial money is used.
# ♠ Debt and credit structures ♣ ♥ commercial/market practices that take physical form, e.g. a contract
# on parchment (not just verbal agreements)
# ♠ Store of wealth ♣ ♥ (example: hoard, chest for storing valuables, treasury room). Note for the future:
#   perhaps should separate these into individual variables.



