db = read.csv("~/GitHub/Seshat-Exodus-R/SESHAT Equinox-2020 20230425.csv", header=TRUE)

str(db)
#length(unique(db$NGA.y))
#length(unique(db$PolID))

#length(unique(db$Section))
unique(db$Section)

#length(unique(db$Subsection))
unique(db$Subsection)

unique(db$Variable)
length(unique(db$Variable))

#### To do 
# filter all data to include variables: 
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
# 


test <- subset(db, db$Variable == "markets" & db$Value.From == "present")
?subset
