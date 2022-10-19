install.packages("Lahman")
library(tidyverse)
library(rvest)
library(data.table)

# After getting data loaded in I want to transform them into Data Tables as it is easier for me to work in

class(Batting)

Batting <- as.data.table(Batting)

Batting <- Batting[yearID == 2021]


# I want to work with Batting first, I want to look at players who have only played in 30 or more games and more than 50 ABs

Batting <- Batting[Batting$AB > 50]

Batting <- Batting[Batting$G > 30]


# Now we have our data the way we want it
# The goal of this project is to identify undervalued players that could be added to playoff rosters for cheap and will contribute

# Something I think could be helpful is the Total Bases a player accumulates during a season

Batting[, single := Batting$H - Batting$`2B` - Batting$`3B` ]
View(Batting)

Batting[, TotalBases := single + (Batting$`2B` * 2) + (Batting$`3B` * 3) + (Batting$HR * 4)]

# With this we can create another column showing expected bases per at bat

Batting[, xBA := Batting$TotalBases/Batting$AB]

# With that let's break down and see all the players who are expected to get .6 Bases an at bat

HighBases <- Batting[xBA > .6]
View(HighBases)

# Having such a high Expected bases per at bat returns mostly super star results, none of these players would come cheap
#Let's change the parameters a bit

mean(Batting$xBA)

# We now see the Average Expected bases per at bat is .43 so lets look at numbers higher than that

GoodxBA <- Batting[Batting$xBA > mean(Batting$xBA)]
View(GoodxBA)

# We now have a much more meaning list of 255 players
# However many of these players are rostered by other playoff teams, who will be less likely to give their players up. Lets filter them out
# If the team made the playoffs in 2021 they will be assigned a 1, if not a 0

as.data.table(GoodxBA)

class(GoodxBA)

GoodxBA$Playoff <- 0

GoodxBA$Playoff <- ifelse(GoodxBA$teamID %in% Playoff, 1, 0)

#GoodxBA$Playoff <- vector("numeric", length = nrow(GoodxBA))

#Playoff <- c('LAN', 'SFN', 'ATL', 'BOS', 'HOU', 'MIL', "NYA", 'SLN', 'TBA')
  
#  for(i in 1:nrow(GoodxBA)) {
    
 #   GoodxBA$Playoff <- as.numeric(GoodxBA$teamID[i] %in% Playoff) 

# OR

#  GoodxBA$Playoff <- as.numeric(any(GoodxBA$teamID[i] == Playoff))}

# Now that we see teams that made the playoffs we can filter out those players

EligiblePlayers <- GoodxBA[GoodxBA$Playoff == 0]
View(EligiblePlayers)

write.csv(EligiblePlayers, "EligiblePlayers.csv")

#HERE MY INITIAL ANALYSIS WILL END, ELIGIBLE PLAYERS WILL GO INTO TABLEAU

# From here we have crafted a list of 170 players that meet our criteria, I personally want to narrow it down a bit more
# I want to limit strike outs for these players, if a player struck out in 20% of their at bats they're no longer considered

EligiblePlayers[, SOP := EligiblePlayers$SO/EligiblePlayers$AB]

# From this I want to only look at players that strike out less than 20% of the time

BIPDemons <- EligiblePlayers[EligiblePlayers$SOP < .20]

# We now have a list of players who consistently put the ball in play, however there is a risk
#Putting the ball in play may mean a lot of weak grounders potentially leading to Double plays, if a player does this at a high clip it may not be worth it

BIPDemons[, DPR := BIPDemons$GIDP/BIPDemons$AB]

View(BIPDemons)

# Luckily it seems none of our BIP Demons have a disturbingly high Double Play Rate (all below 5%)

# Maybe that is not what you are in the market for, maybe you want to get players that will send balls to the moon and do not care about strike outs
# Here I want to Find players who get a proportionally high amount of XBHs

Sluggers <- EligiblePlayers[, XBH := EligiblePlayers$`2B` + EligiblePlayers$`3B` + EligiblePlayers$HR]

# Lets see what players had more than 45 XBHS

Sluggers <- Sluggers[Sluggers$XBH > 45]

View(Sluggers)

# I have looked at two different types of hitters, and I kind of want to see who overlaps

Overlap <- inner_join(Sluggers, BIPDemons, by = c("playerID"))
View(Overlap)

# Looking at this I see a good few stars of the game or rising talent, they are not who we are targeting. They will not be cheap
# The names that intrigue me the most are Jake Cronenworth, Jesse Winker, Ty France, and Andrew Benintendi
# All these guys are solid players that are in their primes, but not superstars of the game. All on teams that are looking to rebuild
# Looking at the salaries of these guys and they are reasonable as well. Winker and Benitendi make 7 million a year but these contracts were earned after the 2021 season
# While France and Cronenworth make in the 700k range. 












