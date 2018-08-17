# TASK:

## Analyzing texas holdem (type of poker)

## Rules:
### Each player is dealt 2 cards. At this point they can bet on how good their hand is.
### Then dealer flips 3 cards from the deck. Again they can bet
### Then dealer flips card (now 4 cards total from deck are flipped)... again they can bet
### Then dealer flips last card (now 5 cards total from deck are flipped)... again they bet
### Player with best hand (combining 2 cards only they know of with 5 cards everyone can see) wins hand

## The list of hands includes:
###rf : 10 J Q K A all of same suit (can only occur for one player so no tie breaker)
###strFlush : 5 card straight all of same suit (winner is higher straight... can only occur in one suit)
###4kind : 4 cards same value (i.e K K K K or 2 2 2 2) (tie breaker is higher remaining card in hand otherwise money is split)
###fullhouse : 1 pair + 1 triplet (of different numbers i.e 2 2 3 3 3 or 4 4 Q Q Q etc) (tie breaker is higher full house)
###flush: 5 cards from same suit (tie breaker is higher highest card in flush)
###straight : 5 cards in a row (separated by 1 number... order doesn't matter i.e 4 5 6 7 8 or 9 10 J Q K or 6 3 2 4 5 etc) (tiebreaker is higher straight)
###3kind : a triplet (3 cards of 1 value i.e 3 3 3 or J J J etc) (tie breaker is higher 3)
###2pair : 2 pairs of different value (i.e 2 2 4 4 or 3 5 3 5 or J 9 9 J etc) (tie breaker is highest pair)
###pair : 2 cards of same value (i.e 2 2 or 8 8) (tie breaker is higher pair)
###highCard : highest card in hand (tie breaker is highest card)

# Your job:

## Given a game state (i.e 2 cards dealt + whatever is on table) calculate:
### 1. Best hand (ranging from royal flush to high card)
### 2. Probability of getting every possible hand (0 to 1 continuous)

## For example:
### If the game state is: hand <- c(1, 13, 18, 39, 51) (which represents [Ace hearts, King hearts, 5 diamonds, King clubs, Queen spades])
### Then best hand is pair [kings]
### Probability of hands is:
#### Rf: 0 [need 3 hearts only have 2]
#### strFlush: 0 [need 3 hearts only have 2]
#### 4 kind: (1/47)*(1/46) (need king on both)
#### fullHouse: 3*((3/47)*(2/46)) + 2*(2/47)*(9/46) .. check this over could be wrong
#### flush: 0 [need 3 hearts only have 2]
#### straight: 2*(4/47)*(4/46)
#### 3kind: 3*((3/47)*(2/46)) + (2/47) + (2/46)
#### ...
#### highCard: 1 (always have a "highcard")




# ---------------------- cards ---------------------- #
## modulo system
cards <- 1:52

# ---------------------- useful functions ---------------------- #
# functionToHand
handFunc <- function(hand, FUN) unlist(lapply(hand, FUN))

# hand mod
modCard <- function(card) ifelse(card %% 13 == 0, 13, card %% 13)

# check royalty card
checkRoyal <- function(card) card %% 13 %in% c(1,10,11,12,0)

# royal hand
royalHand <- function(hand) length(unique(handFunc(hand, checkRoyal))) == 1 & checkRoyal(hand[1])

# find suit
findSuit <- function(card) ifelse(card <= 13, 1, ifelse(card <= 26, 2, ifelse(card <= 39, 3, 4)))

# same suit hand
checkFlush <- function(hand) length(unique(handFunc(hand,findSuit))) == 1

# check straight
checkStraight <- function(hand) all(abs(diff(handFunc(hand, modCard))) == 1)

# count hand
countHand <- function(hand) sort(as.vector(table(handFunc(hand,modCard))),decreasing=TRUE)

# find which pair or returns 0 if no pair

whichPair <- function (hand) ifelse(pair(hand), as.numeric(rownames(countHand(hand))[1]), 0)

# ---------------------- check hands ---------------------- #
#rf

#strFlush

#4kind

#fullhouse

#flush

#straight

#3kind

#2pair

#pair

#highCard


# ---------------------- Probabilities ----------------------#

# ----------- Draws (1 3) ----------- #

# probability there will be a pair

# probability of 2pair

#3kind

#rf

#strFlush

#4kind

#fullhouse

#flush

#straight



# start with 2 cards in a hand 
# then 3 cards flipped
# then 4th card flipped
# then last card flipped

