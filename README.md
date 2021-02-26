# Haskell-AI-players-in-the-dominoes-game

Two different intelligent players implemented by Haskell for the 5s-and-3s dominoes game. Different strategies and tactics was assigned for each of them. In the competition between two players, the one with more tactics enabled won most rounds of games.

## 5S-AND-3S DOMINOES MATCHES Rules:
- A match consists of N games, typically 3. The winner is the player who has won most games. With our tireless computer players, N can be much greater.
- Each game involves a sequence of rounds (as defined in the programming assignment warm up). The two players take it in turns to drop first in each round.
- A game terminates when one player or the other wins by achieving an accumulated score, over the rounds, of exactly 61.
- If at the end of a round neither player has reached 61, a new round starts and the scores obtained within this round are added on to the scores after the last round.
- As with all good pub games, you must finish exactly, i.e. if your score is 59 you need to play a domino which scores 2 to win the game. If you score more (‘going bust’) your score remains the same (i.e. if your score is 59 and you play a domino which scores 4 your score remains at 59).
- The rules are a very slight variant on those provided in Stage 2 of the pre-assignment tasks:
- Each player starts with a hand of 9 dominoes. The remaining dominoes take no part in this round – they are ‘sleeping’.
- Players add a domino to the board (‘dropping’ a domino) in turn and accumulate the 5s-and-3s scores of their ‘drops’.
- The player who drops the first domino gets the 5s-and-3s score of its total spots.
- Play continues until neither player can play (either because s/he has run out of dominos or is knocking).

## The Design
The [Diagram](https://github.com/pokaleo/Haskell-AI-players-in-the-dominoes-game/blob/main/Diagram%20of%20The%20Original%20Design/Screenshot.png) shows how the initial design was. 
The implementation is in the [code file](https://github.com/pokaleo/Haskell-AI-players-in-the-dominoes-game/blob/main/Code/domino.hs).
Detail of the strategies and test results are in the PDF version of report.

## How to run
Once the .hs file is loaded. Simply call **domsMatch** with these parameters: -player -player -roundsOfGames -randomSeed
#### eg:
```
*DomsMatch> domsMatch player2 player1 1000 42
(626,374)
```
This code above means play 1000 rounds of game between player2 an player1 with a random seed of 42. The results indicates player 2 won 626 rounds.
