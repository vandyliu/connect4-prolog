# This is connect4 in prolog.

## Instructions
1. `swipl`
2. `[c4].`
3. `play.`

There are 5 algorithms coded: 2 cool ones.
1. Simple algorithm: always chooses columns in same order.
2. Random algorithm: plays random move.
3. Normal-distribution algorithm: plays moves randomly but normally distributed with mean as the center column.
4. Minimax with alpha-beta pruning: plays out moves in advance as each player and chooses the best move from those games it plays.
5. Monte-carlo: plays out games with each move then plays out the games randomly and chooses the move that wins the most.