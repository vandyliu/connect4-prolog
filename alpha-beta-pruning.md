Minimax with Alpha-Beta pruning Explanation
1. At the MAX (white) player, the maximizing player chooses the MAXIMUM move (best move that the player can play)
2. At the MIN (black) player, the minimizing player chooses the MINIMUM move (best move that the player can play)
3. In the picture, we go through most of the nodes, and see that black's choice at the second layer must be <= -4 because it tries to minimize.
4. Because of this, white is guaranteed to play the move that lets it gets a score of 3 because it maximizes and already has a better option available so we can prune some branches.