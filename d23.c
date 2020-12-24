#include <stdio.h>
#include <stdlib.h>
#include "d23.h"

/* int main (void) */
/* { */
/*     int first_input[] = {6, 2, 4, 3, 9, 7, 1, 5, 8}; */
/*     long int ans = run_game(first_input, 9, 1000000, 10000000); */
/*     printf("%ld\n", ans); */
/* } */

long int run_game(int *first_input, int first_input_len, int full_len, int nrounds) {
    // Initialise the game array
    int *game = malloc(sizeof(int) * full_len + 1);
    // Fill in the provided values. game[n] will contain the number that is after n.
    game[0] = 0;  // Helps me avoid off-by-one errors.
    for (int i = 0; i < first_input_len - 1; i++) {
        game[first_input[i]] = first_input[i + 1];
    }
    // Pad with extra numbers if necessary.
    if (full_len > first_input_len) {
        game[first_input[first_input_len - 1]] = first_input_len + 1;
        for (int i = first_input_len + 1; i < full_len; i++) {
            game[i] = i + 1;
        }
        game[full_len] = first_input[0];
    }
    // Otherwise, just close the loop.
    else {
        game[first_input[first_input_len - 1]] = first_input[0];
    }
    int current = first_input[0];

    for (int i = 0; i < nrounds; i++) {
        game = update(game, &current, full_len);
    }
    long int score = count_score(game);
    free(game);
    return score;
}


int *update(int *game, int *current, int length)
{
    // Pick out the three cups
    int a = game[*current];
    int b = game[a];
    int c = game[b];
    int d = game[c];
    game[*current] = d;

    // Figure out what the destination cup is. It needs to be smaller than the
    // current value, *unless* there is no such cup in which case it needs to
    // be the largest available cup.
    int destination = *current - 1;
    for (;;) {
        if (destination < 1) destination = length;
        if (destination == a || destination == b || destination == c) {
            destination--;  // was picked up already
        }
        else break;
    }

    // Insert the three cups back in
    game[c] = game[destination]; // link third cup -> cup after destination
    game[destination] = a;       // link destination -> first cup
    // Move the current cup to the next
    *current = d;
    
    return game;
}

void print_game(int *game, int *current, int length)
{
    int a = *current;
    for (int i = 0; i < length; i++) {
        printf("%d ", a);
        a = game[a];
    }
    printf("\n");
}

long int count_score(int *game)
{
    return (long int) game[1] * game[game[1]];
}
