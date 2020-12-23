#include <stdio.h>
#include <stdlib.h>
#include "d23b.h"

long int aoc_d23_part2(void)
{
    int first_input[] = {6, 2, 4, 3, 9, 7, 1, 5, 8};
    return run_game(first_input, 9, 1000000, 10000000);
}

long int run_game(int *first_input, int first_input_len, int full_len, int nrounds) {
    // Initialise the game array
    int *game = malloc(sizeof(int) * full_len);
    // Fill in the provided values
    for (int i = 0; i < first_input_len - 1; i++) {
        game[first_input[i] - 1] = first_input[i + 1];
    }
    if (full_len > first_input_len) {
        game[first_input[first_input_len - 1] - 1] = first_input_len + 1;
        for (int i = first_input_len; i < full_len - 1; i++) {
            game[i] = i + 2;
        }
        game[full_len - 1] = first_input[0];
    }
    else {
        game[first_input[first_input_len - 1] - 1] = first_input[0];
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
    int a = game[*current - 1];
    int b = game[a - 1];
    int c = game[b - 1];
    int d = game[c - 1];
    game[*current - 1] = d;

    // Figure out what the destination cup is. It needs to be smaller than a,
    // *unless* there is no such cup in which case it needs to be the largest
    // available cup.
    int destination = *current - 1;
    for (;;) {
        if (destination < 1) destination = length;
        if (destination == a || destination == b || destination == c) {
            destination--;  // not found
        }
        else break;
    }

    // Insert the three cups back in
    game[c - 1] = game[destination - 1]; // link third cup -> cup after destination
    game[destination - 1] = a;       // link destination -> first cup
    // Move the current cup to the next
    *current = d;
    
    return game;
}

void print_game(int *game, int *current, int length)
{
    int a = *current;
    for (int i = 0; i < length; i++) {
        printf("%d ", a);
        a = game[a - 1];
    }
    printf("\n");
}

long int count_score(int *game)
{
    return game[0] * game[game[0] - 1];
}
