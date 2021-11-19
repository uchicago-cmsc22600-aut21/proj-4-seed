/*! \file main.c
 *
 * \author John Reppy
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 */

/*
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 */

#include "mllite.h"
#include <inttypes.h>
#include <string.h>

// the main program produced by the compiler
extern MLL_word_t _mll_entry (MLL_word_t args);

int main (int argc, const char **argv)
{
  /* initialize the heap */
    _mll_init_heap ();

  // build the list of command-line arguments; we assume that there
  // is adequate heap space
    MLL_word_t args = MLL_Nil;
    for (int i = argc - 1;  i > 0;  --i) {
        int argLen = strlen(argv[i]);
      // allocate the string
        MLL_string_t *s = MLL_AllocString (argLen);
        strncpy (s->_data, argv[i], argLen);
      // allocate the list cell
        args = MLL_Cons((MLL_word_t)s, args);
    }

  /* run the program */
    printf("## Running the program\n");
    _mll_entry (args);
    printf("## finished\n");

    return 0;

}
