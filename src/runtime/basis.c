/*! \file basis.c
 *
 * \author John Reppy
 *
 * Sample code
 * CMSC 22600
 * Autumn 2021
 * University of Chicago
 *
 * The implementation of the various runtime library functions.
 */

/*
 * COPYRIGHT (c) 2021 John Reppy (http://cs.uchicago.edu/~jhr)
 * All rights reserved.
 */

#include "mllite.h"
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

// uncomment to trace calls to allocation functions
//#define TRACE_ALLOC

#ifndef NDEBUG
// debugging support: check if a value is a pointer and print a message if not
//
static inline bool assertPtr (char *msg, MLL_word_t p)
{
    if (! isPtr(p)) {
        fprintf(stderr, "expected pointer for '%s', but found %p\n", msg, p);
        return false;
    }
    else
        return true;
}
#endif

// do we need to do a garbage collection before allocating nwords of space?
//
static inline bool NeedGC (size_t nwords)
{
    return (_mll_heap._allocPtr + nwords > _mll_heap._limitPtr);
}

// allocate nwords of space in the heap (plus a header word)
//
static inline MLL_word_t *Alloc (MLL_word_t hdr, size_t nwords)
{
    MLL_word_t *obj = _mll_heap._allocPtr;
    *obj++ = hdr;
    _mll_heap._allocPtr = obj + nwords;
    assert (assertPtr("obj", (MLL_word_t)obj));

    return obj;
}

// Compute the number of words required for a string with the given
// number of characters.  The result includes a word for the length field,
// the data, a byte for the nul terminator, and padding to a multiple
// of 8 bytes.  It does not include the header.
//
static inline size_t NumWordsForString (size_t nChars)
{
    return ((nChars + 8) >> 3) + 1;
}

// Allocate a string object and initialize its length field and null terminator,
// but not its data.  We assume that the GC check has already been done.
//
MLL_string_t *MLL_AllocString (int len)
{
    int nw = NumWordsForString (len);
    MLL_word_t *obj = Alloc(MLL_make_string_header(nw), nw);
    obj[nw-1] = (MLL_word_t)0;  // zero-termination and padding
    MLL_string_t *s = (MLL_string_t *)obj;
    s->_len = MLL_fromInt(len);

    return s;
}

// concatenate two strings
//
MLL_word_t _mll_concat (MLL_word_t a, MLL_word_t b)
{
    MLL_word_t dummy;    /* the address of this is used to get the stack pointer */

    assert (assertPtr("a", a) && assertPtr("b", b));
    MLL_string_t *s1 = (MLL_string_t *)a;
    uint64_t n1 = MLL_toInt(s1->_len);
    MLL_string_t *s2 = (MLL_string_t *)b;
    uint64_t n2 = MLL_toInt(s2->_len);

    if (n1 == 0) {
        return b;
    }
    else if (n2 == 0) {
        return a;
    }
    uint64_t len = n1 + n2;
  // number of words (including zero byte and length)
    size_t nw = NumWordsForString(len);

  // check for GC
    if (NeedGC(nw)) {
        MLL_word_t roots[2] = {a, b};
        _mll_invoke_gc (&dummy, nw, 2, roots);
        s1 = (MLL_string_t *)roots[0];
        s2 = (MLL_string_t *)roots[1];
    }

  // allocate and initialize the object
    MLL_string_t *s = MLL_AllocString(len);
    char *p = s->_data;
    memcpy (p, s1->_data, n1);
    p += n1;
    memcpy (p, s2->_data, n2);

#ifdef TRACE_ALLOC
    printf("_mll_string_cat (%p, %p) -> %p\n", a, b, s);
#endif
    return (MLL_word_t)s;
}

// runtime function for exiting with a status code
//
void _mll_exit (MLL_word_t a)
{
    int64_t sts = MLL_toInt(a);
    exit (sts);
}

// runtime function for reporting failures
//
void _mll_fail (MLL_word_t a)
{
    assert (assertPtr("a", a));
    MLL_string_t *s = (MLL_string_t *)a;
    uint64_t n = MLL_toInt(s->_len);
    if (s->_data[n] != '\0') {
        fprintf (stderr, "FAILURE: bogus argument to fail is not zero-terminated\n");
    }
    else {
        fprintf (stderr, "FAILURE: %s\n", s->_data);
    }
    exit (1);
}

// runtime function for printing
//
void _mll_print (MLL_word_t a)
{
    assert (assertPtr("a", a));
    MLL_string_t *s = (MLL_string_t *)a;
    uint64_t n = MLL_toInt(s->_len);
    if (s->_data[n] != '\0') {
        fprintf (stderr, "FAILURE: bogus argument to print is not zero-terminated\n");
        exit (1);
    }
    else {
        fprintf (stdout, "%s", s->_data);
    }
}

// runtime function for converting an ASCII code point to a single-character
// string.
//
MLL_word_t _mll_str_chr (MLL_word_t c)
{
    MLL_word_t dummy;    /* the address of this is used to get the stack pointer */

  // check for GC
    if (NeedGC(2)) {
        _mll_invoke_gc (&dummy, 2, 0, 0);
    }

    int64_t cpt = MLL_toInt(c);
    assert ((0 < cpt) && (cpt <= 255));

  // allocate and initialize the object
    MLL_string_t *s = MLL_AllocString(1);
    s->_len = MLL_fromInt(1);
    s->_data[0] = (char)cpt;

    return (MLL_word_t)s;
}

// runtime function for allocating space for a tuple object
//
MLL_word_t _mll_alloc (int64_t nwords)
{
    MLL_word_t dummy;    /* the address of this is used to get the stack pointer */

  // check for GC
    if (NeedGC (nwords+1)) {
        _mll_invoke_gc (&dummy, nwords+1, 0, 0);
    }

    return (MLL_word_t)Alloc(MLL_make_tuple_header(nwords), nwords);

}
