/*! \file mllite.h
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

#ifndef _MLLITE_H_
#define _MLLITE_H_

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>

// A machine word can hold either a primitive value (nil, int, or bool), or a pointer
typedef struct _mll_word_ *MLL_word_t;

/***** ML Lite Bool type *****/

#define MLL_true       ((MLL_word_t)3)
#define MLL_false      ((MLL_word_t)1)

/***** ML Lite Int type *****/

static inline MLL_word_t MLL_fromInt (int64_t n)
{
    return (MLL_word_t)((n << 1) | 1);
}

static inline int64_t MLL_toInt (MLL_word_t n)
{
    return ((int64_t)n) >> 1;
}

/***** ML Lite tuple types *****/

typedef MLL_word_t    *MLL_tuple_t;

MLL_word_t _mll_alloc (int64_t nw);

/***** ML Lite String type *****/

typedef struct {
    MLL_word_t        _len;           // number of characters as tagged int
    char                _data[8];       // character data
} MLL_string_t;

MLL_string_t *MLL_AllocString (int len);

/***** ML Lite List type *****/
#define MLL_Nil       ((MLL_word_t)1)

// LANGF cons (assuming that we have space)
//
static inline MLL_word_t MLL_Cons (MLL_word_t hd, MLL_word_t tl)
{
    MLL_word_t *cons = (MLL_word_t *)_mll_alloc(2);
    cons[0] = hd;
    cons[1] = tl;
    return (MLL_word_t)cons;
}

/***** GC API *****/

void _mll_invoke_gc (MLL_word_t *sp, size_t allocSz, uint32_t numRoots, MLL_word_t *roots);
void _mll_init_heap ();

typedef struct {
    uint64_t    _baseAddr;              // base address of semispace
    uint64_t    _usedTop;               // top of used region of semispace
    uint64_t    _szB;                   // total size of semispace
} MLL_semispace_t;

typedef struct {
    MLL_word_t         *_allocPtr;     // next word to allocate in to-space
    MLL_word_t         *_limitPtr;     // top of to-space (_toSp->baseAddr + _toSp->szB)
    MLL_semispace_t    *_toSp;         // current to-space
    MLL_semispace_t    *_fromSp;       // current from-space
} MLL_heap_t;

extern MLL_heap_t _mll_heap;

static inline bool isPtr (MLL_word_t w)
{
    return (((uint64_t)w & 0x7) == 0);
}

static inline bool isInt (MLL_word_t w)
{
    return (((uint64_t)w & 0x1) == 0x1);
}

/* we use the following tagging scheme in the low 3 bits of heap words:
 *
 *      000             -- 8-byte aligned pointer
 *      010             -- tuple
 *      100             -- forward pointer (in object-header slot)
 *      110             -- string object header
 *      xx1             -- tagged integer
 */
#define TAG_MASK        0x7             /* mask off low 3 bits */
#define TAG_FWDPTR      0x4             /* tag for forward pointers */
#define TAG_TUPLE       0x2
#define TAG_STRING      0x6

/* make a tuple-object header */
static inline MLL_word_t MLL_make_tuple_header (int n)
{
    return (MLL_word_t)(((uint64_t)n << 3) | TAG_TUPLE);
}

/* make a string-object header, where `n` is the number of characters in the string */
static inline MLL_word_t MLL_make_string_header (int n)
{
  /* size of string object includes length field, data, zero terminator,
   * and padding (but does not include object header).
   */
    uint64_t nw = ((n + 8) >> 3) + 1;
    return (MLL_word_t)(((uint64_t)nw << 3) | TAG_STRING);
}

/* extract and return the number of words in an object from its header */
static inline uint64_t MLL_get_size_from_header (uint64_t hdr)
{
    return (hdr >> 3);
}

#endif // !_MLLITE_H_
