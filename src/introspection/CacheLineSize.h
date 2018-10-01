/*
 * Original Author: Nick Strupat
 * Original Date: October 29, 2010
 * Returns the cache line size (in bytes) of the processor, or 0 on failure
 * No license info posted and source freely available on Github
 * Downloaded on 2018-01-06 from:
 * https://github.com/NickStrupat/CacheLineSize
 * Minor modifications by Izaak Beekman 2018-01-06
 */

#ifndef CACHELINESIZE_H_INCLUDED
#define CACHELINESIZE_H_INCLUDED
#include <stddef.h>
size_t CacheLineSize();

#endif
