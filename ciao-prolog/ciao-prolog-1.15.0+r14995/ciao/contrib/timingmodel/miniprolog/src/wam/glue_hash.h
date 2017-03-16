/*
** glue_hash.h
**
** Made by Edison Mera
** Login   <edison@vaioedison>
**
** Started on  Fri Oct 12 15:55:31 2007 Edison Mera
** Last update Tue Apr 17 16:00:00 2012 Edison Mera
*/

#ifndef 	_glue_hash_H_
# define	_glue_hash_H_

# include <string>

# if defined(__GNUC__)

#  include <unordered_map>
#  include <unordered_set>

# else

#  include <hash_map>
#  include <xhash>

#  if defined(_MSC_VER)
#   define __STL_EXTRA__ stdext
#  elif defined(__BORLANDC__)
#   define __STL_EXTRA__ stlport
#  endif

using namespace __STL_EXTRA__;

# endif

#endif // _glue_hash_H_
