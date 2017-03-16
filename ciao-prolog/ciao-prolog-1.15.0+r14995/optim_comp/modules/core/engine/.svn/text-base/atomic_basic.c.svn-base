#include <engine/basiccontrol.native.h>

#include <string.h>

/* Shared, no locked --- but it should be private! */
static intmach_t radixlim1;
static intmach_t radixlim2;
static intmach_t radixlim3;

bool_t prolog_init_radix() {
  intmach_t radix = GetSmall(current_radix);

  if (radix<10)
    radixlim1 = '0'+radix,
    radixlim2 = 'a',
    radixlim3 = 'A';
  else
    radixlim1 = '0'+10,
    radixlim2 = 'a'+radix-10,
    radixlim3 = 'A'+radix-10;
  return TRUE;
}

static CBOOL__PROTO_N(prolog_constant_codes, bool_t a,bool_t n, intmach_t ci,char *my_name, intmach_t my_arity);

CBOOL__PROTO(prolog_name) {
  ERR__FUNCTOR("atomic_basic:name", 2);
  CBOOL__LASTCALL_N(prolog_constant_codes,TRUE,TRUE,1,err__name,err__arity);
}

CBOOL__PROTO(prolog_atom_codes) {
  ERR__FUNCTOR("atomic_basic:atom_codes", 2);
  CBOOL__LASTCALL_N(prolog_constant_codes,TRUE,FALSE,1,err__name,err__arity);
}

CBOOL__PROTO(prolog_number_codes_2) {
  ERR__FUNCTOR("atomic_basic:number_codes", 2);
  CBOOL__LASTCALL_N(prolog_constant_codes,FALSE,TRUE,1,err__name,err__arity);
}

CBOOL__PROTO(prolog_number_codes_3) {
  ERR__FUNCTOR("atomic_basic:number_codes", 3);
  CBOOL__LASTCALL_N(prolog_constant_codes,FALSE,TRUE,2,err__name,err__arity);
}


 /* INTEGER :== [minus]{digit} */
 /* FLOAT :== [minus]{digit}.{digit}[exp[sign]{digit}] */
 /* ATOM :== {char} */


/* string_to_number() tries to convert the string pointed to by AtBuf in a
   number contained in the tagged word *strnum.  If this cannot be done
   (e.g., AtBuf does not correspond syntactically to a number), FALSE is
   returned.  Otherwise, TRUE is returned and the conversion is done */

CBOOL__PROTO_N(string_to_number,
	    unsigned char *AtBuf,
	    intmach_t base,
	    tagged_t *strnum,
	    intmach_t arity) {
  bool_t sign = FALSE;
  unsigned char *s = AtBuf;
  intmach_t i;
  intmach_t d;  

  i = *s++;
  if (i=='-') {
    sign = TRUE;
    i = *s++;
  }

  /* First, consider special cases */
  if((i=='0') &&
     (s[0]=='.') &&
     (s[1]=='N') &&
     (s[2]=='a') &&
     (s[3]=='n') &&
     (s[4]=='\0'))
  {
    *strnum=BoxFloat(0.0/0.0); /* Nan */
    CBOOL__PROCEED;
  }
  if((i=='0') &&
     (s[0]=='.') &&
     (s[1]=='I') &&
     (s[2]=='n') &&
     (s[3]=='f') &&
     (s[4]=='\0'))
  {
    *strnum=BoxFloat(1.0/0.0); /* Inf */
    CBOOL__PROCEED;
  }
  d = char_digit[i];
  while (0<=d && d < base) {
    i = *s++;
    d = char_digit[i];
  }
  if ((s - AtBuf) - sign > 1) {
    if (i==0) {
      /* It is an integer, either a small or a bignum */
      intmach_t req;

      req = bn_from_string((char *)AtBuf, (bignum_t *)G->heap_top, (bignum_t *)Heap_Warn(CONTPAD), base);
      if (req) {
        CVOID__CALL_N(explicit_heap_overflow, (req*sizeof(tagged_t)+CONTPAD)*2, arity);
        if (bn_from_string((char *)AtBuf, (bignum_t *)G->heap_top, (bignum_t *)Heap_Warn(CONTPAD), base)) {
          SERIOUS_FAULT("miscalculated size of bignum");
	}
      }
      *strnum = CFUN__EVAL(bn_finish);
      CBOOL__PROCEED;
    }

    /* It is a float. Note that if is a float, after the point the digits
       must not be upper case to avoid confussion with the exponent
       indicator. This is true only if base > 10 + 'e'-'a'*/

    {
      flt64_t m=0;
      flt64_t num;
      intmach_t exp=0,e=0,se=1;
      if (i=='.') {
	s = AtBuf + (sign ? 1 : 0);
	i = *s++;
	do {
	  m = m * base+char_digit[i];
	  i = *s++;
	}
	while(i!='.');
	i = *s++;
	d = char_digit[i];
	if ((0<=d) && (d<base)) {
	  do {
	    m = m * base+char_digit[i];
	    exp++;
	    i = *s++;
	    d = char_digit[i];
	  }
	  while ((0<=d) && (d<base) && (i != 'E'));
	  if ((i=='E') || (i=='e')) {
	    i = *s++;
	    if ((i=='+') || (i=='-')) {
	      if(i=='-')
		se=-1;
	      i = *s++;
	    };
	    d = char_digit[i];
	    if ((0<=d) && (d<base)) {
	      do {
		e = e * base + d;
		i = *s++;
		d = char_digit[i];
	      }
	      while ((0<=d) && (d<base));
	    } else i = -1;
	  };
	}
	else i = -1;
	if (i!=-1) {
	  if(se==1) {
	    exp = -exp + e;
	  }
	  else
	    exp = -exp - e;
	  num=m*powl_int(base,exp);
	  *strnum = BoxFloat(sign ? -num : num);
	  CBOOL__PROCEED;
	}
      }
    }
  }
  /* Could not make the conversion --- maybe a non-numeric atom */
  CBOOL__FAIL;
}

/* Precond: 2<=abs(base)<=36 */
CVOID__PROTO_N(number_to_string,
	    tagged_t term,
	    intmach_t base) {
  if (TaggedIsSmall(term))
    {
      intval_t l = GetSmall(term);
      char hibase = 'a'-10;
      bool_t sx = (l>=0);
      intmach_t digit;
      char *c0, *c, d;

      if (base<0)
	hibase = 'A'-10,
	base = -base;
      c = Atom_Buffer;
      if (!sx)
	*c++ = '-',
	l = -l;

      do
	{
	  digit = l % base;
	  l /= base;
	  *c++ = (digit<10 ? '0'+digit : hibase+digit);
	}
      while (l>0);

      *c++ = 0;
      for (c0=Atom_Buffer+1-sx, c-=2; c0<c; c0++, c--)
	d = *c0, *c0 = *c, *c = d;
    }
  else if (IsFloat(term))
    {
      flt64_t f;
      char *cbuf;
      intmach_t eng_flt_signif = (intmach_t)((SHR_EXP+1) * invlog2[base] + 1);

      /* todo: use a version of TaggedToFloat that assumes that term is a
	 float */
      f = TaggedToFloat(term);

      /* Print using the default precision.  'p' is a new 'prolog' :-)
         format not considered by, e.g., the printf family, to
         implement the expected Prolog behavior */

      cbuf = Atom_Buffer;
      cbuf = float_to_string(cbuf, eng_flt_signif, 'p', f, base);
    }
  else /* if (IsBignum(term)) */
    {
      CVOID__CALL_N(bn_to_string,TaggedToBignum(term),base);
    }
}

/*

  Note: The ci parameter indicates where are the String argument.  If
  ci = 2, then there are 3 parameters, indicating that the second
  parameter could be the numberic base.

 */
static CBOOL__PROTO_N(prolog_constant_codes,
		   bool_t atomp, bool_t numberp, intmach_t ci,
		   char *err__name, intmach_t err__arity) {
  unsigned char *s;
  intmach_t i, base;
  tagged_t car, cdr;

  /*extern flt64_t atof PROTO((char *));*/

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  if(ci==2) {
    DEREF(X(2),X(2));
  }
  if (IsVar(X(0))) {
    /* Construct a character string from the input list */
    cdr = X(ci);
    s = (unsigned char *)Atom_Buffer;
    for (i=0; cdr!=atom_nil; i++) {
      if (IsVar(cdr)) {
        BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,2);
      } else if (TaggedIsLST(cdr)) {
	ENSURE_ATOM_BUFFER(i, { s = (unsigned char *)Atom_Buffer+i; });
	DerefCar(car,cdr);
	if (IsVar(car)) {
	  BUILTIN_ERROR(INSTANTIATION_ERROR,atom_nil,ci+1);
	}
	if (!TaggedIsSmall(car) || (car<=TaggedZero) || (car>=MakeSmall(256))) {
	  BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(character_code_list)),X(ci),ci+1);
	}
	*s++ = GetSmall(car);
	DerefCdr(cdr,cdr);
      } else {
	BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(character_code_list)),X(ci),ci+1);
      }
    }
    ENSURE_ATOM_BUFFER(i, { s = (unsigned char *)Atom_Buffer+i; });
    *s++ = '\0';

    /* s contains now the string of character codes, and i its size */

    if (i>=MAXATOM) atomp = FALSE;  /* Unneded with dynamic atom sizes */

    if (numberp) {
      tagged_t result;
      if (ci==2) {
	if (IsInteger(X(1))) {
	  base = GetSmall(X(1));
	  if ((base < 2)||(base > 36)) {
	    BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(1),2);
	  }
	} else {
	  BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(1),2);
	}
      } else {
	// if (ci==1)
	base = GetSmall(current_radix);
      }
      if (CBOOL__SUCCEED_N(string_to_number, (unsigned char *)Atom_Buffer, base, &result, ci+1)) {
        CBOOL__LASTUNIFY(result, X(0));
      }
    }
    CBOOL__TEST(atomp);
    CBOOL__LASTUNIFY(GET_ATOM(Atom_Buffer), X(0));
  } else {
    if (numberp && IsNumber(X(0))) {
      if (ci==2) {
	if (IsInteger(X(1))) {
	  base = GetSmall(X(1));
	  if ((base < 2)||(base > 36)) {
	    BUILTIN_ERROR(DOMAIN_ERROR(DOMAIN_ERRORS(source_sink)),X(1),2);
	  }
	} else {
	  BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(1),2);
	}
      } else {
	base = GetSmall(current_radix);
      }
      CVOID__CALL_N(number_to_string,X(0),base);
      s = (unsigned char *)Atom_Buffer;
    } else if (atomp && TaggedIsATM(X(0))) {
      s = (unsigned char *)GetString(X(0));
    } else {
      if (numberp) {
        if (atomp) {
          BUILTIN_ERROR(TYPE_ERROR(ATOMIC),X(0),1);
	} else {
	  BUILTIN_ERROR(TYPE_ERROR(NUMBER),X(0),1);
	}
      } else {
	BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(0),1);
      }
    }
	  
    i = strlen((char *)s);
    s += i;
    TEST_HEAP_OVERFLOW(G->heap_top, CONTPAD+i*2*sizeof(tagged_t), ci+1);

    cdr = atom_nil;
    while (i>0)	{
      i--;
      MakeLST(cdr,MakeSmall(*(--s)),cdr);
    }
    CBOOL__LASTUNIFY(cdr,X(ci));
  }
}

CBOOL__PROTO(prolog_atom_length) {
  ERR__FUNCTOR("atomic_basic:atom_length", 2);

  DEREF(X(0),X(0));
  if (!TaggedIsATM(X(0))) {
    ERROR_IN_ARG(X(0),1,STRICT_ATOM);
  }

  DEREF(X(1),X(1));
  if (IsInteger(X(1)) || IsVar(X(1))) {
    CBOOL__LASTUNIFY(MakeSmall(GetAtomLen(X(0))),X(1));
  } else {
    BUILTIN_ERROR(TYPE_ERROR(INTEGER),X(1),2);
    CBOOL__FAIL;
  }
}

/* sub_atom(Atom, Before, Lenght, Sub_atom) */
CBOOL__PROTO(prolog_sub_atom) {
  ERR__FUNCTOR("atomic_basic:sub_atom", 4);
  char *s, *s1;
  intmach_t l, b, atom_length;

  DEREF(X(0),X(0));
  if (!TaggedIsATM(X(0))) {
    ERROR_IN_ARG(X(0),1,STRICT_ATOM);
  }
  DEREF(X(1),X(1));
  if (!IsInteger(X(1))) {
    ERROR_IN_ARG(X(1),2,INTEGER);
  }
  DEREF(X(2),X(2));
  if (!IsInteger(X(2))) {
    ERROR_IN_ARG(X(2),3,INTEGER);
  }

  s = GetString(X(0));
  l = GetAtomLen(X(0));

  b = TaggedToIntmach(X(1));
  CBOOL__TEST(b >= 0 && b <= l);

  atom_length = TaggedToIntmach(X(2));
  CBOOL__TEST(atom_length >= 0 && atom_length+b <= l);

  s += b;

  GET_ATOM_BUFFER(s1, atom_length+1);

  strncpy(s1, s, atom_length);

  *(s1+atom_length) = '\0';

  CBOOL__LASTUNIFY(GET_ATOM(Atom_Buffer),X(3));
}

extern try_node_t *address_nd_atom_concat;
CBOOL__PROTO(nd_atom_concat);

CBOOL__PROTO(prolog_atom_concat) {
  ERR__FUNCTOR("atomic_basic:atom_concat", 3);
  intmach_t new_atom_length;
  char *s, *s1, *s2;

  DEREF(X(0),X(0));
  DEREF(X(1),X(1));
  DEREF(X(2),X(2));

  if (TaggedIsATM(X(0))) {
    s1 = GetString(X(0));

    if (TaggedIsATM(X(1))) {

      if (!TaggedIsATM(X(2)) && !IsVar(X(2)))
        BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(2),3);
      /* atom_concat(+, +, ?) */
      s2 = GetString(X(1));

      new_atom_length = GetAtomLen(X(0)) + GetAtomLen(X(1)) + 1;

      GET_ATOM_BUFFER(s, new_atom_length);

      /* Append the two strings in atom_buffer */
      while (*s1)
        *s++ = *s1++;
      while (*s2)
        *s++ = *s2++;
      *s = '\0';
      CBOOL__LASTUNIFY(GET_ATOM(Atom_Buffer),X(2));

    } else if (IsVar(X(1))) {
      if (!TaggedIsATM(X(2))) ERROR_IN_ARG(X(2),3,STRICT_ATOM);
      /* todo: replace unsigned char * to char * when possible */
      /* atom_concat(+, -, +) */
      s2 = GetString(X(2));

      new_atom_length = GetAtomLen(X(2))+1;

      for ( ; *s1 != 0 && *s2 != 0; s1++, s2++) {
        CBOOL__TEST(*s1 == *s2);
      }
      CBOOL__TEST(*s1 == 0);

      GET_ATOM_BUFFER(s, new_atom_length);

      strcpy(s, s2);

      CBOOL__LASTUNIFY(GET_ATOM(Atom_Buffer),X(1));
    } else {
      BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(1),2);
    }
  } else if (IsVar(X(0))) {
    if (!TaggedIsATM(X(2))) { ERROR_IN_ARG(X(2),3,STRICT_ATOM); }

    if (TaggedIsATM(X(1))) {
      /* atom_concat(-, +, +) */
      s1 = GetString(X(1));
      s2 = GetString(X(2));

      new_atom_length = GetAtomLen(X(2)) - GetAtomLen(X(1));
      CBOOL__TEST(new_atom_length >= 0);

      s = s2+new_atom_length;

      CBOOL__TEST(strcmp(s1, s) == 0); /* equal */

      GET_ATOM_BUFFER(s, new_atom_length+1);

      strncpy(s, s2, new_atom_length);

      *(s+new_atom_length) = '\0';

      CBOOL__LASTUNIFY(GET_ATOM(Atom_Buffer),X(0));
    } else if (IsVar(X(1))) {
      /* atom_concat(-, -, +) */
      s2 = GetString(X(2));
      new_atom_length = GetAtomLen(X(2))+1;
      GET_ATOM_BUFFER(s, new_atom_length); // 's' is unused
      X(3) = TaggedZero;
      CVOID__CALL_N(push_choicept,address_nd_atom_concat);
      CBOOL__LASTCALL(nd_atom_concat);
    } else {
      BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(1),2);
    }
  } else {
    BUILTIN_ERROR(TYPE_ERROR(STRICT_ATOM),X(0),1);
  }
}

/*
   Support for the builtin C-predicate atom_concat/3 (see term_support.c)
*/

CBOOL__PROTO(nd_atom_concat) {
  intmach_t i = GetSmall(X(3));
  unsigned char *s, *s1, *s2;

  w->choice->x[3] = SmallAdd(w->choice->x[3], 1);

  s2 = (unsigned char *)GetString(X(2));

  s = (unsigned char *)Atom_Buffer;

  s1 = s2+i;
  strcpy((char *)s, (char *)s1);
  CBOOL__UnifyCons(GET_ATOM(Atom_Buffer),X(1));

  strcpy((char *)s, (char *)s2);
  *(s+i) = '\0';
  CBOOL__UnifyCons(GET_ATOM(Atom_Buffer),X(0));

  if (i == strlen((char *)s2)) {
    CVOID__CALL(pop_choicept);
  }
  
  CBOOL__PROCEED;
}

