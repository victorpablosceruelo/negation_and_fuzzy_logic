#include <engine/basiccontrol.native.h>

/* Bignum arithmetics by Torbjorn Granlund, Johan Andersson, and Mats Carlsson
   Ref: Knuth vol. 2 sec. 4.3.1
*/  

/* TODO: this code contains lots of duplicated code, use macros */ 

#define BNSIZE (sizeof(bignum_t)*8)
#define HalfUnit (BNSIZE>>1)
#define HalfMask (BNMAX>>HalfUnit) /* 0x0000ffff */
#define BNMIN ((bignum_t)0) /* 0x0 */
#define BNMAX (~BNMIN) /* 0xffffffff */
#define BignumRawLength(b) (*(functor_t *)(b))
#define BignumSetRawLength(b,v) ((*(functor_t *)(b)) = (v))
#define BignumLength(b) FunctorBignumValue(BignumRawLength(b))
#define BignumSetLength(b,l) BignumSetRawLength(b, BlobFunctorBignum(l))

/* Access functions to bignum units (index 1 is the first element) */
#define BnStart (sizeof(functor_t)-sizeof(bignum_t))
#define Bn(X,N) (*((bignum_t *)((char *)(X)+BnStart+(N)*sizeof(bignum_t))))
/* Access functions to bignum half units (index 0 is the first element) */
#define Bh(X,N) (*((bignum_half_t *)((char *)(X)+(N)*sizeof(bignum_half_t))))

#if BIGENDIAN
#define BignumHalf(p,i) (((bignum_half_t *)((char *)(p)+BnStart))[((i)+1)^1])
#else
#define BignumHalf(p,i) (((bignum_half_t *)((char *)(p)+BnStart))[(i)+1])
#endif

#define BignumToIntmach(p) ((uintmach_t)BignumHalf(p,1) + (((uintmach_t)BignumHalf(p,2))<<HalfUnit))

#define BignumPositive(b1) WordPositive(Bn((b1), BignumLength(b1)))
#define WordPositive(w) ((signed_bignum_t)(w)>=0)

#define BignumCheck(p,l,zmax) { \
  if (((char *)(p))+(l)*sizeof(bignum_t)+2*sizeof(functor_t) > (char *)(zmax)) return (l)+2*sizeof(functor_t)/sizeof(bignum_t); \
  BignumSetLength((p),(l)); \
}

/* local declarations */
static void bn_negate(bignum_t *x);
static void bn_canonize(bignum_t *x);
static void bn_mult_knuth(bignum_t *x, intmach_t xlen, bignum_t *y, intmach_t ylen, bignum_t *z);
static intmach_t bn_div_mod_quot_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);
static intmach_t bn_div_mod_quot_not_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax);

/* Only intended to be used outside this file -- e.g., by routines
   located at ciao_prolog.c */

intmach_t bn_length(bignum_t *x) {
  return BignumLength(x);
}

bool_t bn_positive(bignum_t *x) {
  return BignumPositive(x);
}

/* Pre: the result must fit in x.
   +x might occupy one more word than -x */
static void bn_negate(bignum_t *x) {
  intmach_t i, k;
  intmach_t xlen;

  xlen = BignumLength(x);
  for (k=1,i=1; i <= xlen; i++)  {
    Bn(x,i) = ~Bn(x,i)+k;
    if (k && Bn(x,i) != BNMIN) k=0;
  }
}

static void bn_canonize(bignum_t *x) {
  intmach_t xlen;
  intmach_t i;

  xlen = BignumLength(x);
  if (BignumPositive(x)) {
    for (i=xlen; i > 1 && Bn(x,i)==BNMIN && WordPositive(Bn(x,i-1)); i--);
  } else {
    for (i=xlen; i > 1 && Bn(x,i)==BNMAX && !WordPositive(Bn(x,i-1)); i--);
  }

  BignumSetLength(x,i);
}

intmach_t bn_add(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  intmach_t i;
  bignum_t *w, xi, wi;
  bignum_t sign_extension; 
  intmach_t min, max;
  bool_t xs, ys;
  
  xs = BignumPositive(x);
  ys = BignumPositive(y);
  if (BignumLength(x) > BignumLength(y)) {
    min = BignumLength(y);
    max = BignumLength(x);
    sign_extension = ys-1;
    w = x;
  } else {
    min = BignumLength(x);
    max = BignumLength(y);
    sign_extension = xs-1;
    w = y;
  }
  
  BignumCheck(z,max+1,zmax);

  i=0;
 add_no_carry:
  for (i++; i<=min; i++) {
    xi = Bn(x,i);
    Bn(z,i) = xi+Bn(y,i);
    if (xi > Bn(z,i)) goto add_with_carry;
  }

  i--;
 se_no_carry:
  for (i++; i<=max; i++) {
    wi = Bn(w,i);
    Bn(z,i) = wi+sign_extension;
    if (Bn(z,i) < wi) goto se_with_carry;
  }
  goto check_sign_overflow;

 add_with_carry:
  for (i++; i<=min; i++) {
    xi = Bn(x,i);
    Bn(z,i) = xi+Bn(y,i)+1;
    if (Bn(z,i) > xi) goto add_no_carry;
  }

  i--;
 se_with_carry:
  for (i++; i<=max; i++) {
    wi = Bn(w,i);
    Bn(z,i) = wi+sign_extension+1;
    if (Bn(z,i) > wi) goto se_no_carry;
  }
  
 check_sign_overflow:
  if (xs == ys) {
    Bn(z,max+1) = xs-1;
  } else {
    Bn(z,max+1) = WordPositive(Bn(z,max))-1;
  }
  
  bn_canonize(z);  
  return 0;
}

intmach_t bn_incr(bignum_t *x, bignum_t *z, bignum_t *zmax) {
  intmach_t i, k;
  intmach_t xlen;
  intmach_t max;
  
  xlen = BignumLength(x);
  max = xlen+BignumPositive(x);
  BignumCheck(z,max,zmax);
  
  for (i=1, k=1; i<=xlen; i++) {
    if ((Bn(z,i)=Bn(x,i)+k)) k=0;
  }
  
  if (BignumPositive(x)) {
    Bn(z,max) = BNMIN+k;
  }
  
  bn_canonize(z);
  return 0;
}

intmach_t bn_plus(bignum_t *x, bignum_t *z, bignum_t *zmax) {
  intmach_t xlen;
  intmach_t i;
  
  xlen = BignumLength(x);
  BignumCheck(z,xlen,zmax);
  for (i=1; i<=xlen; i++) {
    Bn(z,i) = Bn(x,i);
  }

  return 0;
}

intmach_t bn_subtract(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  intmach_t i;
  bignum_t *w, xi;
  bignum_t sign_extension;
  intmach_t min, max;
  bool_t xs, ys;

  xs = BignumPositive(x);
  ys = BignumPositive(y);
  if (BignumLength(x) > BignumLength(y)) {
    min = BignumLength(y);
    max = BignumLength(x);
    sign_extension= ys-1;
    w = x;
  } else {
    min = BignumLength(x);
    max = BignumLength(y);
    sign_extension = xs-1;
    w = y;
  }

  BignumCheck(z,max+1,zmax);

  i=0;
 subtract_no_carry:
  for (i++; i<=min; i++) {
    xi = Bn(x,i);
    Bn(z,i) = xi-Bn(y,i);
    if (Bn(z,i) > xi) goto subtract_with_carry;
  }

  i--;
 se_no_carry:
  for (i++; i<=max; i++) {
    if (x==w) {
      xi = Bn(x,i);
      Bn(z,i) = xi-sign_extension;
      if (Bn(z,i) > xi) goto se_with_carry;
    } else {
      Bn(z,i) = sign_extension-Bn(y,i);
      if (Bn(z,i) > sign_extension) goto se_with_carry;
    }
  }
  goto check_sign_overflow;
  
 subtract_with_carry:
  for (i++; i<=min; i++) {
    xi = Bn(x,i);
    Bn(z,i) = xi-Bn(y,i)-1;
    if (xi > Bn(z,i)) goto subtract_no_carry;
  }

  i--;
 se_with_carry:
  for (i++; i<=max; i++) {
    if (x==w) {
      xi = Bn(x,i);
      Bn(z,i) = xi-sign_extension-1;
      if (xi > Bn(z,i)) goto se_no_carry;
    } else {
      Bn(z,i) = sign_extension-Bn(y,i)-1;
      if (sign_extension > Bn(z,i)) goto se_no_carry;
    }
  }
  
 check_sign_overflow:
  if (xs != ys) {
    Bn(z,max+1) = xs-1;
  } else {
    Bn(z,max+1) = WordPositive(Bn(z,max))-1;
  }
  
  bn_canonize(z);
  return 0;
}

intmach_t bn_decr(bignum_t *x, bignum_t *z, bignum_t *zmax) {
  intmach_t i, k;
  intmach_t xlen;
  intmach_t max;
  
  xlen = BignumLength(x);
  max = xlen+(!BignumPositive(x));
  BignumCheck(z,max,zmax);
  
  for (i=1, k=1; i<=xlen; i++) {
    if (~(Bn(z,i)=Bn(x,i)-k)) k=0;
  }
  
  if (!BignumPositive(x)) {
    Bn(z,max) = BNMAX;
  }
  
  bn_canonize(z);
  return 0;
}

intmach_t bn_minus(bignum_t *x, bignum_t *z, bignum_t *zmax) {
  intmach_t xlen;
  intmach_t i;
  
  xlen = BignumLength(x);
  if (BignumPositive(x)) {
    BignumCheck(z,xlen,zmax);
    for (i=1; i<=xlen; i++) {
      Bn(z,i) = Bn(x,i);
    }
  } else {
    BignumCheck(z,xlen+1,zmax);
    for (i=1; i<=xlen; i++) {
      Bn(z,i) = Bn(x,i);
    }
    Bn(z,xlen+1) = BNMAX;
  }
  bn_negate(z);
  bn_canonize(z);
  return 0;
}

intmach_t bn_and(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  intmach_t i;
  intmach_t min, max;
  bignum_t mask;
  
  if (BignumLength(x) > BignumLength(y)) {
    min = BignumLength(y);
    max = BignumLength(x);
    mask = BignumPositive(y)-1;
  } else {
    bignum_t *temp;
    min = BignumLength(x);
    max = BignumLength(y);
    mask = BignumPositive(x)-1;
    temp = x;
    x = y;
    y = temp;
  }

  BignumCheck(z,max,zmax);
  
  for (i = 1; i <= min; i++) {
    Bn(z,i) = Bn(x,i)&Bn(y,i);
  }
  
  for (; i <= max; i++) {
    Bn(z,i) = Bn(x,i)&mask;
  }
  
  bn_canonize(z);
  return 0;
}

intmach_t bn_or(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  intmach_t i;
  intmach_t min, max;
  bignum_t mask;
  
  if (BignumLength(x) > BignumLength(y)) {
    min = BignumLength(y);
    max = BignumLength(x);
    mask = BignumPositive(y)-1;
  } else {
    bignum_t *temp;
    min = BignumLength(x);
    max = BignumLength(y);
    mask = BignumPositive(x)-1;
    temp = x;
    x = y;
    y = temp;
  }

  BignumCheck(z,max,zmax);
  
  for (i = 1; i <= min; i++) {
    Bn(z,i) = Bn(x,i)|Bn(y,i);
  }
  
  for (; i <= max; i++) {
    Bn(z,i) = Bn(x,i)|mask;
  }
  
  bn_canonize(z);
  return 0;
}

intmach_t bn_xor(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  intmach_t i;
  intmach_t min, max;
  bignum_t mask;
  
  if (BignumLength(x) > BignumLength(y)) {
    min= BignumLength(y);
    max= BignumLength(x);
    mask= BignumPositive(y)-1;
  } else {
    bignum_t *temp;
    min= BignumLength(x);
    max= BignumLength(y);
    mask= BignumPositive(x)-1;
    temp = x;
    x = y;
    y = temp;
  }

  BignumCheck(z,max,zmax);
  
  for (i= 1; i <= min; i++) {
    Bn(z,i) = Bn(x,i)^Bn(y,i);
  }
  
  for (; i <= max; i++) {
    Bn(z,i) = Bn(x,i)^mask;
  }
  
  bn_canonize(z);
  return 0;
}

intmach_t bn_not(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  intmach_t i;
  intmach_t xlen;

  xlen = BignumLength(x);
  BignumCheck(z,xlen,zmax);
  
  for (i= 1; i <= xlen; i++) {
    Bn(z,i) = ~Bn(x,i);
  }
  
  /* z must be canonical now */
  return 0;
}

/* TODO: pass dist as intmach_t instead of bignum_t, implement
   bn_call_bignum_int */
intmach_t bn_lshift(bignum_t *x, bignum_t *dist, bignum_t *z, bignum_t *zmax) {
  bignum_t xi;
  bool_t xs;
  intmach_t xlen;
  intmach_t shift;
  intmach_t div, rem;
  intmach_t i;

  xs = BignumPositive(x);
  xlen = BignumLength(x);
  shift = BignumToIntmach(dist);
  
  div = shift >> 5;
  rem = shift &  0x1f;

  BignumCheck(z,xlen+div+(rem>0),zmax);
  
  /* first perform the `long' shift, if any */
  for (i=1; i<=div; i++) {
    Bn(z,i) = BNMIN;
  }
  
  /* Then perform the short shift */
  if (rem == 0) { /* copy */
    for (i=1; i <= xlen; i++) {
      Bn(z,div+i) = Bn(x,i);
    }
  } else {
    intmach_t mer=BNSIZE-rem;
    bignum_t carry=0;
      
    for (i=1; i<=xlen; i++) {
      xi = Bn(x,i);
      Bn(z,div+i) = xi<<rem | carry;
      carry = xi>>mer;
    }
    Bn(z,div+i) = (xs-1)<<rem | carry;
  }
  
  bn_canonize(z);
  return 0;
}

/* TODO: pass dist as intmach_t instead of bignum_t, implement
   bn_call_bignum_int */
intmach_t bn_rshift(bignum_t *x, bignum_t *dist, bignum_t *z, bignum_t *zmax) {
  bignum_t xi;
  bool_t xs;
  intmach_t xlen;
  intmach_t shift;
  intmach_t div, rem;
  intmach_t i;

  xs = BignumPositive(x);
  xlen = BignumLength(x);
  shift = BignumToIntmach(dist);

  div = shift >> 5;
  rem = shift &  0x1f;

  if (xlen-div<1) {
    BignumCheck(z,1,zmax);
    Bn(z,1) = xs-1;
    return 0;
  }

  BignumCheck(z,xlen-div,zmax);
  
  if (rem==0) {
    for (i=xlen-div; i>=1; i--) {
      Bn(z,i) = Bn(x,div+i);
    }
  } else {
    intmach_t mer=BNSIZE-rem;
    bignum_t carry=(xs-1)<<mer;
      
    for (i=xlen-div; i>=1; i--) {
      xi = Bn(x,div+i);
      Bn(z,i) = xi>>rem | carry;
      carry = xi<<mer;
    }
  }
  
  bn_canonize(z);
  return 0;
}

intmach_t bn_compare(bignum_t *x, bignum_t *y) {
  intmach_t xlen, ylen;
  bool_t xs, ys;

  xlen = BignumLength(x);
  ylen = BignumLength(y);
  xs = BignumPositive(x);
  ys = BignumPositive(y);

  if (xs != ys) {
    return (xs ? 1 : -1);
  } else if (xlen != ylen) {
    return (xs^(xlen>ylen) ? -1 : 1);
  } else {
    intmach_t i=xlen+1;
    while (--i) {
      if (Bn(x,i)!=Bn(y,i)) return (Bn(x,i)<Bn(y,i) ? -1 : 1);
    }
    return 0;
  } 
}

/* y is shorter than x */
static void bn_mult_knuth(bignum_t *x, intmach_t xlen,
			  bignum_t *y, intmach_t ylen,
			  bignum_t *z) {
  intmach_t i, j;
  bignum_half_t yj;

  for (i=1; i<=xlen+ylen; i++) Bn(z,i) = 0;

  xlen <<= 1;
  ylen <<= 1;
  for (j=1; j<=ylen; j++) {
    if ((yj=BignumHalf(y,j))) {
      bignum_t t=0;
	
      for (i=1; i<=xlen; i++) {
	t = BignumHalf(z,i+j-1) + BignumHalf(x,i)*yj + (t>>HalfUnit);
	BignumHalf(z,i+j-1) = t&HalfMask;
      }
      BignumHalf(z,xlen+j) = (t>>HalfUnit);
    }
  }
}


intmach_t bn_multiply(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  bool_t sx;
  bool_t sy;
  intmach_t xlen;
  intmach_t ylen;

  sx = BignumPositive(x);
  sy = BignumPositive(y);
  xlen = BignumLength(x);
  ylen = BignumLength(y);

  BignumCheck(z,xlen+ylen,zmax);
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  if (xlen>ylen) {
    bn_mult_knuth(x,xlen,y,ylen,z);
  } else {
    bn_mult_knuth(y,ylen,x,xlen,z);
  }
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  if (sx^sy) bn_negate(z);
  bn_canonize(z); 
  return 0;
}

static intmach_t bn_div_mod_quot_wanted(bignum_t *x, bignum_t *y, bignum_t *z, bignum_t *zmax) {
  intmach_t d;
  intmach_t xlen;
  intmach_t ylen;
  intmach_t ulen, vlen;
  intmach_t i, j, k;
  bignum_t carry, v1qhat, v2qhat;
  bignum_half_t
		 *u,		/* dividend, size=ulen+1 */
                 *v,		/* divisor, size=vlen */
                 *q;		/* quotient, size=ulen-vlen+1 */
  bignum_half_t v1, v2, qj;

  xlen = BignumLength(x);
  ylen = BignumLength(y);
  ulen=xlen<<1;
  vlen=ylen<<1;
  
  while (!BignumHalf(x,ulen) && ulen>1) ulen--;
  while (!BignumHalf(y,vlen) && vlen>1) vlen--;
  if (vlen>ulen) {
    BignumCheck(z,1,zmax);
    Bn(z,1) = 0;
    return 0;
  } else if (vlen==1) {                      /* special case: a simple loop */
    v1 = BignumHalf(y,1);
    for (carry=0, i=ulen; i>0; i--) {
      carry += BignumHalf(x,i);
      BignumHalf(z,i) = carry/v1;
      carry = (carry%v1)<<HalfUnit;
    }
    BignumCheck(z,(ulen+2)>>1,zmax);
    BignumHalf(z,ulen+1) = 0;
    BignumHalf(z,ulen+2) = 0;
    return 0;
  }
  BignumCheck(z,(3*ulen+4)>>1,zmax);
  v = (bignum_half_t *)z+ulen+2;
  u = v+vlen;
  q = u+ulen+2;
				/* Normalize. */
  v1 = BignumHalf(y,vlen);
  for (d=0; v1 < (bignum_t)1<<(HalfUnit-1); d++) {
    v1 <<= 1;
  }
  for (carry=0, i=0; i<ulen; i++) {
    carry += BignumHalf(x,i+1)<<d;
    Bh(u,i) = carry & HalfMask;
    carry >>= HalfUnit;
  }
  Bh(u,ulen) = carry;
  for (carry=0, i=0; i<vlen; i++) {
    carry += BignumHalf(y,i+1)<<d;
    Bh(v,i) = carry & HalfMask;
    carry >>= HalfUnit;
  }
  
  v1 = Bh(v,vlen-1), v2 = Bh(v,vlen-2);
  for (j=ulen; j>=vlen; j--) {

    /* Calculate Bn(q,j). */
    carry = (Bh(u,j)<<HalfUnit) + Bh(u,j-1);
    if (Bh(u,j)==v1) {
      qj = HalfMask;
    } else {
      qj = carry/v1;
    }
      
    v1qhat = v1*qj;
    v2qhat = v2*qj;
    while (carry-v1qhat < (1<<HalfUnit) &&
	   v2qhat > ((carry-v1qhat)<<HalfUnit)+Bh(u,j-2)) {
      qj--;
      v1qhat-=v1;
      v2qhat-=v2;
    }

    /* Multiply and subtract. */
    if ((Bn(q,j)=qj)) {
      for (carry=0, i=0, k=j-vlen; i<vlen-2; i++, k++) {
	carry = Bh(u,k) - Bh(v,i)*qj - carry;
	Bh(u,k) = carry & HalfMask;
	carry = (Bh(u,k)-carry)>>HalfUnit;
      }
      carry = Bh(u,k) - v2qhat - carry;
      Bh(u,k) = carry & HalfMask;
      carry = (Bh(u,k)-carry)>>HalfUnit;
      k++;
      carry = Bh(u,k) - v1qhat - carry;
      Bh(u,k) = carry & HalfMask;
      carry = (Bh(u,k)-carry)>>HalfUnit;
      carry = Bh(u,j) - carry;
      Bh(u,j) = carry & HalfMask;
      carry = (Bh(u,j)-carry)>>HalfUnit;
      if (carry) {
	Bn(q,j)--;
	for (carry=0, i=0, k=j-vlen; i<vlen; i++, k++) {
	  carry += Bh(u,k) + Bh(v,i);
	  Bh(u,k) = carry & HalfMask;
	  carry = carry>>HalfUnit;
	}
	Bh(u,j) += carry;
      }
    }
  }
  /* Bn(q,vlen .. ulen) is the desired quotient. */
  BignumSetLength(z,(ulen-vlen+3)>>1);
  for (i=1, k=vlen; k<=ulen; i++, k++) {
    BignumHalf(z,i) = Bn(q,k);
  }
  BignumHalf(z,i++) = 0;
  BignumHalf(z,i++) = 0;
  return 0;
}


static intmach_t bn_div_mod_quot_not_wanted(bignum_t *x, bignum_t *y, bignum_t *z,
				      bignum_t *zmax) {
  intmach_t d;
  intmach_t xlen;
  intmach_t ylen;
  intmach_t ulen, vlen;
  intmach_t i, j, k;
  bignum_t carry, v1qhat, v2qhat;
  bignum_half_t
		 *u,		/* dividend, size=ulen+1 */
                 *v,		/* divisor, size=vlen */
                 *q;		/* quotient, size=ulen-vlen+1 */
  bignum_half_t v1, v2, qj;
  
  xlen = BignumLength(x);
  ylen = BignumLength(y);
  ulen=xlen<<1;
  vlen=ylen<<1;
  while (!BignumHalf(x,ulen) && ulen>1) ulen--;
  while (!BignumHalf(y,vlen) && vlen>1) vlen--;
  if (vlen>ulen) {
    BignumCheck(z,xlen,zmax);
    for (i=1; i<=xlen; i++) {
      Bn(z,i) = Bn(x,i);
    }
    return 0;
  } else if (vlen==1) {
    /* special case: a simple loop */
    v1 = BignumHalf(y,1);
    for (carry=0, i=ulen; i>0; i--) {
      carry += BignumHalf(x,i);
      BignumHalf(z,i) = carry/v1;
      carry = (carry%v1)<<HalfUnit;
    }
    BignumCheck(z,1,zmax);
    Bn(z,1) = carry>>HalfUnit;
    return 0;
  }
  BignumCheck(z,(3*ulen+4)>>1,zmax);
  v = (bignum_half_t *)z+ulen+2;
  u = v+vlen;
  q = u+ulen+1;

  /* Normalize. */
  v1 = BignumHalf(y,vlen);
  for (d=0; v1 < (bignum_t)1<<(HalfUnit-1); d++) {
    v1 <<= 1;
  }
  for (carry=0, i=0; i<ulen; i++) {
    carry += BignumHalf(x,i+1)<<d;
    Bh(u,i) = carry & HalfMask;
    carry >>= HalfUnit;
  }
  Bh(u,ulen) = carry;
  for (carry=0, i=0; i<vlen; i++) {
    carry += BignumHalf(y,i+1)<<d;
    Bh(v,i) = carry & HalfMask;
    carry >>= HalfUnit;
  }
  
  v1 = Bh(v,vlen-1), v2 = Bh(v,vlen-2);
  for (j=ulen; j>=vlen; j--) {
    /* Calculate Bh(q,j). */
    carry = (Bh(u,j)<<HalfUnit) + Bh(u,j-1);
    if (Bh(u,j)==v1) {
      qj = HalfMask;
    } else {
      qj = carry/v1;
    }
      
    v1qhat = v1*qj;
    v2qhat = v2*qj;
    while (carry-v1qhat < (1<<HalfUnit) &&
	   v2qhat > ((carry-v1qhat)<<HalfUnit)+Bh(u,j-2)) {
      qj--;
      v1qhat-=v1;
      v2qhat-=v2;
    }

    /* Multiply and subtract. */
    if ((Bh(q,j)=qj)) {
      for (carry=0, i=0, k=j-vlen; i<vlen-2; i++, k++) {
	carry = Bh(u,k) - Bh(v,i)*qj - carry;
	Bh(u,k) = carry & HalfMask;
	carry = (Bh(u,k)-carry)>>HalfUnit;
      }
      carry = Bh(u,k) - v2qhat - carry;
      Bh(u,k) = carry & HalfMask;
      carry = (Bh(u,k)-carry)>>HalfUnit;
      k++;
      carry = Bh(u,k) - v1qhat - carry;
      Bh(u,k) = carry & HalfMask;
      carry = (Bh(u,k)-carry)>>HalfUnit;
      carry = Bh(u,j) - carry;
      Bh(u,j) = carry & HalfMask;
      carry = (Bh(u,j)-carry)>>HalfUnit;
      if (carry) {
	Bh(q,j)--;
	for (carry=0, i=0, k=j-vlen; i<vlen; i++, k++) {
	  carry += Bh(u,k) + Bh(v,i);
	  Bh(u,k) = carry & HalfMask;
	  carry = carry>>HalfUnit;
	}
	Bh(u,j) += carry;
      }
    }
  }
  /* Bh(u,0 .. vlen-1)>>d is the desired remainder. */
  BignumSetLength(z,(vlen+2)>>1);
  v1qhat = (1<<d)-1;
  for (carry=0, i=vlen; i>0; i--) {
    carry += Bh(u,i-1);
    BignumHalf(z,i) = carry>>d;
    carry = (carry&v1qhat)<<HalfUnit;
  }
  BignumHalf(z,vlen+1) = 0;
  BignumHalf(z,vlen+2) = 0;
  return 0;
}

intmach_t bn_quotient_remainder_quot_wanted(bignum_t *x, bignum_t *y,
				      bignum_t *z, bignum_t *zmax) {
  bool_t sx;
  bool_t sy;
  intmach_t value;
  
  sx = BignumPositive(x);
  sy = BignumPositive(y);
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  value = bn_div_mod_quot_wanted(x,y,z,zmax);
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  if (!value) {
    if (sx^sy) {
      bn_negate(z);
    }
    bn_canonize(z);
  }
  return value;
}

intmach_t bn_quotient_remainder_quot_not_wanted(bignum_t *x, bignum_t *y,
					  bignum_t *z, bignum_t *zmax) {
  bool_t sx;
  bool_t sy;
  intmach_t value;
  
  sx = BignumPositive(x);
  sy = BignumPositive(y);
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  value = bn_div_mod_quot_not_wanted(x,y,z,zmax);
  if (!sx) bn_negate(x);
  if (!sy) bn_negate(y);
  if (!value) {
    if (!sx) {
      bn_negate(z);
    }
    bn_canonize(z);
  }
  return value;
}

intmach_t bn_from_float(flt64_t f, bignum_t *z, bignum_t *zmax) {
  union {
    int32_t fp[2];
    flt64_t f;
  } u;
  /* TODO: use const or define macros */
  flt64_t norm = 4294967296.0;	/* 2**32 */
  flt64_t norm2 = norm*norm;
  intmach_t i, exp, div, rem, zlen;
  bool_t sx;
  bignum_t uhigh, ulow;

  u.f = f;
#if defined(sun)
  if (u.fp[1-BIGENDIAN] & 0x80000000)
#else
  if (u.f < 0.0 ||
      (u.f == 0.0 && (u.fp[0]|u.fp[1]) != 0) ||
      (u.fp[0]&u.fp[1]) == 0xffffffff)
#endif
    {
      sx = FALSE;
      u.f = -u.f;
    } else {
      sx = TRUE;
    }
  
  if (u.f < 1.0) {
    BignumCheck(z,1,zmax);
    Bn(z,1) = 0;
    goto ret;
  } else if (u.f != u.f || u.f == u.f/2.0) {
    /* catch NaN, Infinity */
    return -1;
  }
  
  /* normalize */
  exp = 64;
  while (u.f >= norm2) {
    exp+=64; u.f /= norm2;
  }
  norm2 /= 2.0;
  while (u.f < norm2) {
    --exp;
    u.f *= 2.0;
  }

  /* ensure there is room for sign bit */
  zlen = ((exp-1)>>5)+2;
  div = (exp-64)>>5;
  rem = exp & 0x1f;
  BignumCheck(z,zlen,zmax);
  for (i=1; i<=zlen; i++) Bn(z,i) = 0;

  /* turn off high bit of uhigh since it causes trouble on certain
     machines */
  u.f -= norm2;
  uhigh = u.f/norm;
  ulow = u.f-norm*uhigh;
  uhigh += 0x80000000;
  if (rem==0) {
    if (div>=0) Bn(z,div+1) = ulow;
    if (div>=-1) Bn(z,div+2) = uhigh;
  } else {
    if (div>=0) Bn(z,div+1) = ulow<<rem;
    if (div>=-1) Bn(z,div+2) = (ulow>>(BNSIZE-rem))+(uhigh<<rem);
    if (div>=-2) Bn(z,div+3) = uhigh>>(BNSIZE-rem);
  }
 ret:
  if (!sx) bn_negate(z);
  bn_canonize(z);
  return 0;
}

flt64_t bn_to_float(bignum_t *bn) {
  intmach_t i;
  flt64_t f;
  /* todo[ts]: modify if sizeof(bignum_t) != sizeof(int32_t) */
  i = BignumLength(bn);
  f = (int32_t)Bn(bn,i);
  while (i > 1) {
    uint32_t u;
    i--;
    u = Bn(bn,i);
    if (u & 0x80000000) { /* trouble on some machines */
      f = f*4294967296.0 + 2147483648.0 + (u - 0x80000000);
    } else {
      f = f*4294967296.0 + u;
    }
  }
  return f;
}

/* Pre: x is a syntactically correct string denoting an integer */
intmach_t bn_from_string(char *x, bignum_t *z, bignum_t *zmax, intmach_t base) {
  bool_t sx;
  intmach_t j;
  intmach_t zlen;
  char cur;
  bignum_t t, digit;

  /* Get sign */
  if (*x=='+') {
    x++;
    sx = TRUE;
  } else if (*x=='-') {
    x++;
    sx = FALSE;
  } else {
    sx = TRUE;
  }

  zlen = 2;
  BignumCheck(z,1,zmax);
  Bn(z,1) = 0; /* always keep a zero pad word */
  for (cur = *x++; cur; cur = *x++) {
    digit = (cur>='a' ? cur-'a'+10 : cur>='A' ? cur-'A'+10 : cur-'0');
    for (j=1; j<zlen; j++) {
      t = base*BignumHalf(z,j) + digit;
      BignumHalf(z,j) = t&HalfMask;
      digit = t>>HalfUnit;
    }
    if (digit) {
      zlen += 2;
      BignumCheck(z,zlen>>1,zmax);
      BignumHalf(z,j) = digit;
      BignumHalf(z,j+1) = 0;
      BignumHalf(z,j+2) = 0;
      BignumHalf(z,j+3) = 0;
    }
  }
  if (!sx) bn_negate(z);
  bn_canonize(z);
  return 0;
}

CVOID__PROTO_N(bn_to_string, bignum_t *x, intmach_t base) {
  intmach_t j, k;
  intmach_t xlen, slen, alen, dlen;
  bignum_t r, digit, divisor;
  bool_t sx;
  char hibase;
  bignum_half_t *work;
  char *c0, *c, d;

  if (base<0) {
    hibase = 'A'-10;
    base = -base;
  } else {
    hibase = 'a'-10;
  }
  xlen = BignumLength(x)<<1;

  sx = BignumPositive(x);

  /* compute divisor = base**N such that divisor <= 1<<16. */
  r = (1<<HalfUnit)/base;
  for (dlen=1, divisor=base; divisor<=r; dlen++) {
    divisor *= base;
  }

  /* string length <= (words+1)*ceiling(digits/word) */
  slen = (((xlen+1)*(dlen+1)) & -4)+4 + (xlen<<1);
  for (alen=Atom_Buffer_Length; slen>alen;) {
    alen <<= 1;
  }
  GET_ATOM_BUFFER(c, alen);
  work = (bignum_half_t *)(c+slen-(xlen<<1));
  if (!sx) {
    *c++ = '-';
    for (k=1, j=0; j<xlen; j++) {
      Bh(work,j) = ~BignumHalf(x,j+1)+k;
      k &= !Bh(work,j);
    }
  } else {
    for (j=0; j<xlen; j++) {
      Bh(work,j) = BignumHalf(x,j+1);
    }
  }
  while (xlen>0 && !Bh(work,xlen-1)) {
    xlen--;
  }

  while (xlen>0) {
    for (j=xlen-1, r=0; j >= 0; j--) {
      digit = (r<<HalfUnit) + Bh(work,j);
      Bh(work,j) = digit/divisor;
      r = digit%divisor;
    }
    for (j=dlen; j>0; j--) {
      digit = r%base;
      *c++ = (digit<10 ? '0'+digit : hibase+digit);
      r /= base;
    } 
    while (xlen>0 && !Bh(work,xlen-1)) {
      xlen--;
    }
  }

  do {
    c--;
  } while (c[0]=='0');
  
  c[1] = 0;
  for (c0=Atom_Buffer+1-sx; c0<c; c0++, c--) {
    d = *c0;
    *c0 = *c;
    *c = d;
  }
}


