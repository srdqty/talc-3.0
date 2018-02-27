
/* type definitions. */
typedef unsigned short int UINT2;
typedef unsigned long int UINT4;
typedef UINT4 NN_DIGIT;
typedef UINT2 NN_HALF_DIGIT;

/* defines */
#define MAX_RSA_MODULUS_BITS 2048
#define MAX_RSA_MODULUS_LEN ((MAX_RSA_MODULUS_BITS + 7) / 8) 
#define NN_DIGIT_BITS 32
#define NN_HALF_DIGIT_BITS 16
/* Length of digit in bytes */
#define NN_DIGIT_LEN (NN_DIGIT_BITS / 8)
/* Maximum length in digits */
#define MAX_NN_DIGITS \
  ((MAX_RSA_MODULUS_LEN + NN_DIGIT_LEN - 1) / NN_DIGIT_LEN + 1)
/* Maximum digits */
#define MAX_NN_DIGIT 0xffffffff
#define MAX_NN_HALF_DIGIT 0xffff
#define LOW_HALF(x) (NN_HALF_DIGIT)((x) & MAX_NN_HALF_DIGIT)
#define HIGH_HALF(x) \
  (NN_HALF_DIGIT)(((x) >> NN_HALF_DIGIT_BITS) & MAX_NN_HALF_DIGIT)
#define TO_HIGH_HALF(x) (((NN_DIGIT)(x)) << NN_HALF_DIGIT_BITS)

#define NN_ASSIGN_DIGIT(a, b, digits) {NN_AssignZero (a, digits); a[0] = b;}

/******************************************************************************
Fred's additions
 ******************************************************************************/

#define POP
// #define STACK_ALLOC
#define BOUNDS_CHECK
#define NN_DIV_STATIC
#define NN_MULT_STATIC
#define NN_MODMULT_STATIC
#define NN_MOD_STATIC

extern void *GC_malloc(int);

typedef struct { int size; NN_DIGIT *elts; } pop_array_s, *pop_array;

int array_bounds_error() {
  printf("array bounds error.\n");
  exit(1);
  return 1;
}

#ifdef POP
#ifdef BOUNDS_CHECK
#define ARRAY_BOUNDS(X,Y) (((X)->size <= (Y)) ? array_bounds_error () : (Y))
#else 
#define ARRAY_BOUNDS(X,Y) (Y)
#endif

#  define UNIVERSAL_DECL pop_array ZZ_ARRAY=0; int ZZ_INDEX=0;
#  define NN_NUM pop_array

#define NN_NUM_KNOWN_STATIC(X,Y) NN_DIGIT X##_elts[Y]; pop_array_s X##_arr = {Y,X##_elts}; pop_array X = &X##_arr

#ifdef STACK_ALLOC
#define NN_NUM_KNOWN(X,Y) NN_NUM_KNOWN_STATIC(X,Y)
#else
#  define NN_NUM_KNOWN(X,Y) pop_array X = ((ZZ_ARRAY = (pop_array)GC_malloc(sizeof(pop_array_s))), \
		   (ZZ_INDEX = Y), \
		   (ZZ_ARRAY->size = ZZ_INDEX), \
		   (ZZ_ARRAY->elts = (NN_DIGIT *)GC_malloc(sizeof(NN_DIGIT)*ZZ_INDEX)),\
		   ZZ_ARRAY)
#endif

/* XXX - Duplicating computation of X and Y,
   Make sure no argument has side effects!!!*/
#define ARR_GET(X,Y) ((X)->elts[ARRAY_BOUNDS(X,Y)])
#define ARR_SET_OP(X,Y,OP,W) (((X)->elts)[ARRAY_BOUNDS(X,Y)] OP W)

#define ARR_SET(X,Y,W) ARR_SET_OP(X,Y,=,W)
#define ARR_SET_SUB(X,Y,W) ARR_SET_OP(X,Y,-=,W)
#define ARR_SET_ADD(X,Y,W) ARR_SET_OP(X,Y,+=,W)

#else
#  define UNIVERSAL_DECL
#  define NN_NUM NN_DIGIT *
#  define NN_NUM_KNOWN(X,Y) NN_DIGIT X[Y]
#  define ARR_GET(X,Y) X[Y]
#  define ARR_SET(X,Y,Z) X[Y] = Z
#  define ARR_SET_SUB(X,Y,Z) X[Y] -= Z
#  define ARR_SET_ADD(X,Y,Z) X[Y] += Z
#endif
/******************************************************************************/

/*****************************************************************************
 * prototypes
 *****************************************************************************/
NN_DIGIT NN_Sub(NN_NUM, NN_NUM, int, NN_NUM, unsigned int);
void NN_DigitMult(NN_DIGIT [2], NN_DIGIT, NN_DIGIT);
NN_DIGIT NN_DigitDiv(NN_NUM, int, NN_DIGIT);
static NN_DIGIT NN_RShift(NN_NUM, NN_NUM, unsigned int, unsigned int);
static void NN_Div (NN_NUM, NN_NUM, 
		    NN_NUM, unsigned int, 
		    NN_NUM, unsigned int);
static NN_DIGIT NN_AddDigitMult(NN_NUM, NN_NUM, int, NN_DIGIT, 
				NN_NUM, unsigned int);
static NN_DIGIT NN_SubDigitMult(NN_NUM, NN_NUM, int, NN_DIGIT, 
				NN_NUM, unsigned int);
static unsigned int NN_DigitBits(NN_DIGIT);

/*****************************************************************************
 * Debugging.
 *****************************************************************************/

void dumpArray(char *s,NN_NUM a, unsigned int digits)
{
  UNIVERSAL_DECL
  unsigned int i;

  printf("; %s [ %d ] ", s, digits);
  for (i = 0; i < digits; i++)
    printf("%d ", ARR_GET(a,i));
  printf("\n");
}

/*****************************************************************************
 * digit.c
 *****************************************************************************/

/* Computes a = b * c, where b and c are digits.

   Lengths: a[2].
 */
void NN_DigitMult (NN_DIGIT a[2], NN_DIGIT b, NN_DIGIT c) {
  NN_DIGIT t, u;
  NN_DIGIT bHigh, bLow, cHigh, cLow;

  bHigh = HIGH_HALF(b);
  bLow  = LOW_HALF(b);

  cHigh = HIGH_HALF(c);
  cLow  = LOW_HALF(c);

  a[0] = (NN_DIGIT)bLow  * (NN_DIGIT)cLow;
  t    = (NN_DIGIT)bLow  * (NN_DIGIT)cHigh;
  u    = (NN_DIGIT)bHigh * (NN_DIGIT)cLow;
  a[1] = (NN_DIGIT)bHigh * (NN_DIGIT)cHigh;
  
  if((t += u) < u)
    a[1] += TO_HIGH_HALF (1);
  u = TO_HIGH_HALF (t);
  
  if ((a[0] += u) < u)
    a[1]++;
  a[1] += HIGH_HALF (t);
}

/* Sets a = b / c, where a and c are digits.

   Lengths: b[2].
   Assumes b[1] < c and HIGH_HALF (c) > 0. For efficiency, c should be
   normalized.
 */
NN_DIGIT NN_DigitDiv (NN_NUM b,int b_start, NN_DIGIT c) {
  UNIVERSAL_DECL
  NN_DIGIT t[2], u, v;
  NN_HALF_DIGIT aHigh, aLow, cHigh, cLow;

  cHigh = HIGH_HALF (c);
  cLow  = LOW_HALF (c);

  t[0] = ARR_GET(b,b_start);
  t[1] = ARR_GET(b,b_start+1);

  /* Underestimate high half of quotient and subtract. */
  if (cHigh == MAX_NN_HALF_DIGIT) 
    aHigh = HIGH_HALF (t[1]);
  else
    aHigh = (NN_HALF_DIGIT)(t[1] / (cHigh + 1));

  u = (NN_DIGIT)aHigh * (NN_DIGIT)cLow;
  v = (NN_DIGIT)aHigh * (NN_DIGIT)cHigh;

  if ((t[0] -= TO_HIGH_HALF (u)) > (MAX_NN_DIGIT - TO_HIGH_HALF (u)))
    t[1]--;

  t[1] -= HIGH_HALF (u);
  t[1] -= v;

  /* Correct estimate. */
  while ((t[1] > cHigh) ||
         ((t[1] == cHigh) && (t[0] >= TO_HIGH_HALF (cLow)))) {
    if ((t[0] -= TO_HIGH_HALF (cLow)) > MAX_NN_DIGIT - TO_HIGH_HALF (cLow)) {
      t[1]--;
    }
    t[1] -= cHigh;
    aHigh++;
  }

  /* Underestimate low half of quotient and subtract. */
  if (cHigh == MAX_NN_HALF_DIGIT)
    aLow = LOW_HALF (t[1]);
  else
    aLow =
      (NN_HALF_DIGIT)
        ((NN_DIGIT)(TO_HIGH_HALF (t[1]) + HIGH_HALF (t[0])) / (cHigh + 1));

  u = (NN_DIGIT)aLow * (NN_DIGIT)cLow;
  v = (NN_DIGIT)aLow * (NN_DIGIT)cHigh;

  if ((t[0] -= u) > (MAX_NN_DIGIT - u))
    t[1]--;
  if ((t[0] -= TO_HIGH_HALF (v)) > (MAX_NN_DIGIT - TO_HIGH_HALF (v)))
    t[1]--;
  t[1] -= HIGH_HALF (v);

  /* Correct estimate. */
  while ((t[1] > 0) || ((t[1] == 0) && t[0] >= c)) {
    if ((t[0] -= c) > (MAX_NN_DIGIT - c))
      t[1]--;
    aLow++;
  }
  
  return(TO_HIGH_HALF (aHigh) + aLow);
}

/*****************************************************************************
 * nn.c
 *****************************************************************************/

/* Assigns a = b.

   Lengths: a[digits], b[digits].
 */
void NN_Assign (NN_NUM a, NN_NUM b, unsigned int digits) {
  UNIVERSAL_DECL
  unsigned int i;

  for (i = 0; i < digits; i++)
    ARR_SET(a,i,ARR_GET(b,i));
}

/* Assigns a = 0.

   Lengths: a[digits].
 */
void NN_AssignZero (NN_NUM a, unsigned int digits) {
  UNIVERSAL_DECL
  unsigned int i;

  for (i = 0; i < digits; i++)
    ARR_SET(a,i,0);
}

/* Returns the significant length of a in digits.

   Lengths: a[digits].
 */
unsigned int NN_Digits (NN_NUM a, unsigned int digits) {
  UNIVERSAL_DECL
  int i;
  
  for (i = digits - 1; i >= 0; i--) {
    if (ARR_GET(a,i))
      return (i + 1);
  }

  return (i + 1);
}

/* Computes a = b * 2^c (i.e., shifts left c bits), returning carry.

   Lengths: a[digits], b[digits].
   Requires c < NN_DIGIT_BITS.
 */
static NN_DIGIT NN_LShift (NN_NUM a, NN_NUM b, 
			   unsigned int c, unsigned int digits) {
  UNIVERSAL_DECL
  NN_DIGIT bi, carry;
  unsigned int i, t;
  
  if (c >= NN_DIGIT_BITS)
    return (0);
  
  t = NN_DIGIT_BITS - c;

  carry = 0;

  for (i = 0; i < digits; i++) {
    bi = ARR_GET(b,i);
    ARR_SET(a,i,(bi << c) | carry);
    carry = c ? (bi >> t) : 0;
  }
  
  return (carry);
}

/* Computes a = c div 2^c (i.e., shifts right c bits), returning carry.

   Lengths: a[digits], b[digits].
   Requires: c < NN_DIGIT_BITS.
 */
static NN_DIGIT NN_RShift (NN_NUM a, NN_NUM b, 
			   unsigned int c, unsigned int digits) {
  UNIVERSAL_DECL
  NN_DIGIT bi, carry;
  int i;
  unsigned int t;
  
  if (c >= NN_DIGIT_BITS)
    return (0);
  
  t = NN_DIGIT_BITS - c;

  carry = 0;

  for (i = digits - 1; i >= 0; i--) {
    bi = ARR_GET(b,i);
    ARR_SET(a,i,(bi >> c) | carry);
    carry = c ? (bi << t) : 0;
  }
  
  return (carry);
}

/* Computes a = c div d and b = c mod d.

   Lengths: a[cDigits], b[dDigits], c[cDigits], d[dDigits].
   Assumes d > 0, cDigits < 2 * MAX_NN_DIGITS,
           dDigits < MAX_NN_DIGITS.
 */

#ifdef NN_DIV_STATIC
NN_NUM_KNOWN_STATIC(cc,2*MAX_NN_DIGITS+1);
NN_NUM_KNOWN_STATIC(dd,MAX_NN_DIGITS);
#endif

static void NN_Div (NN_NUM a, NN_NUM b, 
		    NN_NUM c, unsigned int cDigits, 
		    NN_NUM d, unsigned int dDigits) {
  UNIVERSAL_DECL
  NN_DIGIT ai,t;
#ifndef NN_DIV_STATIC
  NN_NUM_KNOWN(cc,2*MAX_NN_DIGITS+1);
  NN_NUM_KNOWN(dd,MAX_NN_DIGITS);
#endif
  int i;
  unsigned int ddDigits, shift;
  
  ddDigits = NN_Digits (d, dDigits);
  if (ddDigits == 0)
    return;
  
  shift = NN_DIGIT_BITS - NN_DigitBits (ARR_GET(d,ddDigits-1));
  NN_AssignZero (cc, ddDigits);
  ARR_SET(cc,cDigits, NN_LShift (cc, c, shift, cDigits));
  NN_LShift (dd, d, shift, ddDigits);
  t = ARR_GET(dd,ddDigits-1);
  
  NN_AssignZero (a, cDigits);

  for (i = cDigits-ddDigits; i >= 0; i--) {

    if (t == MAX_NN_DIGIT)
      ai = ARR_GET(cc,i+dDigits);
    else
      ai = NN_DigitDiv (cc,i+ddDigits-1, t + 1);

    ARR_SET_SUB(cc,i+ddDigits,NN_SubDigitMult (cc, cc, i, ai, dd, ddDigits));

    while (ARR_GET(cc,i+ddDigits) || (NN_Cmp (cc, i, dd, ddDigits) >= 0)) {
      ai++;
      ARR_SET_SUB(cc,i+ddDigits, NN_Sub (cc, cc, i, dd, ddDigits));
    }
    
    ARR_SET(a,i,ai);
  }

  NN_AssignZero (b, dDigits);
  NN_RShift (b, cc, shift, ddDigits);

}

/* Computes a = b + c*d, where c is a digit. Returns carry.

   Lengths: a[digits], b[digits], d[digits].
 */
static NN_DIGIT NN_AddDigitMult (NN_NUM a, NN_NUM b, int ab_start, 
				 NN_DIGIT c, NN_NUM d, unsigned int digits)
{
  UNIVERSAL_DECL
  NN_DIGIT carry, t[2];
  unsigned int i;

  if (c == 0)
    return (0);

  carry = 0;
  for (i = 0; i < digits; i++) {
    NN_DigitMult (t, c, ARR_GET(d,i));
    if ((ARR_SET(a,ab_start + i,ARR_GET(b,ab_start + i) + carry)) < carry)
      carry = 1;
    else
      carry = 0;
    if ((ARR_SET_ADD(a,ab_start + i,t[0])) < t[0])
      carry++;
    carry += t[1];
  }
  
  return (carry);
}

/* Computes a = b - c. Returns borrow.

   Lengths: a[digits], b[digits], c[digits].
 */
NN_DIGIT NN_Sub (NN_NUM a, NN_NUM b, int ab_start, 
		 NN_NUM c, unsigned int digits) {
  UNIVERSAL_DECL
  NN_DIGIT ai, borrow;
  unsigned int i;

  borrow = 0;

  for (i = 0; i < digits; i++) {
    if ((ai = ARR_GET(b,ab_start + i) - borrow) > (MAX_NN_DIGIT - borrow))
      ai = MAX_NN_DIGIT - ARR_GET(c,i);
    else if ((ai -= ARR_GET(c,i)) > (MAX_NN_DIGIT - ARR_GET(c,i)))
      borrow = 1;
    else
      borrow = 0;
    ARR_SET(a,ab_start + i, ai);
  }

  return (borrow);
}

/* Computes a = b * c.

   Lengths: a[2*digits], b[digits], c[digits].
   Assumes digits < MAX_NN_DIGITS.
 */

#ifdef NN_MULT_STATIC
NN_NUM_KNOWN_STATIC(t_nn_mult,2*MAX_NN_DIGITS);
#endif

void NN_Mult (NN_NUM a, NN_NUM b, NN_NUM c, unsigned int digits) {
  UNIVERSAL_DECL
#ifndef NN_MULT_STATIC
  NN_NUM_KNOWN(t_nn_mult,2*MAX_NN_DIGITS);
#endif
  unsigned int bDigits, cDigits, i;

  NN_AssignZero (t_nn_mult, 2 * digits);
  
  bDigits = NN_Digits (b, digits);
  cDigits = NN_Digits (c, digits);

  for (i = 0; i < bDigits; i++)
    ARR_SET_ADD(t_nn_mult,i+cDigits,NN_AddDigitMult(t_nn_mult,t_nn_mult,i,ARR_GET(b,i), c, cDigits));
  
  NN_Assign (a, t_nn_mult, 2 * digits);
  
}

/* Computes a = b mod c.

   Lengths: a[cDigits], b[bDigits], c[cDigits].
   Assumes c > 0, bDigits < 2 * MAX_NN_DIGITS, cDigits < MAX_NN_DIGITS.
 */

#ifdef NN_MOD_STATIC
NN_NUM_KNOWN_STATIC(t_mod,2 * MAX_NN_DIGITS);
#endif

void NN_Mod (NN_NUM a, NN_NUM b, unsigned int bDigits, 
	     NN_NUM c, unsigned int cDigits) {
  UNIVERSAL_DECL
#ifndef NN_MOD_STATIC
  NN_NUM_KNOWN(t_mod,2 * MAX_NN_DIGITS);
#endif
  
  NN_Div (t_mod, a, b, bDigits, c, cDigits);
}

/* Computes a = b * c mod d.

   Lengths: a[digits], b[digits], c[digits], d[digits].  Assumes d > 0, digits
< MAX_NN_DIGITS.  */ 

#ifdef NN_MODMULT_STATIC
NN_NUM_KNOWN_STATIC(t_mm,2*MAX_NN_DIGITS);
#endif

void NN_ModMult (NN_NUM a, NN_NUM b, NN_NUM c, NN_NUM d, 
		 unsigned int digits) {
  UNIVERSAL_DECL
#ifndef NN_MODMULT_STATIC
  NN_NUM_KNOWN(t_mm,2*MAX_NN_DIGITS);
#endif

  NN_Mult (t_mm, b, c, digits);
  NN_Mod (a, t_mm, 2 * digits, d, digits);
}

/* Returns sign of a - b.

   Lengths: a[digits], b[digits].
 */
int NN_Cmp (NN_NUM a, int a_start, NN_NUM b, unsigned int digits) {
  UNIVERSAL_DECL
  int i;
  
  for (i = digits - 1; i >= 0; i--) {
    if (ARR_GET(a, a_start+i) > ARR_GET(b,i)) {
      return (1);
    }
    if (ARR_GET(a, a_start+i) < ARR_GET(b,i)) {
      return (-1);
    }
  }

  return (0);
}

/* Computes a = b - c*d, where c is a digit. Returns borrow.

   Lengths: a[digits], b[digits], d[digits].
 */
static NN_DIGIT NN_SubDigitMult (NN_NUM a, NN_NUM b, int ab_start, NN_DIGIT c, 
				 NN_NUM d, unsigned int digits) {
  UNIVERSAL_DECL
  NN_DIGIT borrow, t[2];
  unsigned int i;

  if (c == 0)
    return (0);

  borrow = 0;
  for (i = 0; i < digits; i++) {
    NN_DigitMult (t, c, ARR_GET(d,i));
    if ((ARR_SET(a,ab_start+i,ARR_GET(b,ab_start+i) - borrow)) > (MAX_NN_DIGIT - borrow))
      borrow = 1;
    else
      borrow = 0;

    if ((ARR_SET_SUB(a,ab_start+i,t[0])) > (MAX_NN_DIGIT - t[0]))
      borrow++;
    borrow += t[1];
  }
  
  return (borrow);
}

/* Returns the significant length of a in bits, where a is a digit.
 */
static unsigned int NN_DigitBits (NN_DIGIT a) {
  UNIVERSAL_DECL
  unsigned int i;
  
  for (i = 0; i < NN_DIGIT_BITS; i++, a >>= 1)
    if (a == 0)
      /* break; */
      return (i);
    
  return (i);
}

/*****************************************************************************
 * aux
 *****************************************************************************/

#ifdef __linux__
#  define _ftime ftime
#  define _timeb timeb
#  include <sys/timeb.h>
#else

typedef long time_t;
struct _timeb {
        time_t time;
        unsigned short millitm;
        short timezone;
        short dstflag;
        };

#endif

int int_time()
{
  struct _timeb tstr;
  long result;

  /* places secs in "time field", msecs in "millitm" field */
  _ftime( &tstr );
  
  /* get secs */
  result = tstr.time;

  /* free up top ten 10 bits (to avoid overflow of following multiplication) */
  result = result & 0x003fffff;

  /* convert from secs to msecs (now uses all 32 bits) */
  result = result * 1000;
  
  /* add in msecs */
  result = result + tstr.millitm;

  return result;
}

void print_time(int start_time, int end_time, int iterations) {
  printf("%d \t %d\n", iterations, (end_time - start_time));
}

/*****************************************************************************
 * main
 *****************************************************************************/

int main(int argc, char **argv)
{
  UNIVERSAL_DECL
  int start_time;
  int end_time;
  int iterations=0;
  int i=0;

  NN_NUM_KNOWN(t,65);
  NN_NUM_KNOWN(d,65);

  if(argc!=2) {
   printf("%s: usage <num>\n",argv[0]);
   return(255);
  }

  iterations = atoi(argv[1]);


  for(i=0;i<65;i++) {
    ARR_SET(t,i,0);
    ARR_SET(d,i,0);
  }
  ARR_SET(t,0,4);
  ARR_SET(t,1,11);
  ARR_SET(t,2,41);
  ARR_SET(t,3,32);
  ARR_SET(t,4,75);
  ARR_SET(t,5,40);
  ARR_SET(t,6,19);
  ARR_SET(t,7,56);

  ARR_SET(d,0,15);
  ARR_SET(d,1,34);
  ARR_SET(d,2,30);
  ARR_SET(d,3,14);
  ARR_SET(d,4,27);
  ARR_SET(d,5,34);
  ARR_SET(d,6,14);
  ARR_SET(d,7,82);

  /*
  NN_DIGIT t[65] = {4, 11, 41, 32, 75, 40, 19, 56};
  NN_DIGIT d[65] = {15, 34, 30, 14, 27, 34, 14, 82};
  */

  start_time = int_time();

  /* Computes t = t^t mod d. */
  for(i = iterations; i > 0; --i) {
    NN_ModMult (t, t, t, d, 8);
  }

  end_time = int_time();

  dumpArray("t",t, 8);
  print_time(start_time, end_time,iterations);

  return 0;
}

