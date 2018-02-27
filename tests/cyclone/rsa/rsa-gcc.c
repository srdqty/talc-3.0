

#define ITERATE 40000

/* global.h */

#ifndef PROTOTYPES
#define PROTOTYPES 0
#endif

typedef unsigned char *POINTER;
typedef unsigned short int UINT2;
typedef unsigned long int UINT4;
#if PROTOTYPES
#define PROTO_LIST(list) list
#else
#define PROTO_LIST(list) ()
#endif

/* nn.h */
typedef UINT4 NN_DIGIT;
typedef UINT2 NN_HALF_DIGIT;
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
#define DIGIT_MSB(x) (unsigned int)(((x) >> (NN_DIGIT_BITS - 1)) & 1)
#define DIGIT_2MSB(x) (unsigned int)(((x) >> (NN_DIGIT_BITS - 2)) & 3)

NN_DIGIT NN_Sub PROTO_LIST
  ((NN_DIGIT *, NN_DIGIT *, NN_DIGIT *, unsigned int));

#define NN_ASSIGN_DIGIT(a, b, digits) {NN_AssignZero (a, digits); a[0] = b;}

#define MAX_RSA_MODULUS_BITS 2048
#define MAX_RSA_MODULUS_LEN ((MAX_RSA_MODULUS_BITS + 7) / 8) 

/* digit.h */
void NN_DigitMult PROTO_LIST ((NN_DIGIT [2], NN_DIGIT, NN_DIGIT));
void NN_DigitDiv PROTO_LIST ((NN_DIGIT *, NN_DIGIT [2], NN_DIGIT));

/*****************************************************************************
 * prototypes
 *****************************************************************************/

static NN_DIGIT NN_RShift PROTO_LIST
  ((NN_DIGIT *, NN_DIGIT *, unsigned int, unsigned int));
static void NN_Div PROTO_LIST
  ((NN_DIGIT *, NN_DIGIT *, NN_DIGIT *, unsigned int, NN_DIGIT *,
    unsigned int));
static NN_DIGIT NN_AddDigitMult PROTO_LIST 
  ((NN_DIGIT *, NN_DIGIT *, NN_DIGIT, NN_DIGIT *, unsigned int));
static NN_DIGIT NN_SubDigitMult PROTO_LIST
((NN_DIGIT *, NN_DIGIT *, NN_DIGIT, NN_DIGIT *, unsigned int));
static unsigned int NN_DigitBits PROTO_LIST
((NN_DIGIT));

void dumpArray(char *s,NN_DIGIT *a, unsigned int digits)
{
  unsigned int i;

  printf("; %s [ %d ] ", s, digits);
  for (i = 0; i < digits; i++)
    printf("%d ", a[i]);
  printf("\n");
}

int Dyn;

/*****************************************************************************
 * digit.c
 *****************************************************************************/

/* Computes a = b * c, where b and c are digits.

   Lengths: a[2].
 */
void NN_DigitMult (a, b, c)
NN_DIGIT a[2], b, c;
{
  NN_DIGIT t, u;
  NN_HALF_DIGIT bHigh, bLow, cHigh, cLow;

  bHigh = HIGH_HALF (b);
  bLow = LOW_HALF (b);
  cHigh = HIGH_HALF (c);
  cLow = LOW_HALF (c);

  a[0] = (NN_DIGIT)bLow * (NN_DIGIT)cLow;
  t = (NN_DIGIT)bLow * (NN_DIGIT)cHigh;
  u = (NN_DIGIT)bHigh * (NN_DIGIT)cLow;
  a[1] = (NN_DIGIT)bHigh * (NN_DIGIT)cHigh;
  
  if ((t += u) < u)
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
void NN_DigitDiv (a, b, c)
NN_DIGIT *a, b[2], c;
{
  NN_DIGIT t[2], u, v;
  NN_HALF_DIGIT aHigh, aLow, cHigh, cLow;

  cHigh = HIGH_HALF (c);
  cLow = LOW_HALF (c);

  t[0] = b[0];
  t[1] = b[1];

  /* Underestimate high half of quotient and subtract.
   */
  if (cHigh == MAX_NN_HALF_DIGIT) {
    aHigh = HIGH_HALF (t[1]);
  }
  else {
    aHigh = (NN_HALF_DIGIT)(t[1] / (cHigh + 1));
  }
  u = (NN_DIGIT)aHigh * (NN_DIGIT)cLow;
  v = (NN_DIGIT)aHigh * (NN_DIGIT)cHigh;

  if ((t[0] -= TO_HIGH_HALF (u)) > (MAX_NN_DIGIT - TO_HIGH_HALF (u))) {
    t[1]--;
  }

  t[1] -= HIGH_HALF (u);
  t[1] -= v;

  /* Correct estimate.
   */
  while ((t[1] > cHigh) ||
         ((t[1] == cHigh) && (t[0] >= TO_HIGH_HALF (cLow)))) {
    if ((t[0] -= TO_HIGH_HALF (cLow)) > MAX_NN_DIGIT - TO_HIGH_HALF (cLow)) {
      t[1]--;
    }
    t[1] -= cHigh;
    aHigh++;
  }

  /* Underestimate low half of quotient and subtract.
   */
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

  /* Correct estimate.
   */
  while ((t[1] > 0) || ((t[1] == 0) && t[0] >= c)) {
    if ((t[0] -= c) > (MAX_NN_DIGIT - c))
      t[1]--;
    aLow++;
  }
  
  *a = TO_HIGH_HALF (aHigh) + aLow;

}

/*****************************************************************************
 * nn.c
 *****************************************************************************/

/* Assigns a = b.

   Lengths: a[digits], b[digits].
 */
void NN_Assign (a, b, digits)
NN_DIGIT *a, *b;
unsigned int digits;
{
  unsigned int i;

  for (i = 0; i < digits; i++)
    a[i] = b[i];
}

/* Assigns a = 0.

   Lengths: a[digits].
 */
void NN_AssignZero (a, digits)
NN_DIGIT *a;
unsigned int digits;
{
  unsigned int i;

  for (i = 0; i < digits; i++)
    a[i] = 0;
}

/* Returns the significant length of a in digits.

   Lengths: a[digits].
 */
unsigned int NN_Digits (a, digits)
NN_DIGIT *a;
unsigned int digits;
{
  int i;
  
  for (i = digits - 1; i >= 0; i--)
    if (a[i])
      /* break; */
      return (i + 1);

  return (i + 1);
}

/* Computes a = b * 2^c (i.e., shifts left c bits), returning carry.

   Lengths: a[digits], b[digits].
   Requires c < NN_DIGIT_BITS.
 */
static NN_DIGIT NN_LShift (a, b, c, digits)
NN_DIGIT *a, *b;
unsigned int c, digits;
{
  NN_DIGIT bi, carry;
  unsigned int i, t;
  
  if (c >= NN_DIGIT_BITS)
    return (0);
  
  t = NN_DIGIT_BITS - c;

  carry = 0;

  for (i = 0; i < digits; i++) {
    bi = b[i];
    a[i] = (bi << c) | carry;
    carry = c ? (bi >> t) : 0;
  }
  
  return (carry);
}

/* Computes a = c div 2^c (i.e., shifts right c bits), returning carry.

   Lengths: a[digits], b[digits].
   Requires: c < NN_DIGIT_BITS.
 */
static NN_DIGIT NN_RShift (a, b, c, digits)
NN_DIGIT *a, *b;
unsigned int c, digits;
{
  NN_DIGIT bi, carry;
  int i;
  unsigned int t;
  
  if (c >= NN_DIGIT_BITS)
    return (0);
  
  t = NN_DIGIT_BITS - c;

  carry = 0;

  for (i = digits - 1; i >= 0; i--) {
    bi = b[i];
    a[i] = (bi >> c) | carry;
    carry = c ? (bi << t) : 0;
  }
  
  return (carry);
}

/* Computes a = c div d and b = c mod d.

   Lengths: a[cDigits], b[dDigits], c[cDigits], d[dDigits].
   Assumes d > 0, cDigits < 2 * MAX_NN_DIGITS,
           dDigits < MAX_NN_DIGITS.
 */

int count_nn_div = 0;

static void NN_Div (a, b, c, cDigits, d, dDigits)
NN_DIGIT *a, *b, *c, *d;
unsigned int cDigits, dDigits;
{
  NN_DIGIT ai, cc[2*MAX_NN_DIGITS+1], dd[MAX_NN_DIGITS], t;
  int i;
  unsigned int ddDigits, shift;
  
  ddDigits = NN_Digits (d, dDigits);
  if (ddDigits == 0)
    return;
  
  shift = NN_DIGIT_BITS - NN_DigitBits (d[ddDigits-1]);
  NN_AssignZero (cc, ddDigits);
  cc[cDigits] = NN_LShift (cc, c, shift, cDigits);
  NN_LShift (dd, d, shift, ddDigits);
  t = dd[ddDigits-1];
  
  NN_AssignZero (a, cDigits);

  for (i = cDigits-ddDigits; i >= 0; i--) {

    if (t == MAX_NN_DIGIT)
      ai = cc[i+dDigits];
    else
      NN_DigitDiv (&ai, &cc[i+ddDigits-1], t + 1);

    cc[i+ddDigits] -= NN_SubDigitMult (&cc[i], &cc[i], ai, dd, ddDigits);

    while (cc[i+ddDigits] || (NN_Cmp (&cc[i], dd, ddDigits) >= 0)) {
      ai++;
      cc[i+ddDigits] -= NN_Sub (&cc[i], &cc[i], dd, ddDigits);
    }
    
    a[i] = ai;
  }

  NN_AssignZero (b, dDigits);
  NN_RShift (b, cc, shift, ddDigits);

}

/* Computes a = b + c*d, where c is a digit. Returns carry.

   Lengths: a[digits], b[digits], d[digits].
 */
static NN_DIGIT NN_AddDigitMult (a, b, c, d, digits)
NN_DIGIT *a, *b, c, *d;
unsigned int digits;
{
  NN_DIGIT carry, t[2];
  unsigned int i;

  if (c == 0)
    return (0);

  carry = 0;
  for (i = 0; i < digits; i++) {
    NN_DigitMult (t, c, d[i]);
    if ((a[i] = b[i] + carry) < carry)
      carry = 1;
    else
      carry = 0;
    if ((a[i] += t[0]) < t[0])
      carry++;
    carry += t[1];
  }
  
  return (carry);
}

/* Computes a = b - c. Returns borrow.

   Lengths: a[digits], b[digits], c[digits].
 */
NN_DIGIT NN_Sub (a, b, c, digits)
NN_DIGIT *a, *b, *c;
unsigned int digits;
{
  NN_DIGIT ai, borrow;
  unsigned int i;

  borrow = 0;

  for (i = 0; i < digits; i++) {
    if ((ai = b[i] - borrow) > (MAX_NN_DIGIT - borrow))
      ai = MAX_NN_DIGIT - c[i];
    else if ((ai -= c[i]) > (MAX_NN_DIGIT - c[i]))
      borrow = 1;
    else
      borrow = 0;
    a[i] = ai;
  }

  return (borrow);
}

/* Computes a = b * c.

   Lengths: a[2*digits], b[digits], c[digits].
   Assumes digits < MAX_NN_DIGITS.
 */
void NN_Mult (a, b, c, digits)
NN_DIGIT *a, *b, *c;
unsigned int digits;
{
  NN_DIGIT t[2*MAX_NN_DIGITS];
  unsigned int bDigits, cDigits, i;

  NN_AssignZero (t, 2 * digits);
  
  bDigits = NN_Digits (b, digits);
  cDigits = NN_Digits (c, digits);

  for (i = 0; i < bDigits; i++)
    t[i+cDigits] += NN_AddDigitMult (&t[i], &t[i], b[i], c, cDigits);
  
  NN_Assign (a, t, 2 * digits);
  
}

/* Computes a = b mod c.

   Lengths: a[cDigits], b[bDigits], c[cDigits].
   Assumes c > 0, bDigits < 2 * MAX_NN_DIGITS, cDigits < MAX_NN_DIGITS.
 */
void NN_Mod (a, b, bDigits, c, cDigits)
NN_DIGIT *a, *b, *c;
unsigned int bDigits, cDigits;
{
  NN_DIGIT t_mod[2 * MAX_NN_DIGITS];
  
  NN_Div (t_mod, a, b, bDigits, c, cDigits);
  
}

/* Computes a = b * c mod d.

   Lengths: a[digits], b[digits], c[digits], d[digits].  Assumes d > 0, digits
< MAX_NN_DIGITS.  */ 

void NN_ModMult (a, b, c, d, digits)
NN_DIGIT *a, *b, *c, *d;
unsigned int digits;
{

  NN_DIGIT t_mm[2*MAX_NN_DIGITS];

  NN_Mult (t_mm, b, c, digits);
  NN_Mod (a, t_mm, 2 * digits, d, digits);
  
}

/* Returns sign of a - b.

   Lengths: a[digits], b[digits].
 */
int NN_Cmp (a, b, digits)
NN_DIGIT *a, *b;
unsigned int digits;
{
  int i;
  
  for (i = digits - 1; i >= 0; i--) {
    if (a[i] > b[i]) {
      return (1);
    }
    if (a[i] < b[i]) {
      return (-1);
    }
  }

  return (0);
}

/* Computes a = b - c*d, where c is a digit. Returns borrow.

   Lengths: a[digits], b[digits], d[digits].
 */
static NN_DIGIT NN_SubDigitMult (a, b, c, d, digits)
NN_DIGIT *a, *b, c, *d;
unsigned int digits;
{
  NN_DIGIT borrow, t[2];
  unsigned int i;

  if (c == 0)
    return (0);

  borrow = 0;
  for (i = 0; i < digits; i++) {
    NN_DigitMult (t, c, d[i]);
    if ((a[i] = b[i] - borrow) > (MAX_NN_DIGIT - borrow))
      borrow = 1;
    else
      borrow = 0;
    if ((a[i] -= t[0]) > (MAX_NN_DIGIT - t[0]))
      borrow++;
    borrow += t[1];
  }
  
  return (borrow);
}

/* Returns the significant length of a in bits, where a is a digit.
 */
static unsigned int NN_DigitBits (a)
NN_DIGIT a;
{
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


/*
void print_times(struct rusage start, 
		 struct rusage stop,
		 int iterations)
{
  float start_user, stop_user, start_sys, stop_sys;

  start_user = start.ru_utime.tv_sec + (start.ru_utime.tv_usec / 1000000.0);
   stop_user =  stop.ru_utime.tv_sec + ( stop.ru_utime.tv_usec / 1000000.0);
  
  start_sys = start.ru_stime.tv_sec + (start.ru_stime.tv_usec / 1000000.0);
   stop_sys =  stop.ru_stime.tv_sec + ( stop.ru_stime.tv_usec / 1000000.0);

  printf("[user: %f, sys: %f]\n\n", (stop_user - start_user) / iterations,
	                         (stop_sys - start_sys) / iterations);
}
*/

/*****************************************************************************
 * main
 *****************************************************************************/

int main(int argc, char **argv)
{
  int start_time;
  int end_time;
  int iterations=0;
  int i;

  NN_DIGIT t[65] = {4, 11, 41, 32, 75, 40, 19, 56};
  NN_DIGIT d[65] = {15, 34, 30, 14, 27, 34, 14, 82};

  if(argc!=2) {
   printf("%s: usage <num>\n",argv[0]);
   return(255);
  }

  iterations = atoi(argv[1]);

  start_time = int_time();

  /* Computes t = t^t mod d. */
  for(i = iterations; i > 0; --i) {
    NN_ModMult (t, t, t, d, 8);
  }

  end_time = int_time();

  dumpArray("t",t, 8);
  print_time(start_time, end_time, iterations);

  return 0;

}

