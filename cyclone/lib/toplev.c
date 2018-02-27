#include "cyc_include.h"

struct $tuple0 {
  void *f1;
  void *f2;
};
struct $tuple1 {
  void *f1;
  void *f2;
  void *f3;
};
struct $tuple2 {
  int f1;
  int f2;
};
struct __sFILE;
typedef struct __sFILE FILE;
extern struct __sFILE *cyc_stdout;
extern struct __sFILE *cyc_stdin;
extern struct __sFILE *cyc_stderr;
typedef unsigned int uint;
typedef unsigned int size_t;
typedef char *Cstring;
typedef struct $tagged_string *string;
extern struct $tagged_string *new_string(int);
extern char *string_to_Cstring(struct $tagged_string *);
extern int system(char *);
extern int f_string_read(struct __sFILE *,struct $tagged_string *,int,int);
extern int fflush(struct __sFILE *);
extern int fgetc(struct __sFILE *);
extern char Core$$FileOpenError$_tag[19];
struct Core$$FileOpenError$_struct {
  char *tag;
  struct $tagged_string *f1;
};
extern char Core$$FileCloseError$_tag[20];
struct Core$$FileCloseError$_struct {
  char *tag;
};
extern struct __sFILE Core$file_using(struct $tagged_string *,struct $tagged_string *);
extern void Core$file_close(struct __sFILE *);
extern int Core$file_length(struct $tagged_string *);
extern struct $tagged_string *Core$get_env(struct $tagged_string *);
struct Core$Opt {
  void *v;
};
typedef struct Core$Opt *Core$Opt_t;
extern struct Core$Opt *Core$opt_map(void *(*f)(void *),struct Core$Opt *x);
extern bool Core$true_f(void *);
extern bool Core$false_f(void *);
extern void *Core$fst(struct $tuple0 *);
extern void *Core$snd(struct $tuple0 *);
extern void *Core$third(struct $tuple1 *);
extern void *Core$identity(void *);
extern int Core$intcmp(int,int);
extern int Core$charcmp(char,char);
extern char Core$$InvalidArg$_tag[16];
struct Core$$InvalidArg$_struct {
  char *tag;
  struct $tagged_string *f1;
};
extern char Core$$Failure$_tag[13];
struct Core$$Failure$_struct {
  char *tag;
  struct $tagged_string *f1;
};
extern char Core$$Impossible$_tag[16];
struct Core$$Impossible$_struct {
  char *tag;
  struct $tagged_string *f1;
};
extern char Core$$Not_found$_tag[15];
struct Core$$Not_found$_struct {
  char *tag;
};
extern bool Core$is_space(char);
extern int Core$int_of_string(struct $tagged_string *);
extern struct $tagged_string *Core$string_of_int(int);
extern struct $tagged_string *Core$string_of_uint(unsigned int);
extern struct $tagged_string *Core$string_of_char(char);
extern Bool Core$box_bool(bool);
extern Char Core$box_char(char);
extern Short Core$box_short(short);
extern $LongLong Core$box_long_long(long long);
extern Float Core$box_float(float);
extern Double Core$box_double(double);
extern bool Core$unbox_bool(Bool);
extern char Core$unbox_char(Char);
extern short Core$unbox_short(Short);
extern long long Core$unbox_long_long($LongLong);
extern float Core$unbox_float(Float);
extern double Core$unbox_double(Double);
int x = 0;
int y = 1 + 1;
int *z = &y;
struct Point {
  int x;
  int y;
};
typedef struct Point *point;
struct Point p = (struct Point){.x=1,.y=2};
struct Point *pp = (struct Point *)&p;
struct $tuple2 q = (struct $tuple2){.f1=1,.f2=2};
struct $tuple2 *qq = (struct $tuple2 *)&q;
char foo[4] = "foo";
char $temp0[3] = {'a','b','c'};
struct $tagged_string $temp1 = {3,$temp0};
struct $tagged_string *blahb = (struct $tagged_string *)&$temp1;
char $temp2[4] = "abc";
struct $tagged_string $temp3 = {4,$temp2};
struct $tagged_string *blahc = (struct $tagged_string *)&$temp3;
int arr[10] = {0,1,2,3,4,5,6,7,8,9};
Char c1 = (Char)'a';
Char c2 = (Char)3;
char c3 = 'b';
char c4 = (char)3;
Short s1 = (Short)3;
short s2 = (short)3;
Int i1 = (Int)3;
int i2 = 3;
double d1 = (double)3;
struct $boxed_double_struct $temp4 = {3};
Double d2 = (Double)&$temp4;
long long l1 = (long long)3;
struct $boxed_long_long_struct $temp5 = {3};
$LongLong l2 = ($LongLong)&$temp5;
void blah() {
  struct $tagged_string *baz = (struct $tagged_string *)({ char *$temp6 = (char *)foo;
    struct $tagged_string *$temp7 = (struct $tagged_string *)malloc(sizeof(struct $tagged_string));
    $temp7->sz = 4;
    $temp7->contents = $temp6;
    $temp7; });
  ({ struct $tagged_string *$temp8 = baz;
    printf("%.*s\n",$temp8->sz,$temp8->contents); });
}
void (*bbb)() = blah;
void (*ccc)() = 0;
typedef void *Day;
void *Sun = (void *)0;
void *Mon = (void *)1;
void *Tue = (void *)2;
void *Wed = (void *)3;
void *Thu = (void *)4;
void *Fri = (void *)5;
void *Sat = (void *)6;
typedef void *day;
void *ddd = (void *)1;
typedef void *Exp;
const int $Intexp$_tag = 0;
struct $Intexp$_struct {
  int tag;
  int f1;
};
const int $Plusexp$_tag = 1;
struct $Plusexp$_struct {
  int tag;
  void *f1;
  void *f2;
};
const int $Varexp$_tag = 2;
struct $Varexp$_struct {
  int tag;
  struct $tagged_string *f1;
};
typedef void *exp;
struct $Intexp$_struct $temp9 = {0,3};
void *eee = (struct $Intexp$_struct *)&$temp9;
struct $Intexp$_struct $temp11 = {0,1};
struct $Intexp$_struct $temp12 = {0,2};
struct $Plusexp$_struct $temp10 = {1,(struct $Intexp$_struct *)&$temp11,(struct $Intexp$_struct *)&$temp12};
void *ggg = (struct $Plusexp$_struct *)&$temp10;
char $temp14[6] = "howdy";
struct $tagged_string $temp15 = {6,$temp14};
struct $Varexp$_struct $temp13 = {2,(struct $tagged_string *)&$temp15};
void *hhh = (struct $Varexp$_struct *)&$temp13;
typedef struct $xenum_struct *foo_t;
char $Barbie$_tag[7] = "Barbie";
struct $Barbie$_struct {
  char *tag;
};
char $Ken$_tag[4] = "Ken";
struct $Ken$_struct {
  char *tag;
  int f1;
  struct $tagged_string *f2;
};
struct $Barbie$_struct $temp16 = {$Barbie$_tag};
struct $xenum_struct *barbie = (struct $xenum_struct *)&$temp16;
char $temp18[6] = "hello";
struct $tagged_string $temp19 = {6,$temp18};
struct $Ken$_struct $temp17 = {$Ken$_tag,3,(struct $tagged_string *)&$temp19};
struct $xenum_struct *ken = (struct $xenum_struct *)&$temp17;
