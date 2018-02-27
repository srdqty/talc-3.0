#include <errno.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

void *GC_malloc(int size);


#define FALSETAG 0
#define TRUETAG 1
#define NULLTAG 2
#define EOFTAG  3
#define INTTAG 0
#define PAIRTAG 1
#define STRINGTAG 2
#define CHARTAG 3
#define INDESCTAG 4
#define OUTDESCTAG 5
#define FIRSTCLOSURETAG 6

FILE* original_stdin = NULL;
FILE* original_stdout = NULL;
FILE* curr_in = NULL;
FILE* curr_out = NULL;

void init_sclib_io() {
  original_stdin = stdin;
  original_stdout = stdout;
  curr_in = stdin;
  curr_out = stdout;
}


typedef struct { 
  int size;
  char chars[1];
} *tal_string;

typedef struct {
  int tag;
  int thechar;
} *tal_char;

typedef struct Cons{
  FILE* port;
  struct Cons* next;
} cons;


tal_char make_char(char ch) {
  tal_char c = (tal_char)(GC_malloc(2*sizeof(int)));
  c->tag = CHARTAG;
  c->thechar = (int)ch;
  return(c);
}

char *tal_string_to_string(tal_string s)
{
  int i;
  char *cs = (char *)(GC_malloc((s->size)+1));

  for (i=0; i < s->size; i++) {
    cs[i] = s->chars[i];
  }
  cs[s->size] = (char)0;
  return(cs);
}


tal_string scnewstring(int sz){
  tal_string t = GC_malloc(sz+1);
  t->size = sz;
  
  return (t);
}

tal_string scnewstringchar(int sz, char init){
  int i;
  tal_string t = scnewstring(sz);
  for (i=0; i< t->size; i++) {
    t->chars[i] = init;
  }
  return (t);
}

void scfputstring(FILE *f,tal_string s)
{
  int i;
  for(i=0; i < s->size; i++)
    fprintf(f,"%c",s->chars[i]);
  return;
}

// eventually do not write this in C!!!
void print_v(void* v)
{
  tal_string s;

  if ((unsigned int) v == FALSETAG) {
    fprintf(curr_out,"#f");
  } else if ((unsigned int)v == TRUETAG) {
    fprintf(curr_out,"#t");
  } else if ((unsigned int)v == NULLTAG) {
    fprintf(curr_out, "'()");
  } else if ((unsigned int)v == EOFTAG) {
    fprintf(curr_out,"#EOF");
  } else if (((unsigned int *)v)[0] == INTTAG) {
    fprintf(curr_out,"%d",((unsigned int *)v)[1]);
  } else if (((unsigned int *)v)[0] == PAIRTAG) {
    fprintf(curr_out,"(");
    print_v(((void **)v)[1]);
    fprintf(curr_out," . ");
    print_v(((void **)v)[2]);
    fprintf(curr_out,")");
  } else if (((unsigned int *)v)[0] == STRINGTAG) {
    fprintf(curr_out,"\"");
    s = ((tal_string *)v)[1];
    scfputstring(curr_out,s);
    fprintf(curr_out,"\"");
  } else if (((unsigned int *)v)[0] == CHARTAG) {
    fprintf(curr_out,"'");
    fprintf(curr_out,"%c",((int *)v)[1]);
    fprintf(curr_out,"'");
  } else if (((unsigned int *)v)[0] == INDESCTAG) {
    fprintf(curr_out,"input-port(");
    fprintf(curr_out,"%d",((int *)v)[1]);
    fprintf(curr_out,")");
  } else if (((unsigned int *)v)[0] == OUTDESCTAG) {
    fprintf(curr_out,"output-port(");
    fprintf(curr_out,"%d",((int *)v)[1]);
    fprintf(curr_out,")");
  } else {
    fprintf(curr_out,"#fn");
  }
}

void scprint(void *v) 
{ 
  print_v(v);
  fprintf(curr_out,"\n");
}


FILE *scopen_in(tal_string s)
{
  FILE *fd = fopen(tal_string_to_string(s),"r");
  if (errno != 0) {
    fprintf(stderr,"open_in failed with errno %d\n",errno);
    exit(1);
  }
  return(fd);
}

FILE *scopen_out(tal_string s)
{
  FILE *fd = fopen(tal_string_to_string(s),"w");
  if (errno != 0) {
    fprintf(stderr,"open_out failed with errno %d\n",errno);
    exit(1);
  }
  return(fd);
}

void scclose_in(FILE *f)
{
  if (fclose(f) == EOF) {
    fprintf(stderr,"close_in failed\n");
    exit(1);
  }
  return;
}

void scclose_out(FILE *f)
{
  if (fclose(f) == EOF) {
    fprintf(stderr,"close_out failed\n");
    exit(1);
  }
  return;
}

void scflush_out(FILE *f)
{
  if (fflush(f) == EOF) {
    fprintf(stderr,"flush_out failed\n");
    exit(1);
  }
  return;
}

int scgetstdin()
{
  return((int)stdin);
}

int scgetstdout()
{
  return((int)stdout);
}

int scgetstderr()
{
  return((int)stderr);
}



tal_char scfgetchar(FILE *f)
{
  int c = getc(f);
  if (c == EOF)
    return((tal_char)0);
  return(make_char((char)c));
}

int scfpeekchar(FILE *f)
{
  fprintf(stderr,"peekchar unimplemented\n");
  exit(1);
}

int scfgetstring(FILE *f,int size)
{
  fprintf(stderr,"fgetstring unimplemented\n");
  exit(1);
}

void scfputchar(FILE *f,int c)
{
  fprintf(f,"%c",(char)c);
  return;
}

tal_char scgetchar()
{
  return(scfgetchar(curr_in));
}

int scpeekchar()
{
  return(scfpeekchar(curr_in));
}

int scgetstring(int size)
{
  return(scfgetstring(curr_in,size));
}

void scputchar(int c)
{
  scfputchar(curr_out,c);
}

void scputstring(tal_string s)
{
  scfputstring(curr_out,s);
}

void notInt()
{
  fprintf(stderr,"not an int error!\n");
  exit(1);
}

void notFn()
{
  fprintf(stderr,"not a function or wrong arity error!\n");
  exit(1);
}

void notPair()
{
  fprintf(stderr,"not a pair error!\n");
  exit(1);
}

void notString()
{
  fprintf(stderr,"not a string error!\n");
  exit(1);
}

void notChar()
{
  fprintf(stderr,"not a char error!\n");
  exit(1);
}

void notIndesc()
{
  fprintf(stderr,"not an input-file-descriptor error!\n");
  exit(1);
}

void notOutdesc()
{
  fprintf(stderr,"not an output-file-descriptor error!\n");
  exit(1);
}

/* For implementation of call-with-input-file, with-input-from-file etc. */

FILE* currentInPort() {
  return curr_in;
}

FILE* currentOutPort() {
  return curr_out;
}

cons* input_list = NULL;
cons* output_list = NULL;

/* Should I use normal malloc or GC_malloc here? */

void pushInPort(tal_string s){

  cons* next = GC_malloc(sizeof(cons));
  next->port = curr_in;
  input_list->next = next;
  /* input_list->next; */
  curr_in = scopen_in(s);
}


void pushOutPort(tal_string s){

  cons* next = GC_malloc(sizeof(cons));
  next->port = curr_in;
  output_list->next = next;
  /* output_list->next; */
  curr_out = scopen_out(s);
}

FILE* popInPort() {
  cons* temp;
  
  scclose_in(curr_in);
  if (input_list != NULL) {
    curr_in = input_list->port;
    temp = input_list;
    input_list = input_list->next;
    return(curr_in);
  } else {
    fprintf(stderr, "Compiler error: too many pop-input-port calls");
    exit(1);
  }
}

FILE* popOutPort() {
  cons* temp;

  scclose_out(curr_in);
  if (output_list != NULL) {
    curr_out = output_list->port;
    temp = output_list;
    output_list = output_list->next;
    return(curr_in);
  } else {
    fprintf(stderr, "Compiler error: too many pop-output-port calls");
    exit(1);
  }
}

/* EOF: sclib.c */

