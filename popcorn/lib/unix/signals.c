/* Support for signals in Popcorn.  This would be straightforward if
   not for the fact that 
     Popcorn does not respect callee-saves registers. 
     Popcorn does not respect the C calling convention (EBP is used
       for the exception handler)
   Therefore, we have to keep track of signals in C, and call the
   Popcorn function after properly setting up the frame. See popraise.S.

   XXX Right now we ignore an exception that is raised in a signal
   handler.  Could change this to raise the exception in the context
   of the interrupted function (as in Ocaml).
*/

#include "unixsupport.h"
#include <signal.h>

/* defined for Linux */
#define POSIX_SIGNALS

/* Define this if translate_fn_ptrs is set to true in popdyntrans.ml.
   In this case, the signal handler that is passed in has one
   extra level of indirection due to the dynamic updating translation. */
/* #define TRANSLATE_FN_PTRS */
#if defined(TRANSLATE_FN_PTRS) && defined(NOTICE_UPDATES)
#define GET_HANDLER(v) (*((void **)(v)))
#else
#define GET_HANDLER(v) (v)
#endif

extern void call_sig_handler(int sig, void (*f)(int sig));

#define NPOP_SIGS 4
static int popsigs[NPOP_SIGS] = { SIGINT, SIGPIPE, SIGTERM, SIGCHLD };
static struct str_internal popsigstrings[NPOP_SIGS] = {
  { 6, "SIGINT" },
  { 7, "SIGPIPE" },
  { 7, "SIGTERM" },
  { 7, "SIGCHLD" }
};
static void **signal_handlers = NULL;

static void execute_signal(int pop_signum, int signal_number)
{
#ifdef POSIX_SIGNALS
  sigset_t sigs;
  /* Block the signal before executing the handler, and record in sigs
     the original signal mask */
  sigemptyset(&sigs);
  sigaddset(&sigs, signal_number);
  sigprocmask(SIG_BLOCK, &sigs, &sigs);
#endif
  call_sig_handler(pop_signum, signal_handlers[pop_signum-1]);
#ifdef POSIX_SIGNALS
  /* Restore the original signal mask */
  sigprocmask(SIG_SETMASK, &sigs, NULL);
#endif
}

static void handle_signal(int sig)
{
  int i;
  signal(sig, handle_signal);
  /* translate to the Popcorn signal number */
  for (i=0; i<NPOP_SIGS; i++)
    if (popsigs[i] == sig) {
      /* call the handler */
      execute_signal(i+1,sig);
      return;
    }
  assert(1 == 0);
}

struct union_field {
#define Signal_handle 3
  int flag;
  void *handler;
};

union signal_behavior {
#define Signal_default 1
#define Signal_ignore 2
  int flag;
  struct union_field *field;
};

void unix_signal(union signal_behavior action, int popsignum) {
  int signum;
  void (*act)(int signo), (*oldact)(int signo);
#ifdef POSIX_SIGNALS
  struct sigaction sigact, oldsigact;
#endif

  assert(popsignum >= 1 && popsignum <= NPOP_SIGS);
  signum = popsigs[popsignum-1];

  switch (action.flag) {
  case Signal_default:
    act = SIG_DFL;
    break;
  case Signal_ignore:
    act = SIG_IGN;
    break;
  default:
    if (signal_handlers == NULL) {
      signal_handlers = xalloc(sizeof(void *) * NPOP_SIGS);
    }
    signal_handlers[popsignum-1] = GET_HANDLER(action.field->handler);
    act = (void (*)(int)) handle_signal;
    break;
  }
#ifdef POSIX_SIGNALS
  sigact.sa_handler = act;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = 0;
  if (sigaction(signum, &sigact, &oldsigact) == -1) 
    unix_error(__FILE__,__LINE__,"sigaction");
  oldact = oldsigact.sa_handler;
#else
  oldact = signal(signum, act);
  if (oldact == SIG_ERR) 
    unix_error(__FILE__,__LINE__,"signal");
#endif
  /* XXX returns the old value; don't do that for now */
#ifdef notdef
  if (oldact == (void (*)(int)) handle_signal) {
    res = alloc_small(1, 0);          /* Signal_handle */
    Field(res, 0) = Field(signal_handlers, signum);
  }
  else if (oldact == SIG_IGN)
    res = Val_int(1);           /* Signal_ignore */
  else
    res = Val_int(0);           /* Signal_default */
  return res;
#endif
}

string unix_signal_string(int sig) {
  return &popsigstrings[sig-1];
}
