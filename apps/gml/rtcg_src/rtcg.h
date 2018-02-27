
// Simple functions to make sure we never generate the same code twice.
// As a side-effect keeps pointers to all code all the time.

prefix Rtcg {
open Rtcg

extern cache<a,b,c>;

extern <a,b>cache init<a,b>(string name, c env, b gen(c,a));

// Only generates new code if it hasn't already.
extern b gen<a,b,c>(<a,b,c>cache ca, a x);

extern void report(<a,b,c>cache);

extern c env(<a,b,c>cache ca);

}
