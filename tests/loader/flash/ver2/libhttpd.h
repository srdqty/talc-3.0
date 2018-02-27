#ifndef _LIBHTTPD_H_
#define _LIBHTTPD_H_

#include "core.h"
#include "buffer.h"
#include "unixlib.h"
#include "conn.h"

/* extern httpd_conn?; */

/* The httpd structs. */

extern struct httpd_server {
  <string>Core::Opt hostname;
  int port;
  <string>Core::Opt cgi_pattern;
  string cwd;
  int fd;
  /* for updates */
  int maint_port;
  int maint_fd;
}

extern httpd_server HS;

extern timeval globalTimeOfDay;
extern string globalTimeOfDayStr;
extern Buffer::t globalTimeOfDayBuf;
extern void MakeHTTPDate(Buffer::t buf, int unixTime);

/* extern float DiffTime(timeval start, timeval end); */



/* Initializes.  Does the socket(), bind(), and listen().   Returns an
** httpd_server* which includes a socket fd that you can select() on.
** Return (httpd_server*) 0 on error.
*/
extern bool HttpdInitialize(<string>Core::Opt hostname, int port, 
                            int maint_portd, <string>Core::Opt cgi_pattern, 
                            string cwd);

/* Call to shut down. */
extern void HttpdTerminate();

/* Starts sending data back to the client.  In some cases (directories,
** CGI programs), finishes sending by itself - in those cases, return value
** is SR_DONOTHING.  If there is more data to be sent, 
** then returns SR_READY
**
** Returns SR_ERROR on error.
*/

/* Send an error message back to the client. */
extern void HttpdSendErr(httpd_conn hc, int status, string title, 
			 string form, string arg);

/* Some error messages. */
extern string httpd_err503title;
extern string httpd_err503form;

/* Generates a string representation of a method number. */
extern string HttpdMethodStr(int method);

/* Sets the allowed number of file descriptors to the maximum and returns it. */
extern int HttpdGetNFiles();


extern int defaultSendBufSize;

/* ---------------------------------------------------------------- */


extern string ok200title;
extern string err302title;
extern string err302form;
extern string err304title;
extern string err400title;
extern string err400form;
extern string err403title;
extern string err403form;
extern string err404title;
extern string err404form;
extern string err408title;
extern string err408form;
extern string err500title;
extern string err500form;
extern string err501title;
extern string err501form;
extern string httpd_err503title;
extern string httpd_err503form;

extern void SendMime(Buffer::t, httpd_conn hc, int status, string title, 
		     string encodings, string extraheads, string type, 
		     int length, int mod);

/* string realloc_str(char** strP, int* maxsizeP, int size); */


extern string methodStrings[];

extern void SendDirRedirect(httpd_conn hc);

extern int systemPageSize;
extern int systemPageSizeBits;

extern int GenericStringHash(string str);

extern void printConn(httpd_conn c);

#endif
