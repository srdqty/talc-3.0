/* Version of hotfile.h but without stuff for caching.  This is
   the first rev of the server, so we ditched this stuff, to be added
   as updates later. */

#include "conn.h"
/* extern httpd_conn?; */
extern /* Hot */NameEntry?;
extern SRCode;

extern void ResumeAfterFileReading(httpd_conn c);
extern int DoSingleWrite(httpd_conn c, int fd, int forRW);
extern void DoSingleWriteBackend(httpd_conn c, int fd, bool testing);
extern SRCode RegularFileStuff(httpd_conn hc, NameEntry hotName,
			       int fileSize, int modTime);
