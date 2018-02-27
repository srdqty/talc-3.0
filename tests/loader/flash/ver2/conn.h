#ifndef __CONN_H
#define __CONN_H

/* 
#include <sys/param.h>
#include <sys/types.h>
#include <netinet/in.h>
*/

extern DataEntry?;
extern HeaderInfoStruct?;

/* extern HotNameEntry?; */
extern NameEntry?;

extern ?struct httpd_conn {
  int hc_cnum;
  int /* inet_addr */ hc_clientAddr;
  int hc_method;
  int hc_status;
  
  /* these come from the very first header line */
  string hc_encodedurl;
  string hc_protocol;
  
  /* HotNameEntry hc_hne; */
  NameEntry hc_hne;

  /* these are "simple" header lines - they optionally get
     filled from the contents of the request header, and there
     are not supposed to be multiple of them. */
  string hc_cookie;
  string hc_contentType;
  string hc_referer;
  string hc_userAgent;
  
  /* these are complicated header lines - the header may
     contain multiple entries which are supposed to be
     concatenated */
  string hc_accept;
  string hc_accepte;
  
  int hc_ifModifiedSince;
  int hc_contentLength;
  bool hc_mimeFlag;
  int hc_fd;
  bool hc_isPersistentConnection;
  HeaderInfoStruct hc_headerInfo;
  /* CGIInfo *hc_cgiInfo; */
  /* CacheEntry hc_cacheEnt; */
  DataEntry hc_dataEnt;
  /* DirStageBuf *hc_dsb; */	/* staging buf for doing 'ls' */
  int hc_expirationTime;
  void hc_expireFunc(httpd_conn);
  int hc_timerPrivate;		/* to be used by expireFunc if it wants */
  int hc_sndbuf;
  int hc_bytesSent;
  /* int hc_numChunkLocks; */		/* have we set a chunk lock? */
  
  /* httpd_conn hc_siblings; */	/* used in convert */
  /* httpd_conn hc_next; */	/* used in convert */
  string hc_stripped;		/* used in convert */
  /* int hc_asyncByteRead; */	/* used in async reading */
  /* int hc_asyncReadLen; */	/* used in async reading */
  bool hc_nagleOff;		/* has nagle been turned off already */

  /* string hc_origFirstLine; *//* text of first line sent by client */
  /* int hc_origFirstLineLen; *//* length of first line text */
  /* bool hc_neededDiskRead; */	/* set on per-req basis if read needed */
  /* int hc_hadMincoreMisses;*/	/* set on per-req basis if mincore failed */
}

/* size of this structure in memory words---#fields times 4 */
#define SIZEOF_HTTPD_CONN (33 * 4)

/* keep these numbered from zero, in order of frequency */
#define METHOD_GET 0
#define METHOD_HEAD 1
#define METHOD_POST 2
#define METHOD_ERROR 3
#define NUM_METHODS 4

extern int numConnects;
extern int maxConnects;
extern int firstFreeConnHint;
extern char freeConnBits[];
extern httpd_conn allConnects[];
#define ISCONNFREE(x) ((freeConnBits[(x)>>3] & (1<<((x)&7))) != 0)

#define MakeConnFDAssociation(hc, fd)  {fdToConnMap[(fd)] = (hc.hc_cnum);}
#define ClearConnFDAssociation(fd) {fdToConnMap[(fd)] = -1;}

extern int fdToConnMap[];

#endif
