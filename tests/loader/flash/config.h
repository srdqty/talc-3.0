#ifndef _CONFIG_H_
#define _CONFIG_H_


/* The following configuration settings are sorted in order of decreasing
** likelihood that you'd want to change them - most likely first, least
** likely last.
**
** In case you're not familiar with the convention, "#ifdef notdef"
** is a Berkeleyism used to indicate temporarily disabled code.
** The idea here is that you re-enable it by just moving it outside
** of the ifdef.
*/

/* CONFIGURE: CGI programs must match this pattern to get executed.  It's
** a simple shell-style filename pattern, with ? and *, or multiple such
** patterns separated by |.  The patterns get checked against the filename
** part of the incoming URL.
**
** Restricting CGI programs to a single directory lets the site administrator
** review them for security holes, and is strongly recommended.  If there
** are individual users that you trust, you can enable their directories too.
**
** You can also specify a CGI pattern on the command line, with the -c flag.
** Such a pattern overrides this compiled-in default.
**
** If no CGI pattern is specified, neither here nor on the command line,
** then CGI programs cannot be run at all.  If you want to disable CGI
** as a security measure that's how you do it, just don't define any
** pattern here and don't run with the -c flag.
*/
#define CGI_PATTERN "/cgi-bin/*"
#ifdef notdef
#define CGI_PATTERN "/cgi-bin/*"
#define CGI_PATTERN "/cgi-bin/*|/jef/*"
#define CGI_PATTERN "*.cgi"
#define CGI_PATTERN "*"
#endif

/* CONFIGURE: How many seconds to allow CGI programs to run before killing
** them.  This is in case someone writes a CGI program that goes into an
** infinite loop, or does a massive database lookup that would take hours,
** or whatever.  If you don't want any limit, comment this out, but that's
** probably a really bad idea.
*/
#define CGI_TIMELIMIT 500

/* CONFIGURE: How many seconds before an idle connection gets closed.
*/
#define IDLEC_TIMELIMIT 500

/* CONFIGURE: Tilde mapping.  Many URLs use ~username to indicate a
** user's home directory.  thttpd provides two options for mapping
** this construct to an actual filename.
**
** 1) Map ~username to <prefix>/username.  This is the recommended choice.
** Each user gets a subdirectory in the main chrootable web tree, and
** the tilde construct points there.  The prefix could be something
** like "users", or it could be empty.  See also the makeweb program
** for letting users create their own web subdirectories.
**
** 2) Map ~username to <user's homedir>/<postfix>.  The postfix would be
** the name of a subdirectory off of the user's actual home dir, something
** like "public_html".  This is what NCSA and other servers do.  The problem
** is, you can't do this and chroot() at the same time, so it's inherently
** a security hole.  This is strongly dis-recommended, but it's here because
** some people really want it.  Use at your own risk.
**
** You can also leave both options undefined, and thttpd will not do
** anything special about tildes.  Enabling both options is an error.
*/
#ifdef notdef
#define TILDE_MAP_1 "users"
#define TILDE_MAP_2 "public_html"
#endif


/* Most people won't want to change anything below here. */

/* CONFIGURE: This controls the SERVER_NAME environment variable that gets
** passed to CGI programs.  By default thttpd does a gethostname(), which
** gives the host's canonical name.  If you want to always use some other name
** you can define it here.
**
** Alternately, if you want to run the same thttpd binary on multiple
** machines, and want to build in alternate names for some or all of
** them, you can define a list of canonical name to altername name
** mappings.  thttpd seatches the list and when it finds a match on
** the canonical name, that alternate name gets used.  If no match
** is found, the canonical name gets used.
**
** If both SERVER_NAME and SERVER_NAME_LIST are defined here, thttpd searches
** the list as above, and if no match is found then SERVER_NAME gets used.
**
** In any case, if thttpd is started with the -h flag, that name always
** gets used.
*/
#ifdef notdef
#define SERVER_NAME "your.hostname.here"
#define SERVER_NAME_LIST \
"canonical.name.here/alternate.name.here", \
"canonical.name.two/alternate.name.two"
#endif

/* CONFIGURE: Define this if you want to always chroot(), without having
** to give the -r command line flag.  Some people like this as a security
** measure, to prevent inadvertant exposure by accidentally running without -r.
*/
#ifdef notdef
#define ALWAYS_CHROOT
#endif

/* CONFIGURE: When started as root, the default username to switch to after
** initializing.  If this user (or the one specified by the -u flag) does
** not exist, the program will refuse to run.
*/
#define DEFAULT_USER "nobody"

/* CONFIGURE: When started as root, the program can automatically chdir()
** to the home directory of the user specified by -u or DEFAULT_USER.
** An explicit -d still overrides this.
*/
#ifdef notdef
#define USE_USER_DIR
#endif

/* CONFIGURE: nice(2) value to use for CGI programs.  If this is left
** undefined, CGI programs run at normal priority.
*/
#ifdef notdef
#define CGI_NICE 10
#endif

/* CONFIGURE: $PATH to use for CGI programs.
*/
#define CGI_PATH "/usr/local/bin:/usr/ucb:/bin:/usr/bin"


/* CONFIGURE: The default port to listen on.  80 is the standard HTTP port.
*/
#ifndef DEFAULT_PORTSTR
#define DEFAULT_PORTSTR "80"
#endif

/* CONFIGURE: The filename to use for index files.  This should really be a
** list of filenames - index.htm, index.cgi, etc., that get checked in order.
*/
#define INDEX_NAME "index.html"

/* CONFIGURE: The listen() backlog queue length.  The 1024 doesn't actually
** get used, the kernel uses its maximum allowed value.  This is a config
** parameter only in case there's some OS where asking for too high a queue
** length causes an error.  Note that on many systems the maximum length is
** way too small - see http://www.acme.com/software/thttpd/notes.html
*/
#define LISTEN_BACKLOG 1024

/* CONFIGURE: Maximum number of symbolic links to follow before
** assuming there's a loop.
*/
#define MAX_LINKS 32


/* --------------------- Flash defines ------------- */

/* the server maintains its own LRU cache, but it needs some estimate
  of how much effective memory to assume. Always err on the low side -
  it'll cause more work, but it'll avoid blocking */
extern int maxLRUCacheSizeBytes;

/* the server employs two sets of helper applications ("slaves") that
  perform pathname translation and data reading. These applications
  are spawned dynamically as needed, but it's useful to cap the number
  of these that can be active at once.  */
extern int maxReadHelp;
extern int maxConvHelp;
extern int maxDirHelp;

/* although slaves are forked dynamically as needed, we can opt
   to start a few when the server is started */
extern int initReadHelp;
extern int initConvHelp;

/* for files larger than the OS's default sendbuf size, the server
   does a setsockopt to increase the sendbuf size. On some systems,
   values larger than 64k bytes are illegal or cause weirdess */
extern int sendBufSizeBytes;

/* number of non-helper file descriptors to reserve for uses other
   than connections.  Currently the 5 non-helper connections are:

   - the listen socket
   - the maintenance socket
   - stderr
   - syslog
   - 2 for slop 
   */
#define SPARE_FDS 6

/* how many fd's we want. the maximum number of connections will be
   about half of this number
   
   IMPORTANT: be sure to include the config.h file early for any source
   that really needs FD_SETSIZE */
/* #ifndef __linux__ */
#ifndef FD_SETSIZE
#define FD_SETSIZE 1024
#endif
/* #endif *//* __linux__ */

/* we can choose to aggressively start slaves whenever they are needed,
   or wait for a certain number of seconds and see if the slave queues
   are still busy - the latter prevents quick bursts from starting large
   numbers of slave processes */
extern int slaveDelayTime;

/* slaves can decide to close after being idle for a certain amount of
   time if we want. The actual mechanism for doing this involves the slave
   telling the master it's idle, and the master closing it. If we want
   slaves to stick around forever until the server process closes, set this
   value to zero. Otherwise, specify number of seconds to stay idle before
   asking to quit */
extern int maxSlaveIdleTime;


/* when the server is idle for two seconds, we assume that the recent tests
   are over, and we can use this time to show statistics about what happened.
   These statistics are always collected, but only printed if requested */
extern int doMainStats;

/* when the server prints its idle "recent" statistics during idle time,
   we can get information about the lengths in various hash bins. This
   is useful for debugging, but not much else. */
extern int doQueueLenDumps;

/* if mincore says the page isn't in memory, it's sometimes useful to
   see exactly what page in the chunk is missing - good for testing
   and debugging */
extern int doMincoreDump;

/* the name cache has a number of non-active entries, and this variable
   controls how many are allowed. Only applies to name cache (not CGIs)
   currently
   */
extern int maxNameCacheSize;

/* if accesses are being logged, this flag is set */
extern bool accessLoggingEnabled;

/* when reading data from files, we operate on fixed-size blocks -
   block size should be large enough to be useful, since it'll determine
   how large auxiliary structures are, and it'll affect disk read times */
#define READBLOCKSIZE 65536

extern int systemPageSize;
extern int systemPageSizeBits;

#endif /* _CONFIG_H_ */

