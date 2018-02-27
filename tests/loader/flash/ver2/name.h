/* Version of hotname.h but without stuff for caching.  This is
   the first rev of the server, so we ditched this stuff, to be added
   as updates later. */

extern union NameTypes {
  void HNT_FILE;
  void HNT_REDIRECT;
  void HNT_LS;
}

extern ?struct NameEntry {
  int hne_refCount;
  string hne_encoded;		/* the original URL */
  int hne_time;			/* when this entry was entered */
  NameEntry hne_next;
  NameEntry hne_prev;
  int hne_bin_index;		/* hotNames[] index of this entry */
  string hne_stripped;		/* decoded, void of query or pathinfo */
  string hne_expanded;		/* symlinks expanded */
  NameTypes hne_type;
  int hne_modTime;		/* currently used only by "ls" */
  NameEntry hne_nextMRU;	/* if refcount is zero, it's on MRU list */
  NameEntry hne_prevMRU;
}

extern NameEntry GetEmptyNE();
extern void EnterIntoHotNameCache(*(NameEntry) tempP);
extern NameEntry FindMatchInHotNameCache(string matchName);
extern void ReleaseHNE(NameEntry hne);
