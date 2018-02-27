/* Version of hotname.h but without stuff for caching.  This is
   the first rev of the server, so we ditched this stuff, to be added
   as updates later. */

extern union NameTypes {
  void HNT_FILE;
  void HNT_REDIRECT;
  void HNT_LS;
  void NUM_HNT;
}

extern ?struct NameEntry {
  string hne_encoded;		/* the original URL */
  string hne_stripped;		/* decoded, void of query or pathinfo */
  string hne_expanded;		/* symlinks expanded */
  NameTypes hne_type;
  int hne_modTime;		/* currently used only by "ls" */
}

extern NameEntry GetEmptyNE();
