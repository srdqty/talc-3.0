extern union event {
  void Tick;    /* some amount of time has passed */
  string Load;  /* load the given file from the filesystem */
}

extern string event2string (event e);
extern event string2event (string s);
