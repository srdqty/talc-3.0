// Used to generate the interface Popcorn code to cloader.h

extern struct seg_info {
  unsigned int address;
  unsigned int kind;
  unsigned int data[];
}

extern struct exec_info {
  unsigned int entry; // address of entry point
  unsigned int data_size; // size of the data segments
  seg_info segments[];
}

extern exec_info load_exec(Cstring fname);

