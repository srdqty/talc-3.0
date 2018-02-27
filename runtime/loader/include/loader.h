#ifndef __LOADER_H__
#define __LOADER_H__

extern void *load(char *buf, int buflen);
extern void register_symbol(unsigned long value, unsigned long name);

#endif
