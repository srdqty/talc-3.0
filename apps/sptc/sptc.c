/* C wrappers to ptc library. */


// #include "ptc.h"

#define NULL 0
#define PTCAPI __stdcall
/* ptc.h includes inlined explicitly to eliminate dependence on ptc source. */

typedef int ptc_int32;
typedef void* PTC_KEY;          /* equivalent to C++ 'Key*'         */
typedef void* PTC_AREA;         /* equivalent to C++ 'Area*'        */
typedef void* PTC_FORMAT;       /* equivalent to C++ 'Format*'      */
typedef void* PTC_PALETTE;      /* equivalent to C++ 'Palette*'     */
typedef void* PTC_SURFACE;      /* equivalent to C++ 'BaseSurface*' */
typedef void* PTC_CONSOLE;      /* equivalent to C++ 'BaseConsole*' */

int PTCAPI ptc_key_code(PTC_KEY object);
int PTCAPI ptc_key_alt(PTC_KEY object);
int PTCAPI ptc_key_shift(PTC_KEY object);
int PTCAPI ptc_key_control(PTC_KEY object);

PTC_AREA PTCAPI ptc_area_create(int left,int top,int right,int bottom);

PTC_FORMAT PTCAPI ptc_format_create_indexed(int bits);

PTC_PALETTE PTCAPI  ptc_palette_create();
ptc_int32* PTCAPI  ptc_palette_lock(PTC_PALETTE object);
void  PTCAPI ptc_palette_unlock(PTC_PALETTE object);

PTC_SURFACE PTCAPI  ptc_surface_create(int width,int height,PTC_FORMAT format);
void  PTCAPI ptc_surface_destroy(PTC_SURFACE object);
void* PTCAPI ptc_surface_lock(PTC_SURFACE object);
void  PTCAPI ptc_surface_unlock(PTC_SURFACE object);
void  PTCAPI ptc_surface_copy_area(PTC_SURFACE object,PTC_SURFACE surface,
				   PTC_AREA source,PTC_AREA destination);
void  PTCAPI ptc_surface_palette_set(PTC_SURFACE object,PTC_PALETTE palette);

PTC_CONSOLE PTCAPI ptc_console_create();
void PTCAPI ptc_console_open_resolution(PTC_CONSOLE object,char *title,
					int width,int height,
					PTC_FORMAT format,int pages);
void PTCAPI ptc_console_destroy(PTC_CONSOLE object);
void PTCAPI ptc_console_update(PTC_CONSOLE object);
void PTCAPI ptc_console_palette_set(PTC_CONSOLE object,PTC_PALETTE palette);
int  PTCAPI ptc_console_key(PTC_CONSOLE object);
void PTCAPI ptc_console_read(PTC_CONSOLE object,PTC_KEY key);

extern void *GC_malloc(int size);

typedef struct { int size; char *data; } pop_string;
typedef struct { int size; int *data; } pop_int_array;

static PTC_CONSOLE console;
static PTC_SURFACE surface;
static int is_open;
static int is_locked; // Is the surface locked?
static int width;
static int height;

pop_string empty_string = {0,0};

int sptc_open(int w,int h) {
  PTC_FORMAT f;
  /* XXX No title for now. */

  if(is_open) return 0;

  width = w;
  height = h;

  f = ptc_format_create_indexed(8);

  if(f==NULL) return 0;

  console = ptc_console_create();

  if(console==NULL) return 0;

  ptc_console_open_resolution(console,"",w,h,f,0);

  surface = ptc_surface_create(w,h,f);

  if(surface==NULL) return 0;

  is_open = 1;
  return 1; /* return true on success. */
}

void sptc_close() {
  if (!is_open) return; /* Its not open, do nothing. */

  ptc_console_destroy(console);
  ptc_surface_destroy(surface);
  is_open = 0;
}

pop_string *sptc_pixels() {
  pop_string *result;

  if(!is_open) return &empty_string;

  result = (pop_string *)GC_malloc(sizeof(pop_string));

  result->size = width*height;
  result->data = ptc_surface_lock(surface);

  is_locked=1;

  return result;
}

void sptc_update() {
  PTC_AREA a;
  if(!is_open) return;

  if(!is_locked) {
    ptc_surface_lock(surface);
    is_locked=1;
  }

  ptc_surface_unlock(surface);
  is_locked=0;

  a=ptc_area_create(0,0,width,height);

  ptc_surface_copy_area(surface,console,a,a);

  ptc_console_update(console);

}

int sptc_set_palette(pop_int_array *p) {
  PTC_PALETTE pal;
  int *pal_data;
  int i;

  if(!is_open) return 0;

  if(p==NULL || p->size!=256) return 0;

  pal = ptc_palette_create();

  if(pal==0) return 0;

  pal_data = ptc_palette_lock(pal);

  for(i=0;i<256;i++) {
    pal_data[i]=p->data[i];
  }

  ptc_palette_unlock(pal);

  ptc_console_palette_set(console,pal);
  ptc_surface_palette_set(surface,pal);

  return 1;
}

int sptc_keyhit() {
  return (ptc_console_key(console) ? 1 : 0);
}

int sptc_read() {
  PTC_KEY k=0;
  int result;

  ptc_console_read(console,k);

  if(k==NULL) return 0;

  result = ptc_key_code(k);
  if(ptc_key_alt(k))     result |= (1<<31);
  if(ptc_key_shift(k))   result |= (1<<30);
  if(ptc_key_control(k)) result |= (1<<29);

  return result;
}
