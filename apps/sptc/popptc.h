// popptc.h
// Popcorn header for the comple PTC library.  
// This got too complicated so I have abandoned it for now.

// All header files mushed into one.
// Make use of abstract types Carray and Cstring.

#ifndef __POP_PTC_H
#define __POP_PTC_H

prefix ptc {
  open ptc;
// C/Index.h
  extern key;
  extern area;
  extern mode;
  extern copy;
  extern clear;
  extern timer;
  extern error;
  extern color;
  extern format;
  extern palette;
  extern surface;
  extern console;

  // C/Key.h  
  // *** Setup ***
  key key_create(int code, int alt, int shift, int control);
  void key_destroy(key object);

  // *** Key Code ***
  key_code(key object);

  // *** modifiers ***
  int key_alt(key object);
  int key_shift(key object);
  int key_control(key object);
  
  // *** operators ***
  void key_assign(key object, key k);
  int key_equals(key object, key k);

  // *** key code ***
#include "keyCodes.h"

  // C/Area.h
  
  // *** Setup ***
  area area_create(int left, int top, int right, int bottom);
  void area_destroy(area object);

  // *** Data Access ***
  int area_left(area o);
  int area_right(area o);
  int area_top(area o);
  int area_bottom(area o);
  int area_width(area o);
  int area_height(area o);

  // *** operators ***
  void area_assign(area o, area a);
  int  area_equals(area o, area a);
  
  // C/Copy.h

  // *** Setup ***
  copy copy_create();
  void copy_destroy(copy o);

  // *** Set source and destination formats ***
  void copy_request(copy o, format src, format dest);
  void copy_palette(copy o, palette src, palette dest);
  
  // *** copy pixels ***
  /* XXX void copy_copy(copy object, <int>Carray source_pixels, int source_x, int source_y,
		 int source_width, int source_height,int source_pitch,
		 <int>Carray destination_pixels,int destination_x, int destination_y,
		 int destination_width,int destination_height, 
		 int destination_pitch); */

  // *** copy option string *** 
  int copy_option(copy o, Cstring option);

  // C/Mode.h
  
  // *** Setup ***
  mode mode_create(int width,int height, format f);
  mode mode_create_invalid();
  mode mode_destroy(mode o);

  // *** valid mode flag ***
  int mode_valide(mode o);

  // *** data access ***
  int mode_width(mode o);
  int mode_height(mode o);
  format mode_format(mode o);

  // *** operators ***
  void mode_assign(mode o,mode m);
  int mode_equals(mode o, mode m);

  // C/Clear.h
  
  // *** Setup ***
  clear clear_create();
  void clear_destroy(clear o);

  // *** Request Clear ***
  void clear_request(clear o, format f);
  
  // *** Clear Pixels ***
  /* void clear_clear(clear o, <int>Carray pixels, int x, int y, int w, int h, int p,
     color c); XXX */

  // C/Color.h
  
  // *** Setup ***
  color color_create();
  color color_create_indexed(int index);
  //color color_create_direct(float r, float g, float b,float a); // XXX
  void color_destroy(color o);

  // *** Data Access ***
  int color_index(color o);
  // float color_r(color o); // XXX
  // float color_g(color o); // XXX
  // float color_b(color o); // XXX
  // float color_a(color o); // XXX
  
  int color_direct(color o);
  int color_indexed(color o);

  // *** Operators ***
  void color_assign(color o, color c);
  int color_equals(color o, color c);

  // C/Error.h

  // *** Setup ***
  error error_create(Cstring msg);
  error error_create_composite(Cstring msg, error e);
  void error_destroy(error o);

  // *** Report Error ***
  void error_report(error o);
  
  // *** Get Error Message ***
  Cstring error_message(error o);

  // *** Operators ***
  void error_assign(error o, error e);
  void error_equals(error o, error e);

  // *** Error management ***
  void error_handler(void handler(error));

  // C/Timer.h

  // *** Setup ***
  timer timer_create();
  void timer_destroy(timer o);
  
  // *** Set Time ***
  // void timer_set(timer o,double time); // XXX
  
  // *** Control ***
  void timer_start(timer o);
  void timer_stop(timer o);

  // *** Time Data ***
  // double timer_time(timer o);
  // double timer_delta(timer o);
  // double timer_resoltuion(timer o);

  // *** Operators ***
  void timer_assign(timer o, timer t);
  void timer_equals(timer o, timer t);

  // C/Format.h
  
  // *** Setup ***
  format format_create();
  format format_create_indexed(int bits);
  format format_create_direct(int bits, int r, int g, int b, int a);
  void format_destroy(format o);

  // *** Data Access ***
  int format_r(format o);
  int format_g(format o);
  int format_b(format o);
  int format_a(format o);
  int format_bits(format o);
  int format_bytes(format o);
  int format_direct(format o);
  int format_indexed(format o);

  // *** Operators ***
  void format_assign(format o, format f);
  void format_equals(format o, format f);

  // C/Clipper.h
  
  // *** Clip a single area against clip area ***
  void clipper_clip(area a,area clip, area clipped);
  
  // *** Clip source and destination area against source and destination clip areas ***
  void clipper_clip_complex(area src,are clip_src,area clipped_src,
			    area dest, area clip_dest, area clipped_dest);

  // C/Palette.h
  // Palette's always contain 256 entries in RGB format.
  
  // *** Setup ***
  palette palette_create();
  /* palette palette_create_data(<int>Carray data); XXX */
  void palette_destroy(palette o);

  // *** Memory Access ***
  // <int>Carray palette_lock(palette o); // XXX
  void palette_unlock(palette o);

  // *** Load palette data ***
  // void palette_load(palette o, <int>Carray data);
  
  // *** Save palette data ***
  // void palette_save(palette o, <int>Carray data);

  // *** Get palette data ***
  // <int>Carray palette_data(palette o);

  // *** Operators ***
  void palette_assign(palette o, palette p);
  int palette_equals(palette o, palette p);


  // C/Surface.h
  
  // *** Setup ***
  surface surface_create(int w, int h, format f);
  void surface_destroy(surface o);

  // *** Copy To Surface ***
  void surface_copy(surface o, surface s);
  void surface_copy_areas(surface o, surface s, area src, area dest);
  
  // *** Memory Access ***
  // <int>Carray surface_lock(surface o); // XXX
  void surface_unlock(surface o);

  // *** Load pixels to surface ***
  /* void surface_load(surface o, <int>Carray pixels, int w, int h, int p, 
     format f, palette p); XXX */
  /* void surface_load_area(surface o, <int>Carray>, int w, int h, int p, 
     format f, palette p, area src, area dest); XXX */

  // *** Save surface pixels ***
  /* void surface_save(surface o, <int>Carray pixels, int w, int h, int p, 
     format f, palette pal); XXX */
  /* void surface_save_area(surface o, <int>Carray pixels, int w, int h, int p,
     format f, palette pal, area src, area dest); XXX */

  // *** Clear Surface ***
  void surface_clear(surface o);
  void surface_clear_color(surface o, color c);
  void surface_clear_color_area(surface o, color c, area a);
  
  // *** Surface Palette ***
  void surface_palette_set(surface o, palette p);
  palette surface_palette_get(surface o);

  // *** Surface Clip Area ***
  void surface_clip_set(surface o, area a);

  // *** Data Access ***
  int surface_width(surface o);
  int surface_height(surface o);
  int surface_pitch(surface o);
  
  area surface_area(surface o);
  area surface_clip(surface o);
  format surface_format(surface o);

  // *** Surface option string ***
  int surface_option(surface o, Cstring opt);

  // C/Console.h

  // *** Setup ***
  console console_create();
  void console_destroy(console o);

  // *** Console configuration ***
  void console_configure(console o, Cstring file);
  
  // *** Console option string ***
  int console_option(console o, Cstring opt);

  // *** Console Modes ***
  mode console_mode(console o,int index);

  // *** Console Management ***
  void console_open(console object, Cstring title, int pages);
  void console_open_format(console object, Cstring title, format f, int pages);
  void console_open_resolution(console o, Cstring title, int w, int h, 
			       format f, int pages);
  void console_open_mode(console o, Cstring title, mode m, int pages);
  void console_close(console o);

  // *** Synchronization ***
  void console_flush(console o);
  void console_finish(console o);
  void console_update(console o);
  void console_update_area(console o, area a);

  // *** Keyboard input ***
  int console_key(console o);
  void console_read(console o, key k);

  // *** Copy to surface ***
  void console_copy(console o, surface s);
  void console_copy_area(console o, surface s, area src, area dest);

  // *** Memory Access ***
  // <int>Carray console_lock(console o); // XXX
  void console_unlock(console o);

  // *** Load pixels to Console ***
  /* void console_load(console object, <int>Carray pixels, int w, int h, int p,
     format f, palette pal); XXX */
  /* void console_load_area(console o, <int>Carray p, int w, int h, int p,
     format f, palette pal, area src, area dest); XXX */
  
  // *** Save Console Pixels ***
  /* void console_save(console o, <int>Carray pixels, int w, int h, int p,
     format f, palette pal); // XXX */
  /* void console_save_area(console o, <int>Carray p, int w, int h, int p,
     format f, palette p, area src, area dest); // XXX */

  // *** Clear Console ***
  void console_clear(console o);
  void console_clear_color(console o, color c);
  void console_clear_color_area(console o, color c, area a);
  
  // *** Console Palette ***
  void console_palette_set(console o, palette p);
  palette console_palette_get(console o);

  // *** Console clip area ***
  void console_clip_set(console o, area a);

  // *** Data Access ***
  int console_width(console o);
  int console_height(console o);
  int console_pages(console o);
  int console_pitch(console o);
  area console_area(console o);
  area console_clip(console o);
  format console_format(console o);
  Cstring console_name(console o);
  Cstring console_title(console o);
  Cstring console_information(console o);

  // FMS: Skipped extension functions in C/Console.h

  // FMS: Skipped Extra.h -- seems unnecessary.
}
#endif
