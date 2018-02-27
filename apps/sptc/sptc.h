// SPTC version 0.1
// An extremely simple graphics library.
// January 2000
// Fred Smith

// Clears to black a display area of width*height pixels centered on the screen
// Returns false on failure
// Must be closed before another open can succeed.
extern bool sptc_open(int width,int height); 

// Has no effect if called before open. Otherwise closes the current display
extern void sptc_close();

// A palette is an array of 256 32-bit integers.
// Each integer has format ARGB where A=alpha, R=red, G=green, B=blue
// The array passed in must have length 256.
// Returns false on failure.
extern bool sptc_set_palette(int[]);

// Returns a string where each byte represents a pixel on the screen.
// Bytes go from left-to-right top-to-bottom.
// Returns the empty string on failure.
// Updates to the pixels are buffered -- they will not show up on the screen.
extern string sptc_pixels(); 

// Update the screen with the string returned by the last_call to sptc_pixels.  
extern void sptc_update();

// Keyboard input

#define PLAIN_KEY(X) ((X) & 0xFFF)
#define ALT_KEY(X) (((X) & (1<<31)) != 0)
#define SHIFT_KEY(X) (((X) & (1<<30)) != 0)
#define CTRL_KEY(X) (((X) & (1<<29)) != 0)

// Return true if a key was hit.
// Does not block!
extern bool sptc_keyhit();

// Blocks until a key is pressed, and reads the value.
// keyCodes.h explains the values of the key.
// Encode alt, shift, and control in higher order bit.
extern int sptc_read();

