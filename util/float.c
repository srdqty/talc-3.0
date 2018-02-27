#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <stdlib.h>

#define FLOATSIZE  4
#define DOUBLESIZE 8
/* 

float to_float(value v) {
  CAMLparam1(v);
  {
    int i;
    float f;
    char *s = (char *)&f;
    
    for (i=0; i<FLOATSIZE; i++) {
      s[i] = Byte(v,i);
    }
    
    CAMLreturn(f);
  }
}

double to_double(value v) {
  CAMLparam1(v);
  {
    int i;
    double d;
    char *s = (char*)&d;
    
    for (i=0; i<DOUBLESIZE; i++) {
      s[i] = Byte(v,i);
    }
    
    CAMLreturn(d);
  }
}

value d_to_h32(value digits) {
  CAMLparam1(digits);
  {
    float f = (float)(atof(String_val(digits))); 
    value v = alloc_string(FLOATSIZE);
    
    from_float(f,v);
    
    CAMLreturn(v);
  }
}

value d_to_h64(value digits) {
  CAMLparam1(digits);
  {
    double d = (double)(atof(String_val(digits)));
    value v = alloc_string(DOUBLESIZE);
    
    from_double(d,v);
    
    CAMLreturn(v);
  }
}

value negate_f32(value v) {
  CAMLparam1(v);
  {
    float f = to_float(v);
    CAMLlocal1(v2);
    
    v2 = alloc_string(FLOATSIZE);
    from_float(-f,v2);

    CAMLreturn(v2);
  }
}

value negate_f64(value v) {
  CAMLparam1(v);
  {
    double d = to_double(v);
    CAMLlocal1(v2);
    
    v2 = alloc_string(DOUBLESIZE);
    from_double(-d,v2);
    
    CAMLreturn(v2);
  }
}

*/


void from_float(float f, value v) {
  CAMLparam1(v);
  {
    int i;
    char *s = (char *)&f;
    
    for (i=0; i<FLOATSIZE; i++) {
      Byte(v,i) = s[i];
    }
    
    CAMLreturn0;
  }
}

void from_double(double d, value v) {
  CAMLparam1(v);
  {
    int i;
    char *s = (char *)&d;
    
    for (i=0; i<DOUBLESIZE; i++) {
      Byte(v,i) = s[i];
    }
    
    CAMLreturn0;
  }
}

value dec_to_float(value s) {
  CAMLparam1(s); {
    double d = (double)(atof(String_val(s)));

    CAMLreturn(copy_double(d));
  }
}

value float_to_dec(value f) {
  CAMLparam1(f); {
    double d = Double_val(f);

    char temp[80] = {0};
    sprintf(temp,"%e",d);
    
    CAMLreturn(copy_string(temp));
  }
}

value float_to_f32(value d) {
  CAMLparam1(d); {
    float f = (float) Double_val(d);
    CAMLlocal1(f32);
    f32 = alloc_string(FLOATSIZE);

    from_float(f,f32);

    CAMLreturn(f32);
  }
}

value float_to_f64(value f) {
  CAMLparam1(f); {
    double d = Double_val(f);
    CAMLlocal1(f64);
    f64 = alloc_string(DOUBLESIZE);
    
    from_double(d,f64);
    CAMLreturn(f64);
  } 
}

value f32_to_float(value f32) {
  CAMLparam1(f32); {
    int i;
    float f;
    char *s = (char *)&f;
    
    for (i=0; i<FLOATSIZE; i++) {
      s[i] = Byte(f32,i);
    }
    
    CAMLreturn(copy_double((double)f));
  }
}

value f64_to_float(value f64) {
  CAMLparam1(f64);
  {
    int i;
    double d;
    char *s = (char*)&d;
    
    for (i=0; i<DOUBLESIZE; i++) {
      s[i] = Byte(f64,i);
    }
    
    CAMLreturn(copy_double(d));
  }
}

