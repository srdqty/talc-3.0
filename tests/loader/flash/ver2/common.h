extern void PanicBack(string name, int line, string temp);
#define Panic(x) PanicBack(__FILE__, __LINE__, x)
