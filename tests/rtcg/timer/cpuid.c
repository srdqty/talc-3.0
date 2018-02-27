
#include <stdio.h>

#define FLAG_PR(X,Y) if(X) printf("\t%s\n",Y)
#define TLB_PR(X,Y,Z,W) printf("\t(  TLB) %12s, %5s pages, %d-way set associative, %2d entries\n",X,Y,Z,W)
#define CACHE_PR(X,Y,Z,W) printf("\t(CACHE) %12s, %5s      , %d-way set associative, %2d line size\n",X,Y,Z,W)

#define CPUID __asm __emit 0fh __asm __emit 0a2h

unsigned get_bits(unsigned word, int offset, int len) {
  unsigned mask;

  word = (word >> offset);
  mask = ((unsigned)(~0)) >> (32 - len);
  word = word & mask;

  return word;
}

typedef struct {
  unsigned c1;
  unsigned c2;
  unsigned c4;
  unsigned c3;
  unsigned dummy;
} cpuid_s, *cpuid_t;

cpuid_s read_cpuid(int input) {
  unsigned c1,c2,c3,c4;
  cpuid_s c = {0};

#ifdef __linux__
  __asm__ __volatile__("\tpushal\n"
		       "\tmovl %4,%%eax\n"
		       "\tcpuid\n"
		       "\tmovl %%eax,%0\n"
		       "\tmovl %%ebx,%1\n"
		       "\tmovl %%ecx,%2\n"
		       "\tmovl %%edx,%3\n"
		       "\tpopal\n" :
		       "=m" (c1), "=m" (c2), "=m" (c3), "=m" (c4) : "m" (input));
#else
  __asm {
    pushad
      __asm mov eax, input
      CPUID
      __asm mov c1,eax
      __asm mov c2,ebx
      __asm mov c3,ecx
      __asm mov c4,edx
      popad
      }
#endif
 
  c.c1 = c1; c.c2 = c2; c.c3=c3; c.c4 = c4;

  return c;
}

void print_cpuid(int input, cpuid_s c) {
  printf("CPUID(%d) = %08x : %08x : %08x : %08x\n",input,c.c1,c.c2,c.c3,c.c4);
}

void interpret_byte(int byte) {
  switch(byte) {
  case 0x00: return;
  case 0x01: TLB_PR("Instruction", "4KB", 4, 32); return;
  case 0x02: TLB_PR("Instruction", "4MB", 4, 4);  return;
  case 0x03: TLB_PR("Data", "4KB", 4, 64); return;
  case 0x04: TLB_PR("Data", "4MB", 4, 8); return;
  case 0x06: CACHE_PR("Instruction","8KB",4,32); return;
  case 0x08: CACHE_PR("Instruction","16KB",4,32); return;
  case 0x0A: CACHE_PR("Data","8KB",2,32); return;
  case 0x0C: CACHE_PR("Data","16KB",2,32); return;
  case 0x41: CACHE_PR("Unified","128KB",4,32); return;
  case 0x42: CACHE_PR("Unified","256KB",4,32); return;
  case 0x43: CACHE_PR("Unified","512KB",4,32); return;
  case 0x44: CACHE_PR("Unified","1MB",4,32); return;
  case 0x45: CACHE_PR("Unified","2MB",4,32); return;
  case 0x50: TLB_PR("Instruction","4KB",4,64); return;
  case 0x51: TLB_PR("Instruction","4KB",4,128); return;
  case 0x52: TLB_PR("Instruction","4KB",4,256); return;
  case 0x5b: TLB_PR("Data","4KB",4,64); return;
  case 0x5c: TLB_PR("Data","4KB",4,128); return;
  case 0x5d: TLB_PR("Data","4KB",4,256); return;
  case 0x66: CACHE_PR("data","8KB",4,64); return;
  case 0x67: CACHE_PR("data","16KB",4,64); return;
  case 0x68: CACHE_PR("data","32KB",4,64); return;
  case 0x70: CACHE_PR("trace","12Ku",8,0); return;
  case 0x71: CACHE_PR("trace","16Ku",8,0); return;
  case 0x72: CACHE_PR("trace","32Ku",8,0); return;
  case 0x79: CACHE_PR("unified","128KB",8,64); return;
  case 0x7A: CACHE_PR("unified","256KB",8,64); return;
  case 0x7B: CACHE_PR("unified","512KB",8,64); return;
  case 0x7C: CACHE_PR("unified","1MB",8,64); return;
  case 0x82: CACHE_PR("unified","256KB",8,32); return;
  case 0x84: CACHE_PR("unified","1MB",8,32); return;
  case 0x85: CACHE_PR("unified","2MB",8,32); return;
  }
  printf("\tUnrecognized cache or tlb : %2x\n",byte);
}

void interpret_word(int count, unsigned word) {

  if(get_bits(word,31,1)) {
    char * s;
    switch(count) {
    case 1: s = "EAX"; break;
    case 2: s = "EBX"; break;
    case 3: s = "ECX"; break;
    case 4: s = "EDX"; break;
    default: s = "???"; break;
    }

    printf("\tInvalid descriptor in %s\n",s);
    return; /* Invalid descriptor. */
  }

  if(count!=1) interpret_byte(get_bits(word,0,8));
  interpret_byte(get_bits(word,8,8));
  interpret_byte(get_bits(word,16,8));
  interpret_byte(get_bits(word,24,8));

}
int main() {
  int max_input,i;

  cpuid_s c = read_cpuid(0);
  print_cpuid(0,c);

  max_input = c.c1;
  printf("Maximum input value = %d \n",c.c1);
  printf("Id string = %s \n\n",(char *)(&(c.c2)));

  if(max_input>=1) {
    int type, family, model,step;
    int fpu,vme,de,pse,tsc,msr, pae, mce,cx8,apic,mtrr,pge,mca,cmov,mmx;
    c = read_cpuid(1);
    print_cpuid(1,c);
    printf("\n");

    step   = get_bits(c.c1, 0,4);
    model  = get_bits(c.c1, 4,4);
    family = get_bits(c.c1, 8,4);
    type   = get_bits(c.c1,12,2);
    
    printf("Processor Type = %x, Family = %x, Model = %x, Step = %x\n",
	   type,family,model,step);

    fpu  = get_bits(c.c4,0,1);
    vme  = get_bits(c.c4,1,1);
    de   = get_bits(c.c4,2,1);
    pse  = get_bits(c.c4,3,1);
    tsc  = get_bits(c.c4,4,1);
    msr  = get_bits(c.c4,5,1);
    pae  = get_bits(c.c4,6,1);
    mce  = get_bits(c.c4,7,1);
    cx8  = get_bits(c.c4,8,1);
    apic = get_bits(c.c4,9,1);
    mtrr = get_bits(c.c4,12,1);
    pge  = get_bits(c.c4,13,1);
    mca  = get_bits(c.c4,14,1);
    cmov = get_bits(c.c4,15,1);
    mmx  = get_bits(c.c4,23,1);

    printf("Processor supports: \n");
    FLAG_PR( fpu, "on-chip fpu");
    FLAG_PR( vme, "virtual mode enhancements");
    FLAG_PR(  de, "debugging extensions");
    FLAG_PR( pse, "page size extensions");
    FLAG_PR( tsc, "time stamp counter");
    FLAG_PR( msr, "model specific registers");
    FLAG_PR( pae, "physical address extension");
    FLAG_PR( mce, "machine check exception");
    FLAG_PR( cx8, "CMPXCHG8B instruction");
    FLAG_PR(apic, "advance programmable interrupt controller");
    FLAG_PR(mtrr, "memory type range registers");
    FLAG_PR( pge, "PTE global flag");
    FLAG_PR( mca, "machine check architecture");
    FLAG_PR(cmov, "conditional move and compare instructions");
    FLAG_PR( mmx, "MMX technology");
    printf("\n");

  }

  if(max_input >= 2) {
    int num_reads;

    c = read_cpuid(2);
    print_cpuid(2,c);
    printf("\n");

    num_reads = get_bits(c.c1,0,8);

    printf("Reads required to get all cache information = %d \n",num_reads);

    printf("Cache/TLB information:\n");
    for(i=0;i<num_reads;i++) {
      interpret_word(1,c.c1);
      interpret_word(2,c.c2);
      interpret_word(3,c.c3);
      interpret_word(4,c.c4);

      if(i != num_reads - 1) {
	c = read_cpuid(2);
	print_cpuid(2,c);
      }
    }
  }

  if(max_input > 2) {
    printf("Unrecognized processor. Can accept more than two reads.\n");
  }

  for(i=3;i<=max_input;i++) {
    c = read_cpuid(i);
    print_cpuid(i,c);
    printf("\n");
  }

  return 0;
}
