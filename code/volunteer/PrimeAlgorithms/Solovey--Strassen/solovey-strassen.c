#include <stdio.h>
#include <stdlib.h>
#include<time.h>

/* ƒ‹[ƒv‰ñ” L */
#define LOOP 10

/* •Ô’l 0: not prime, 1: prime */
long int isPrimeSS(long int p);
long int input() {
  long int val; char buf[100]={0};
  printf("’Tõ‚·‚é‘f”‚Ì”ÍˆÍ‚ğŒˆ‚ß‚Ä‰º‚³‚¢i®”‚Å)\n");
  fgets(buf, 50, stdin);
  sscanf(buf, "%ld", &val);
  return val;
}

main() {
  long int p = 0, flag = 0; /* flag=1 ‚È‚ç‘f” */
  float start,end;  
  while(p <= 1) p = input();
  start=((float)clock())/CLOCKS_PER_SEC;
  srand(p);
  if(p == 2)
    flag = 1;
  else
  flag = isPrimeSS(p);
  printf("p = %d is %s.\n", p, flag ? "prime" : "not prime");
  end=(((float)clock())/CLOCKS_PER_SEC)-start;
  printf("Œo‰ßŠÔ%.5f\n",end);
}

/* Š|‚¯Z‚Ì•MZ */
long int calcMul(long int x, long int y, long int z) {
  long int i, v = 0;
  if(x < y) {i = x; x = y; y = i;}
  if(y == 1) return x;
  for(i = 31; i >= 0; i--) {
    v = (v * 2) % z;
    if((x >> i) % 2 == 1) v = (v + y) % z;
  }
  return v % z;
}

/* x^y mod z ‚ğ•½•ûæ–@‚Å‹‚ß‚é */
long int calcSqMul(long int x, long int y, long int z) {
  long int i, v = 1;
  for(i = 31; i >= 0; i--) {
    v = calcMul(v, v, z);
    if((y >> i) % 2 == 1) v = calcMul(v, x, z);
  }
  return v % z;
}

/* Solovey--Strassen–@‚ÉŠî‚Ã‚­‘f””»’è */
long int isPrimeSS(long int p) {
  long int i;
  for(i = 0; i < LOOP; i++) {
    long int a = rand() % (p -2) +2, b, y, z = 1, q = p;

    /* y = a^((p-1)/2) mod p ‚ÌZo */
    y = calcSqMul(a, (p-1)/2, p);
    if(y > 1) y -= p;

    /* z = (a/p) ‚ÌZoiƒ„ƒRƒr”‚ÌZoj */
    while(a > 2) {
      if(a % 2 == 0) {
        a /= 2;
        if(q % 8 != 1 && q % 8 != 7) z *= -1;
      }
      else {
        if(a % 4 == 3 && q % 4 == 3) z *= -1;
        b = a; a = q; q = b;
        a %= q;
      }
    }
    if(a == 0) z = 0;
    else if(a == 2 && q % 8 != 1 && q % 8 != 7) z *= -1;
    if(y != z) return 0;
  }
  return 1;
}
