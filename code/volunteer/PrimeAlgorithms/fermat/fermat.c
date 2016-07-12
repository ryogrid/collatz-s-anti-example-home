#include <stdio.h>
#include <stdlib.h>
#include<time.h>

/* ƒ‹[ƒv‰ñ” L */
#define LOOP 10

/* •Ô’l 0: not prime, 1: prime */
long int isPrimeFermat(long int);

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
//  while(p <= 1) p = input();
  scanf("%d",&p);
  start=((float)clock())/CLOCKS_PER_SEC;
  srand(p);
  if(p == 2)
    flag = 1;
  else
    flag = isPrimeFermat(p);
  printf("p = %ld is %s.\n", p, flag ? "prime" : "not prime");
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

/* Fermat–@‚É‚æ‚é‘f””»’è */
long int isPrimeFermat(long int p) {
  long int i;
  for(i = 0; i < LOOP; i++) {
    long int a = rand() % (p -2) +2, b = 1;
    b = calcSqMul(a, p-1, p);
    if(b % p != 1) return 0;
  }
  return 1;
}

