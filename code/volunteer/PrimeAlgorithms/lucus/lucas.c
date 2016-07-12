#include<stdio.h>
#include<stdlib.h>
#include<time.h>

int lucas(int p);

main() {
  long int p = 0, flag = 0; /* flag=1 ‚È‚ç‘f” */
  float start,end;  
       
       printf("‰½æ‚Ìƒƒ‹ƒZƒ“ƒk‘f”Mn‚ğ‹‚ß‚Ü‚·‚©\n");
       scanf("%d",&p);
       start=((float)clock())/CLOCKS_PER_SEC;
       flag=lucas(p);
	   printf("2^%d-1 is %s.\n",p,flag ? "prime" : "not prime");
       end=(((float)clock())/CLOCKS_PER_SEC)-start;
       printf("Œo‰ßŠÔ%.5f\n",end);
}

int lucas(int p)
{
    char *l, *m;
    char *lp, *mp1, *mp2;
    int  i, j, k;
    int  ca, x;
    int  ret = 1;

    if (p <= 0) return 0;
    if (p <= 2) return 1;

    if ((l = (char *)malloc(p)) == NULL) return -1;
    if ((m = (char *)malloc(p)) == NULL) {
        free(l);
        return -1;
    }

    for (lp = l + p; lp != l; ) *--lp = 0;
    *(l+2) = 1;                                     /* L(1) = 4 */

    for (i = 2; i < p; i++) {
        for (lp = l + p, mp1 = m + p; lp != l; ) {
            *--mp1 = *--lp;
            *lp = 1;                        /* 2^p -1 mod 2^p -1 = 0 */
        }

        *(l+1) = 0;                                 /* L(i) = L(i) - 2 */
        for (mp1 = m, j = 0; j < p; j++) {
            if (*mp1++) {                           /* this bit is 1 */
                ca = 0;
                k = j;
                for (mp2 = m; mp2 != m + p; ) {     /* L(i) * L(i) */
                    x = *mp2++ + *(l+k) + ca;
                    *(l+k) = x & 1;
                    ca = x >> 1;
                    if (++k == p) k = 0;
                }
                if (ca) {
                    while (*(l+k)) {
                        *(l+k) = 0;
                        if (++k == p) k = 0;
                    }
                    *(l+k) = 1;
                }
            }
        }
    }

    x = *l;
    for (lp = l + p; lp != l; )               /* L(p-1) == all 0 or 1 ? */
        if (*--lp != x) {
            ret = 0;
            break;
        }

    free(m);
    free(l);
    return ret;
}
