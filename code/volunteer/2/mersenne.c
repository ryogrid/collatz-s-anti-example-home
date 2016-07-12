#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include<string.h>
#include <direct.h> 

main(int argc,char* argv[]){
    int *work;
	char path[40];
	char *l, *m;
    char *lp, *mp1, *mp2;
    FILE *fp1,*fp2;
    int  i=2, j, k;
    int  ca, x,p;
    int  ret = 1;
    
	p=atoi(argv[1]);
	strcpy(path,argv[2]);
	if (p <= 0) return ;
    if (p <= 2) return ;
	
    work=(int *)malloc(sizeof(int));
    chdir(path);
	fp1=fopen("CulculateData","rb");              /*�t�@�C�������݂��邩�������߂āA���݂���ꍇ���ׂēǂݍ���*/
    if(fp1!=NULL){
		fread(work,sizeof p,1,fp1);
		p=*work;
	}
    if ((l = (char *)malloc(p)) == NULL){
		free(work);
		exit(1);
	}
    if ((m = (char *)malloc(p)) == NULL) {
        free(l);
		free(work);
        exit(1);
    }
	if(fp1!=NULL){                                /*�t�@�C�������݂���ꍇ�A�O��̕ۑ����Ă������f�[�^�������J�n����*/
		fread(work,sizeof p,1,fp1);
		i=*work;
		if(i==-1||i==-2){      /*�O��̏I�������v�Z�f�[�^��ǂݍ���ł��܂����ꍇ�I������*/
			free(work);
			free(m);
			free(l);
			exit(1);
		}
		fread(l,1,p,fp1);
		fread(m,1,p,fp1);
        fclose(fp1);
	}
	else{
	    for (lp = l + p; lp != l; ) *--lp = 0;
        *(l+2) = 1;                                     /* L(1) = 4 */   
	}
	for (; i < p; i++) {
		chdir(path);
		fp1=fopen("CulculateData","wb");
		fwrite(&p,1,sizeof p,fp1);
		fwrite(&i,1,sizeof p,fp1);
		fclose(fp1);
		fp2=fopen("EndTeller","rb");           /*�I���������s���w�����o�Ă��Ȃ������ׂ�,�t�@�C�������݂���ꍇ�I���������s��*/
		if(fp2!=NULL){				
				  fclose(fp2);	
			      goto endworking;
		}
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
	
	fp1=fopen("CulculateData","wb");
	fwrite(&p,1,sizeof p,fp1);
	i=-1*(ret+1);         /*2�Ȃ�^�A�P�Ȃ�U�Ƃ���*/
	fwrite(&i,1,sizeof p,fp1);                 /*�v�Z�I��������2���P����������*/
	free(work);
	free(m);
    free(l);
	exit(1);

                                                 /*�u�a�̃A�v�����I���������s�����������t�@�C���ɏ������݂��s�����ꍇ�I������*/
endworking:
	    chdir(path);
	    fp1=fopen("CulculateData","wb");
		fwrite(&p,1,sizeof p,fp1);
		fwrite(&i,1,sizeof p,fp1);
		fwrite(l,1,p,fp1);
		fwrite(m,1,p,fp1);
		fclose(fp1);
		fclose(fp2);
		remove("EndTeller");            
		free(l);
		free(m);
		free(work);
		exit(1);
}


