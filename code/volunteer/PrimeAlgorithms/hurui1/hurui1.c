#include<stdio.h>
#include<stdlib.h>
#include<limits.h>
#include<math.h>
#include<time.h>


main()
{
	unsigned char *flag;
	int n,k,limit;
    float start,end;
	FILE *fp1;

	printf("�T������f���͈̔͂����߂ĉ������i������)\n");
	scanf("%d",&limit);
	start=((float)clock())/CLOCKS_PER_SEC;
	flag=(unsigned char *)malloc(limit*sizeof(unsigned char));
	if (flag == NULL){
		printf("������������܂���\n");  
	    exit(1);
	}
    fp1=fopen("Prime","wb");
	/*�z��̏�����*/
	for(n=2;n<=limit;n++){
		flag[n]=0;
	}
	for(n=2;n<=limit;n++){
		if(flag[n]==0){
			  for(k=2*n;k<=limit;k+=n){
				   flag[k]=1; 
			  }
		}	
    }
	for(n=2;n<=limit;n++){
		if(flag[n]==0){
			fwrite(&n,1,sizeof(int),fp1);      /*�t�@�C���ɏ�������*/
		}	
    }
    end=(((float)clock())/CLOCKS_PER_SEC)-start;
	printf("�o�ߎ��ԁ�%.5f\n",end);

}	
