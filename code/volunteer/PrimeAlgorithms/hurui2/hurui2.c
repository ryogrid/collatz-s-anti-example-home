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
	double s;
	FILE *fp1;

	printf("探索する素数の範囲を決めて下さい（整数で)\n");
	scanf("%d",&limit);
	start=((float)clock())/CLOCKS_PER_SEC;
	flag=(unsigned char *)malloc(limit*sizeof(unsigned char));
	if (flag == NULL){
		printf("メモリが足りません\n");  
	    exit(1);
	}
    fp1=fopen("Prime","wb");
	/*配列の初期化*/
	for(n=2;n<=limit;n++){
		flag[n]=0;
	}
	for(n=2;n<=limit;n++){
		if(flag[n]==0){
			s=n;
			s=s*s;
			if(s<limit){
			      for(k=n*n;k<=limit;k+=2*n){
				      flag[k]=1; 
				  }
			}
		}	
    }
	for(n=2;n<=limit;n++){
		if(flag[n]==0){
			fwrite(&n,1,sizeof(int),fp1);      /*ファイルに書き込む*/
		}	
    }
    end=(((float)clock())/CLOCKS_PER_SEC)-start;
	printf("経過時間＝%.5f\n",end);

}	
