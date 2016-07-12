/*素数発見プログラム*/
#include<stdio.h>
#include<math.h>
#include<time.h>

main(){

	int torf,a,b,i;
	float start,end;
	FILE *fp1;
        
	    fp1=fopen("Prime","wb");
		printf("探索する素数の範囲を決めて下さい（整数で)\n");
		scanf("%d",&i);
		start=((float)clock())/CLOCKS_PER_SEC;
		fp1=fopen("Prime","wb");
		for(a=5;a<=i;a=a+2) {
		  torf=1;
		   for(b=3;b<=sqrt(a)-2;b=b+2) { 
				if(a%b==0){
						   torf=0;  
						   break;   
				}    
		   }
		   if(torf==1){
			    fwrite(&a,1,sizeof(int),fp1);      /*ファイルに書き込む*/
			}
		}
		end=(((float)clock())/CLOCKS_PER_SEC)-start;
		printf("経過時間＝%.5f\n",end);
}                    

        