/*素数発見プログラム*/
#include<stdio.h>
#include<math.h>
#include<time.h>

main(){

	int torf1,torf2,a,b,i,w1,w2;
	float start,end;
	FILE *fp1;
        
	    fp1=fopen("Prime","wb");
		printf("探索する素数の範囲を決めて下さい（整数で)\n");
		scanf("%d",&i);
		start=((float)clock())/CLOCKS_PER_SEC;
		fp1=fopen("Prime","wb");
		for(a=1;6*a+1<=i;a++) {
		   torf1=1;
		   torf2=1;
		   w1=6*a+1;
		   if(w1%3==0){
                torf1=0;
				goto jump1;
		   }
		   if(w1%5==0){
                torf1=0;
				goto jump1;
				}
		   for(b=1;6*b+1<=sqrt(w1)-2;b++) { 
				if(w1%(6*b+1)==0){
						   torf1=0;  
						   break;   
				}
				if(w1%(6*b+5)==0){
					       torf1=0;
						   break;
				}
		   }
jump1:
           w2=6*a+5;
		   if(w2%3==0){
			      torf2=0;
				  goto jump2;
		   }
	       if(w2%5==0){
			      torf2=0;
				  goto jump2;
				}
		   for(b=1;6*b+1<=sqrt(w2)-2;b++) { 
			    if(w2%(6*b+1)==0){
						   torf2=0;  
						   break;   
				}
				if(w2%(6*b+5)==0){
					       torf2=0;
						   break;
				}
		   }
jump2:
		   if(torf1==1){
			    fwrite(&w1,1,sizeof(int),fp1);      /*ファイルに書き込む*/
		   }
           if(torf2==1){
			    fwrite(&w2,1,sizeof(int),fp1);      /*ファイルに書き込む*/
		   }
		}
		end=(((float)clock())/CLOCKS_PER_SEC)-start;
		printf("経過時間＝%.5f\n",end);
}                    

