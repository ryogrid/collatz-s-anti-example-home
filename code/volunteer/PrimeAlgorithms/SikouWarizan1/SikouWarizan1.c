/*�f�������v���O����*/
#include<stdio.h>
#include<math.h>
#include<time.h>

main(){

	int torf,a,b,i;
	float start,end;
	FILE *fp1;
        
	    fp1=fopen("Prime","wb");
		printf("�T������f���͈̔͂����߂ĉ������i������)\n");
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
			    fwrite(&a,1,sizeof(int),fp1);      /*�t�@�C���ɏ�������*/
			}
		}
		end=(((float)clock())/CLOCKS_PER_SEC)-start;
		printf("�o�ߎ��ԁ�%.5f\n",end);
}                    

        