#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include<string.h>
#include <direct.h> 

#define N 10000
#define M 13000
#include"multi.h"

main(int argc,char* argv[])
{
    
	int n,i,*bufsize,writecount,truefalse;
	unsigned int x[N],work[N],work2[N];
	unsigned int *reserve;
	FILE *fp1,*fp2;
    char path[40];

		  n=atoi(argv[1]);
	      strcpy(path,argv[2]);
		  reserve=(unsigned int *)malloc(sizeof(unsigned int)*N);
	      bufsize =(int *)malloc(sizeof(int)*2);
		  Initialize(work);
		  work[0]=1;
		  chdir(path);
		  fp1=fopen("CulculateData","rb");            /*ファイルが存在するかたしかめて、存在する場合すべて読み込む*/
          if(fp1!=NULL){
		      fread(bufsize,sizeof bufsize,1,fp1);       
			  if(*bufsize==-1||*bufsize==-2){      /*前回の終了した計算データを読み込んでしまった場合終了する*/
					free(reserve);
					free(bufsize);
					exit(1);
			  }
			  fread(reserve,sizeof reserve,N,fp1);
			  Copy(reserve,x);
			  fread(bufsize,sizeof bufsize,1,fp1);
			  i=*bufsize;
			  fread(bufsize,sizeof bufsize,1,fp1);
			  writecount=*bufsize;
			  fclose(fp1);
		  }
		  else{
			  Initialize(x);
			  x[0]=1;
			  LongLeftShift(x,n);
			  Sub(x,work);
              i=0; /*試行の回数*/
              writecount=1;
		  }
		  truefalse=0;
		  while(Compare(x,work)!=0){               /* x が 1 になるまで以下を繰り返す */ 
			      chdir(path);
			      fp1=fopen("CulculateData","rb");
                  if(fp1!=NULL){
						  fread(reserve,sizeof(unsigned int),N,fp1);    
						  fseek(fp1,sizeof(unsigned int)*N+sizeof(int)*3,SEEK_SET);       /*最初の部分は飛ばす*/
						  while(feof(fp1)==0){
								  fread(bufsize,sizeof(int),1,fp1);
								  n=*bufsize;
								  fread(reserve,sizeof(unsigned int),n,fp1);
								  if(Compare(x,reserve)==0){          /*ファイルに保存しておいた通過した値と比較する*/
									  truefalse =-2;  
									  goto endworking;
								  }
						  }
						  fclose(fp1);
						  fp1=fopen("CulculateData","r+b");
						  fseek(fp1,sizeof(int),SEEK_SET);
						  Copy(x,reserve);
						  fwrite(reserve,sizeof(unsigned int),N,fp1);
                          fwrite(&i,sizeof(int),1,fp1);
						  if(i==writecount){                              /*書き込むべき回数に達していた場合*/
								 fwrite(&writecount,sizeof(int),1,fp1);
							     fseek(fp1,0,SEEK_END);
								 Copy(x,reserve);     
								 *bufsize=Degree(reserve);
								 fwrite(bufsize,sizeof(int),1,fp1);
								 fwrite(reserve,sizeof(unsigned int),*bufsize,fp1);
								 writecount = writecount*10;
						  }
						  fclose(fp1);
				  }
				  else{
					  fp1=fopen("CulculateData","wb");
					  Copy(x,reserve);  
					  fwrite(&truefalse,sizeof(int),1,fp1);
					  fwrite(reserve,sizeof(unsigned int),N,fp1);
					  fwrite(&i,sizeof(int),1,fp1);
					  fwrite(&writecount,sizeof(int),1,fp1);
					  fclose(fp1);
				  }
              fp2=fopen("EndTeller","rb");           /*終了処理を行う指示が出ていないか調べる,ファイルが存在する場合終了処理を行う*/
		      if(fp2!=NULL){				
				  fclose(fp2);	
			      goto endworking;
			  }
			  Copy(x,work2);
			  if(Div(work2,2)==0){                /* 偶数の時 */
				   RightShift(x,1);            /* x を 2 で割り、x に代入する*/
			  } else {                    /* 奇数の時 */       
				   Mul(x,3);
				   Add(x,work);           /* x を 3 倍して 1 を足したものを、x に代入する */
			  }
			  i++;
		  }
              truefalse = -1;
			  fp1=fopen("CulculateData","wb");
			  fwrite(&truefalse,sizeof(int),1,fp1);
		      fclose(fp1);
			  free(reserve);
              free(bufsize);
		      exit(1);
                                                 /*ＶＢのアプリが終了処理を行う事を示すファイルに書き込みを行った場合終了処理*/
endworking:
	    chdir(path);
	    fp1=fopen("CulculateData","r+b");
        fseek(fp1,sizeof(int),SEEK_SET);
		fwrite(&truefalse,sizeof(int),1,fp1);
		fclose(fp1);
		fclose(fp2);
		remove("EndTeller");            
		free(reserve);
        free(bufsize);
		exit(1);
}




