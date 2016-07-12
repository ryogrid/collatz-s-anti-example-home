#include<stdio.h>
#include<math.h>
#define ENDED '#'

main(int argc,char* argv[])
        {
FILE *fp1;
float a,b;
long within,given;
char frag;

within=atol(argv[1]);
fp1=fopen("given.txt","r");
fscanf(fp1,"%d",&given);
fclose(fp1);
fp1=fopen("out.txt","w");
for(a=given;a<=given+within;a=a+2) 
   {
  frag=1;
   for(b=3;b<=sqrt(a)-1;b=b+2) 
     { 
        if(fmod(a,b)==0)    {
                   frag=0;  }    
     }
     if(frag==1) {
        printf("%.0f\n",a);  
        fprintf(fp1,"%.0f,",a);
              }
   }  
       fclose(fp1);
       fp1=fopen("given.txt","w");
       fprintf(fp1,"%c",ENDED);
       fclose(fp1);
}     
