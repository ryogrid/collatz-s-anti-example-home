#include<stdio.h>
#include<stdlib.h>
#include<limits.h>
#include<time.h>
#include<math.h>

main()
{
	unsigned char *flag;
	int n,k,m,p,limit;
	float s,start,end;
	FILE *fp1;
    
	printf("探索する素数の範囲を決めて下さい（整数で)\n");
	scanf("%d",&limit);
	start=((float)clock())/CLOCKS_PER_SEC;
	flag=(unsigned char *)malloc(limit*sizeof(unsigned char));
	if (flag == NULL){
		printf("メモリが足りません\n");  
	    exit(1);
	}
	/*配列の初期化*/
	for(n=3;n<=limit;n+=2){
		flag[n]=0;
	}
	flag[2]=0;
	for (n=2;n<=210;n++){
		if(flag[n]==0){
			for(k=n*n;k<=limit;k+=n){
			    flag[k]=1;
			}    
		}
	}
	for (n=1;210*n+1<=sqrt(limit);n++){
		m=210*n+1;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
		m=210*n+11;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+13; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}
			}
		}
        m=210*n+17;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
	    m=210*n+19;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+23; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+29;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
        }
	    m=210*n+31;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
		m=210*n+37;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+41; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}
			}
		}
        m=210*n+43;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
	    m=210*n+47;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+53; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+59;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
        }
	    m=210*n+61;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
		m=210*n+67; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+71;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
        }
		m=210*n+73;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+79; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}
			}
		}
        m=210*n+83;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
	    m=210*n+89;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
		m=210*n+97;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
		m=210*n+101;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+103; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}
			}
		}
        m=210*n+107;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
	    m=210*n+109;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+113; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+121;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
        }
	    m=210*n+127;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
		m=210*n+131;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+137; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}
			}
		}
        m=210*n+139;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
	    m=210*n+143;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+149; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+151;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
        }
	    m=210*n+157;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
		m=210*n+163; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+167;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
        }
		m=210*n+169;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+173; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}
			}
		}
        m=210*n+179;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
	    m=210*n+181;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+187;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
		m=210*n+191; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+193;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
        }
		m=210*n+197;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
        m=210*n+199; 
		if(flag[m]==0){			
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}
			}
		}
        m=210*n+209;
		if(flag[m]==0){
			s=m;
			s=s*s;
			if(s<=limit){
			    p=2*m;
				for(k=s;k<=limit;k+=p){
				     flag[k]=1;
				}    
			}
		}
	}
	printf("limit = %d is %s.\n", limit, flag[limit] ? "not prime":"prime");
	end=(((float)clock())/CLOCKS_PER_SEC)-start;
	printf("経過時間＝%.5f\n",end);

}	

