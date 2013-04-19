



/* This program does the cost estimation for software projects by making use of the cocomo model :
    effort=(a*(size)^b)*(E.A.F)+c. the program uses particle swarm optimization technique for estimating the values
    of a ,b and c.
 */
#include<stdio.h>
#include<stdlib.h>
#include<math.h>




/* variables declaration*/
double gbesta,gbestb,gbestc,nop,noi,w,c1,c2,val;
double va[30],vb[30],sa[30],sb[30],pbesta[30],pbestb[30],pbestc[30],eaf[55],eval[55],size[55],f[55],me[20],sc[30],vc[30],gfunc=99999999;
int rank_mare[100],rank_count[55],temp_count[55],totranks[55];
double breakpc=0.25,temp_mare[55];

/* list of functions used*/
double rnd();
double fn(double ,double,double);
void updatea(int);

void updateb(int);
void updatec(int);
void cal_fn(int);
void find_pbest(int);
void find_gbest();
void init();

/* rnd() function is used to generate r1 and r2 values in the range (0,1)*/
double rnd()
 {
  double r;
  r=rand()%10000;
  r=(double)r/10000.0;
  return r;
  }

/* fn(a,b) is the fitness function in this case we have considered mean of percentage  error as a function*/
double fn(double aa,double bb,double cc)
{ 
   double y=0,error=0,toterr=0,exp_val,sz;
   int i;
 
   for(i=0;i<val;i++)
   { 
      sz=size[i];
      exp_val=eval[i];
      y=aa*(pow(sz,bb));
      y=y*eaf[i]+cc;
      error=(exp_val-y);
      error=(error/exp_val);
      if(error<0)
          {error=-1*error;}
        toterr+=error;
  }
  
   toterr=toterr/val;
   return toterr;
 }

/*updatea(k) updates the a value of the kth particle based on the velocity and position functions*/
void updatea(int k)
 {    
   double r1,r2,sk1=0,vk1=0;
      x:
        r1=rnd();r2=rnd();
   
        vk1=w*va[k]+(c1*r1*(pbesta[k]-sa[k]))+(c2*r2*(gbesta-sa[k])); /* velocity updates*/
        sk1=sa[k]+vk1;                                                /*position updates*/
   
        if(sk1>40)    /*restricting a value to below 10*/
         {goto x;}
   
   
   va[k]=vk1;
   sa[k]=sk1;

 }
/*updateb(k) updates the b value of the kth particle based on the velocity and position functions*/
void updateb(int k)
 {    
   double r1,r2,sk2=0,vk2=0;
   y: 
        r1=rnd();r2=rnd();

        vk2=w*vb[k]+(c1*r1*(pbestb[k]-sb[k]))+(c2*r2*(gbestb-sb[k])); /* velocity updates*/
        sk2=sb[k]+vk2;                                                /*position updates*/
   
        if(sk2>20 || sk2<-20) /*restricting bvalue to (-10,10)*/
         {goto y;}
   
   
   vb[k]=vk2;
   sb[k]=sk2;
   
 }
/*updatec(k) updates the c value of the kth particle based on the velocity and position functions*/
void updatec(int k)
 {    
   double r1,r2,sk2=0,vk2=0;
   z: 
        r1=rnd();r2=rnd();

        vk2=w*vc[k]+(c1*r1*(pbestc[k]-sc[k]))+(c2*r2*(gbestc-sc[k])); /* velocity updates*/
        sk2=sc[k]+vk2;                                                /*position updates*/
   
        if(sk2<-50 || sk2>50) /*restricting c value to (-10,10)*/
         {goto z;}
   
   
   vc[k]=vk2;
   sc[k]=sk2;
   
 }






/*new functions added */

int find_count(double a,double b,double c)
{   

  int countpercent=0;
  int i;
  double y=0,error=0,toterr=0,exp_val,sz;
  for(i=0;i<val;i++)
  {
    sz=size[i];
      exp_val=eval[i];
      y=a*(pow(sz,b));
      y=y*eaf[i]+c;
      error=(exp_val-y);
      error=(error/exp_val)*100;
      if(error<0)
      {error=-1*error;}
      if(error<=breakpc*100)
      {countpercent++;
      }
   }
return countpercent;
}

void find_rankings()
{
  
   int count_temp=0,temp_val[55],i,j,temp1,temp_v;
   double temp;
  for(i=0;i<nop;i++)
  { temp_val[i]=i;}

   for(i=0;i<nop-1;i++)
   { for(j=i;j<nop;j++)
     { if(temp_mare[i]>temp_mare[j]) 
       { temp=temp_mare[i];
         temp_mare[i]=temp_mare[j];
         temp_mare[j]=temp;
         temp_v=temp_val[i];
        temp_val[i]=temp_val[j];
         temp_val[j]=temp_v;
       }
      }
   }

    
      temp=temp_mare[0];
     for(i=0;i<nop;i++) 
      {  temp1=temp_val[i];
         rank_mare[temp1]=count_temp;
	 if(temp_mare[i+1]!=temp_mare[i])
        { count_temp++;
        }
      }

for(i=0;i<nop;i++)
  { temp_val[i]=i;}

   for(i=0;i<nop-1;i++)
   { for(j=i;j<nop;j++)
     { if(temp_count[i]>temp_count[j]) 
       { temp=temp_count[i];
         temp_count[i]=temp_count[j];
         temp_count[j]=temp;
       temp_v=temp_val[i];
        temp_val[i]=temp_val[j];
         temp_val[j]=temp_v;
       }
      }
   }
 
    temp=temp_count[0];
     count_temp=0;
     for(i=0;i<nop;i++) 
      { temp1=temp_val[i];
           rank_count[temp1]=count_temp;
        if(temp_count[i+1]!=temp_count[i])
        { count_temp++;
        }
       }

 for(i=0;i<nop;i++)
  {  totranks[i]=rank_mare[i]+rank_count[i];}

}













/* cal_fn(k) function calculates function value for kth particle by accessing fn(a,b) function*/
void cal_fn( int k)
 {  
   double newy;
   newy=fn(sa[k],sb[k],sc[k]);
   f[k]=newy;
   temp_mare[k]=f[k];
   //new added
   temp_count[k]=find_count(sa[k],sb[k],sc[k]);
  //  printf("\n%d",temp_count[k]);
 }
   
/*find_pbest(k) function finds the personal best values of the kth particle*/
void find_pbest(int k)
 {  
   double newy,oldy,newc,oldc;
   newy=f[k];
   newc=temp_count[k];
   oldy=fn(pbesta[k],pbestb[k],pbestc[k]);
   oldc=find_count(pbesta[k],pbestb[k],pbestc[k]);
   
   
   if((newy<oldy && newy>breakpc) || (newy<breakpc && oldy<breakpc && newc>oldc)||(newy<oldy && newy<breakpc && oldy>=breakpc))
    { 
      pbesta[k]=sa[k];
      pbestb[k]=sb[k];
      pbestc[k]=sc[k];    
}
 

 }

/* find_gbest() function finds the global best particle position attained so far*/
void find_gbest()
 {  
     double min=0;
     int minpos=0,i=0;
      min=totranks[0];
      minpos=0;
      for(i=1;i<nop;i++)
         {     if(totranks[i]<min)
             {  min=totranks[i];  
                minpos=i;
             }
          } 
    // if(gfunc>min)
   {
       gfunc=min;
       gbesta=sa[minpos];
       gbestb=sb[minpos];
       gbestc=sc[minpos];
    }

 }

/*rnd_range(a,b) function generates random values in the range (a,b)*/
double rnd_range(int a,int b)
 {
    double x,y,ff;
    x=rand()%(b*10000);
    y=x/10000.0;
    x=y;
    ff=rand()%2;
    if(ff==1 && (-1*x)>b)
     {x=-1*x;}                 /* used for randomizing between positive and negative values*/

    return x;  
 }

/* init() function initializes values too all the parameters and also initial v,s values */
void init()
{ 
   w=0.50;
   c1=2.0;
   c2=2.0;
   noi=500;    /* no of iterations*/
   nop=20;    /*no of particles*/
   val=21;
   int i;
   for(i=0;i<val;i++)
   {  
      scanf("%lf %lf %lf\n",&size[i],&eval[i],&eaf[i]);

    }
     for(i=0;i<nop;i++){
        sa[i]=rnd_range(0,5);
        va[i]=rnd_range(0,2); 
        sb[i]=rnd_range(0,5);
        vb[i]=rnd_range(0,2);
        sc[i]=rnd_range(0,5);
        vc[i]=rnd_range(0,2);
         pbesta[i]=sa[i];
         pbestb[i]=sb[i];
         pbestc[i]=sc[i];
      f[i]=fn(pbesta[i],pbestb[i],pbestc[i]);

   }

   find_gbest();
}

 int main()
{  
     int i,k;
     init();    /* initialize values*/

    
      for(i=0;i<noi;i++)
      { 
           for(k=0;k<nop;k++)
           {
         
            
             updatea(k);           /*update to find new a value*/
             updateb(k);         /*update particle to new b value*/
             updatec(k);
             cal_fn(k);           /* calculate function fn value*/
             find_pbest(k);       /* find the personal best for this particle*/
          
           }
           find_rankings();
           find_gbest();        /* find global best position traversed so far*/
      }
   

    printf("a=%lf \n b=%lf\nc=%lf\n",gbesta,gbestb,gbestc);
    double mare=0,tmre=0;
    printf("Estimated effort \t actual effort\n");
    printf("---------------------------------------------\n");
    for(i=0;i<val;i++)     /* calculation of efoort values from final values of a,b,c*/
    {  
        gfunc=gbesta*pow(size[i],gbestb)*eaf[i]+gbestc;
     
       printf("%lf\t\t %lf\n",gfunc,eval[i]);
    }
printf("\n number of values below 25=%d",find_count(gbesta,gbestb,gbestc));
  //end:printf("\n %lf",gfunc); 
 printf("\n #### MARE =%lf###\n",fn(gbesta,gbestb,gbestc)*100);

return 0;
}
     

     

      












   














