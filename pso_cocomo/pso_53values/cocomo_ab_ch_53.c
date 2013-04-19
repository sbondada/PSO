
/* This program does the cost estimation for software projects by making use of the cocomo model :
    effort=a*(size)^b*EAF. the program uses particle swarm optimization technique for estimating the values
    of a and b. 
 */

#include<stdio.h>
#include<stdlib.h>
#include<math.h>

/* variables declaration*/
double gbesta,gbestb,nop,noi,w,c1,c2,val;
double va[25],vb[25],sa[25],sb[25],pbesta[25],pbestb[25],eval[55],eaf[55],size[55],f[25],gfunc=99999999;


/* list of functions used*/
double rnd();
double fn(double ,double);
void updatea(int);
void updateb(int);
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

/* fn(a,b) is the fitness function in this case we have considered mean of  percentage error as a function*/
double fn(double aa,double bb)
{ 
   double y=0,error=0,toterr=0,exp_val,sz,t;
   int i;
 
   for(i=0;i<val;i++)
   { 
      sz=size[i];
      exp_val=eval[i];
      t=pow(sz,bb);
      y=aa*(t)*eaf[i];
      
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
            if(vk1 >10)
         goto x;

        sk1=sa[k]+vk1;                                                /*position updates*/
   
        if(sk1>20)    /*restricting a value to below 20*/
         {goto x;}
   
   
   va[k]=vk1;
   sa[k]=sk1;

 }

void updateb(int k)
 {    
   double r1,r2,sk2=0,vk2=0;
   y: 
        r1=rnd();r2=rnd();

        vk2=w*vb[k]+(c1*r1*(pbestb[k]-sb[k]))+(c2*r2*(gbestb-sb[k])); /* velocity updates*/
        if(vk2-vb[k] >5 || vk2-vb[k]<-5)
         goto y;

         sk2=sb[k]+vk2;                                                /*position updates*/
   
        if(sk2>20) /*restricting bvalue to below 20*/
         {goto y;}
   
   
   vb[k]=vk2;
   sb[k]=sk2;
   
 }

/* cal_fn(k) function calculates function value for kth particle by accessing fn(a,b) function*/
void cal_fn( int k)
 {  
   double newy;
   newy=fn(sa[k],sb[k]);
   f[k]=newy;
 }
   
/*find_pbest(k) function finds the personal best values of the kth particle*/
void find_pbest(int k)
 {  
   double newy,oldy;
   newy=f[k];
   oldy=fn(pbesta[k],pbestb[k]);
   
   if(newy<oldy)
    { 
      pbesta[k]=sa[k];
      pbestb[k]=sb[k];
    }

 }

/* find_gbest() function finds the global best particle position attained so far*/
void find_gbest()
 {  
     double min=0;
     int minpos=0,i=0;
      min=f[0];
      minpos=0;
      for(i=1;i<nop;i++)
         {     if(f[i]<min)
             {  min=f[i];  
                minpos=i;
             }
          } 
     if(gfunc>min)
   {
       gfunc=min;
       gbesta=sa[minpos];
       gbestb=sb[minpos];
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
/* function to update parameters c1 and c2*/
void update_param(int k)
{ c1=2.5-2*(k/noi);
  c2=0.5+2*(k/noi);
}
/* init() function initializes values too all the parameters and also initial v,s values */
void init()
{ 
   w=0.50;
   c1=2.5;
   c2=0.5;
   noi=100;    /* no of iterations*/
   nop=20;     /*no of particles*/
   val=20;
   int i;
   for(i=0;i<val;i++)
   {  
      scanf("%lf %lf %lf",&size[i],&eval[i],&eaf[i]);
}
    for(i=0;i<nop;i++)
   {  
        sa[i]=rnd_range(0,10);
        va[i]=rnd_range(0,5); 
        sb[i]=rnd_range(0,5);
        vb[i]=rnd_range(0,2);
        
         pbesta[i]=sa[i];
         pbestb[i]=sb[i];
      
      f[i]=fn(pbesta[i],pbestb[i]);

   }

   find_gbest();
}

 int main()
{  
     int i,k;
     init();    /* initialize values*/
     printf("va\t\t vb\t\t sa\t\t sb\t\tf\t\t pa\t\t pb\t\n");
    
      for(i=0;i<noi;i++)
      { update_param(i);
          
           for(k=0;k<nop;k++)
           {
          
             updatea(k);           /*update to find new a value*/
             updateb(k);          /*update particle to new b value*/
             cal_fn(k);           /* calculate function fn value*/
             find_pbest(k);       /* find the personal best for this particle*/
          
           }
         
           find_gbest();         /* find global best position traversed so far*/
      }
   

    printf("a=%lf \n b=%lf\n",gbesta,gbestb);
    printf ("estimated effort \t original effort\n");
    printf("---------------------------------------\n");
    for(i=0;i<val;i++)
    {
       gfunc=gbesta*pow(size[i],gbestb)*eaf[i];
       printf("%lf \t\t %lf\n",gfunc,eval[i]);
    }
 printf("\n #### MARE =%lf###\n",fn(gbesta,gbestb)*100);

return 0;
}
     

     

      












   














