/* This program does the cost estimation for software projects by making use of the cocomo model :
    effort=a*(size)^b+c*(M.E)+d. the program uses particle swarm optimization technique for estimating the values
    of a ,b ,cand d.
 */


#include<stdio.h>
#include<stdlib.h>
#include<math.h>




/* variables declaration*/
double gbesta,gbestb,gbestc,nop,noi,w,c1,c2,gbestd;
double va[25],vb[25],sa[25],sb[25],pbesta[25],pbestb[25],pbestc[25],eval[25],size[25],f[25],me[20],sc[20],vc[20],vd[20],sd[20],pbestd[20],gfunc=99999999;


/* list of functions used*/
double rnd();
double fn(double ,double,double,double);
void updatea(int);
void updatec(int);
void updateb(int);
void updated(int);
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

/* fn(a,b) is the fitness function in this case we have considered mean of percentage error as a function*/
double fn(double aa,double bb,double cc,double dd)
{ 
   double y=0,error=0,toterr=0,exp_val,sz;
   int i;
 
   for(i=0;i<nop;i++)
   { 
      sz=size[i];
      exp_val=eval[i];
      y=aa*(pow(sz,bb));
      y=y+cc*me[i]+dd;
      error=(exp_val-y);
      error=(error/exp_val)*100;
      if(error<0)
          {error=-1*error;}
        toterr+=error;
  }
  
   toterr=toterr/nop;
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
   
       if(sk1>10 || sk1 <-10)    /*restricting a value to -10,10*/
         {goto x;}
       if(vk1<-5)
        {goto x;
        }
 
   va[k]=vk1;
   sa[k]=sk1;

 }
/*updateb(k) updates the  b value of the kth particle based on the velocity and position functions*/
void updateb(int k)
 {    
   double r1,r2,sk2=0,vk2=0;
   y: 
        r1=rnd();r2=rnd();

        vk2=w*vb[k]+(c1*r1*(pbestb[k]-sb[k]))+(c2*r2*(gbestb-sb[k])); /* velocity updates*/
        sk2=sb[k]+vk2;                                                /*position updates*/
   
      if(sk2>5 || sk2<-5) /*restricting b value to (-5,5)*/
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
   
        if(sk2<-1 || sk2>1) /*restricting c value to  (-1,1)*/
         {goto z;}
   
   
   vc[k]=vk2;
   sc[k]=sk2;
   
 }
/*updated(k) updates the d value of the kth particle based on the velocity and position functions*/
void updated(int k)
 {    
   double r1,r2,sk2=0,vk2=0;
   a: 
        r1=rnd();r2=rnd();

        vk2=w*vd[k]+(c1*r1*(pbestd[k]-sd[k]))+(c2*r2*(gbestd-sd[k])); /* velocity updates*/
        sk2=sd[k]+vk2;                                                /*position updates*/
   
        if(sk2>20) /*restricting d value to below 20*/
         {goto a;}
         if(vk2>5){goto a;}
       
   vd[k]=vk2;
   sd[k]=sk2;
   
 }



/* cal_fn(k) function calculates function value for kth particle by accessing fn(a,b) function*/
void cal_fn( int k)
 {  
   double newy;
   newy=fn(sa[k],sb[k],sc[k],sd[k]);
   f[k]=newy;
 }
   
/*find_pbest(k) function finds the personal best values of the kth particle*/
void find_pbest(int k)
 {  
   double newy,oldy;
   newy=f[k];
   oldy=fn(pbesta[k],pbestb[k],pbestc[k],pbestd[k]);
   
   if(newy<oldy)
    { pbestd[k]=sd[k];
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
       gbestc=sc[minpos];
       gbestd=sd[minpos]; 
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
      w=0.50;    /* inertia factor*/
   c1=2.0;   /* c1 denotes local search weight*/
   c2=2.0;   /* c2 denotes global search weight*/
   noi=900;    /* no of iterations*/
   nop=13;    /*no of particles*/
   
   int i;
   for(i=0;i<nop;i++)
   {  
      scanf("%lf %lf %lf\n",&size[i],&eval[i],&me[i]);
     // printf("%lf %lf %lf\n",size[i],eval[i],me[i]);
    }
     for(i=0;i<nop;i++){
        sa[i]=rnd_range(0,10);
        va[i]=rnd_range(0,5); 
        sb[i]=rnd_range(0,5);
        vb[i]=rnd_range(0,2);
        sc[i]=rnd_range(0,1);
        vc[i]=rnd_range(0,1);
        sd[i]=rnd_range(0,2);
        vd[i]=rnd_range(0,2);
         pbesta[i]=sa[i];
         pbestb[i]=sb[i];
         pbestc[i]=sc[i];
         pbestd[i]=sd[i];    
  
         f[i]=fn(pbesta[i],pbestb[i],pbestc[i],pbestd[i]);

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
             updated(k);
             cal_fn(k);           /* calculate function fn value*/
             find_pbest(k);       /* find the personal best for this particle*/
          
           }
         
           find_gbest();         /* find global best position traversed so far*/
      }
   

    printf("a=%lf \n b=%lf\nc=%lf\nd=%lf\n",gbesta,gbestb,gbestc,gbestd);
    printf("estimated value \t actual value\n");
    printf("---------------------------------------\n");
   double mare=0,tmre=0;          /* final effort values calculated using a,b,c,d values obtained*/
    for(i=0;i<nop;i++)
    {  
       gfunc=gbesta*pow(size[i],gbestb)+(gbestc*me[i])+gbestd;
       mare=gfunc-eval[i];
       mare=mare<0?-mare:mare;
       tmre+=mare;
       printf("%lf\t\t%lf\n",gfunc,eval[i]);
    }
    tmre=tmre/nop;
    printf("total mre=%lf",tmre);

return 0;
}
     

     

      












   














