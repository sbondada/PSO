
/* This program does the cost estimation for software projects by making use of the cocomo model.
   Here we first cluster the data set using the kmeans clusteriung algorithm.
   Then the cocomo model parameters are tuned using the Particle Swarm optimization technique.
   The software estimation model used is:
        Effort=(a*(size)^b)*(E.A.F)+c.
    This program uses particle swarm optimization technique for estimating the values of a ,b and c.
 */

#include<stdio.h>
#include<stdlib.h>
#include<math.h>




/* variables declaration*/
double gbesta,gbestb,gbestc,nop,noi,w,c1,c2,gfunc=99999999,val=45;
double va[30],vb[30],sa[30],sb[30],sc[30],vc[30],pbesta[30],pbestb[30],pbestc[30],eaf[55];
double eval[55],size[55],f[55],me[20];
int allc[5][100],hasht[100][5],count[5];
int kmeans=3,cent=0;

/* list of functions used*/
void init();
double rnd();
double rnd_range(int,int);
void cal_fn(int);
double fn(double ,double,double);
void updatea(int);
void updateb(int);
void updatec(int);
void find_pbest(int);
void find_gbest();
void kmean(int);


/* rnd() function is used to generate r1 and r2 values in the range (0,1)*/
double rnd()
 {
   double r;
   r=rand()%10000;
   r=(double)r/10000.0;
   return r;
 }

/* fn(a,b) is the fitness function in this case we have considered mean of percentage  error as a function.
   Here the formula : 
   Effort=(a*(size)^b)*(E.A.F)+c. 
   is used.
   Fitness function is the mean of percentage error calculated between the measured effort and expected effort.
 */
double fn(double aa,double bb,double cc)
{ 
    double y=0,error=0,toterr=0,exp_val,sz;
    int i;
 
   for(i=0;i<count[cent];i++)
   { 
      sz=size[hasht[i][cent]];
      exp_val=eval[hasht[i][cent]];
      y=aa*(pow(sz,bb));
      y=y*eaf[hasht[i][cent]]+cc;
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
   
         if(sk1>20)    /*restricting a value to below 20*/
          {goto x;}
   
   
    va[k]=vk1;
    sa[k]=sk1;

 }

/*updateb(k) updates the b value of the kth particle based on the velocity and position functions*/
void updateb(int k)
 { 
   double r1,r2,sk2=0,vk2=0;
   y:  
        r1=rnd();
        r2=rnd();

        vk2=w*vb[k]+(c1*r1*(pbestb[k]-sb[k]))+(c2*r2*(gbestb-sb[k])); /* velocity updates*/
        sk2=sb[k]+vk2;                                                /*position updates*/
        
        if(sk2>30 || sk2<-20) /*restricting bvalue to (-20,30)*/
         {goto y;}
   
   
     vb[k]=vk2;
     sb[k]=sk2;
   
 }

/*updatec(k) updates the c value of the kth particle based on the velocity and position functions*/
void updatec(int k)
 {   
     double r1,r2,sk2=0,vk2=0;
   
      z:         
         r1=rnd();
         r2=rnd();
         vk2=w*vc[k]+(c1*r1*(pbestc[k]-sc[k]))+(c2*r2*(gbestc-sc[k])); /* velocity updates*/
         
         if(vk2-vc[k]>20 || vk2-vc[k]<-20)       /*  restricting c values to (-20,20) */
           {vc[k]=0;goto z;}                   
         
         sk2=sc[k]+vk2;                                                /*position updates*/

     vc[k]=vk2;
     sc[k]=sk2;
   
 }

/* cal_fn(k) function calculates function value for kth particle by accessing fn(a,b) function*/
void cal_fn( int k)
 {  
   double newy;
   newy=fn(sa[k],sb[k],sc[k]);
   f[k]=newy;
 }
   
/*find_pbest(k) function finds the personal best values of the kth particle*/
void find_pbest(int k)
 {  
   double newy,oldy;
   newy=f[k];
   oldy=fn(pbesta[k],pbestb[k],pbestc[k]);
   
   if(newy<oldy)
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

/* The function kmean(k) is used to cluster the input data set into k clusters by using the kmeans clustering algorithm*/

void kmean(int k)
{  
    double dist[5],centroid[5][3],sumsz[5],countk[5],sumeaf[5],x,y,min;
    int i,j,hd,p,q,pos=0,iter=1000,r;
    
    /* Centroids are values around which clusters are formed. cluster[0] represents the size and cluster[1] represents  
      the  eaf values
   */
    for (i=0;i<k;i++)
     {  
        centroid[i][0]=size[i*4+1];
        centroid[i][1]=eaf[i*4+1];
        sumsz[i]=sumeaf[i]=countk[i]=0;
     }

   
      for(hd=0;hd<iter;hd++)
     {     
         for(r=0;r<k;r++)
         { sumsz[r]=sumeaf[r]=countk[r]=0;}
    
         for(i=0;i<val;i++)
         { 

              for(j=0;j<k;j++)
               {  x=pow((size[i]-centroid[j][0]),2);
                  y=pow((eaf[i]-centroid[j][1]),2);
                  y=y+x;
                  
                  dist[j]=y;
               }
               
               
                 min=dist[0];
                 pos=0;
         
              for(j=0;j<k;j++)
               {  if(dist[j]<min)
                   { min=dist[j];
                     pos=j;
                   }
               }
              for(j=0;j<k;j++)
               { if(pos==j)
                  allc[j][i]=1;
                  else
                  allc[j][i]=0;
               }
         }
         
        for(p=0;p<val;p++)
        { 
             for(q=0;q<k;q++)
             {
                if(allc[q][p]==1)
                 { sumsz[q]+=size[p];
                   sumeaf[q]+=eaf[p];
                   countk[q]+=1;
                  }
             }    
        }
        for(p=0;p<k;p++)
         {  centroid[p][0]=sumsz[p]/countk[p];
            centroid[p][1]=sumeaf[p]/countk[p];
         }
      // To view the cluster remove the comments delimiter from the below lines.
      /* printf("\n allc matrixes \n");
         for(i=0;i<3;i++)
         {  for(j=0;j<val;j++)
            {
               printf("  %d",allc[i][j]);
            }   printf("\n");   }
     */

  }

}


/* init() function initializes values too all the parameters and also initial velocity and position values */
void init()
{ 
   w=0.50;
   c1=2.0;
   c2=2.0;
   noi=1000;    /* no of iterations */
   nop=20;     /* no of particles */
   gfunc=99999999;/* initially we set error to very large value to start searching */
   int i;
 
     for(i=0;i<nop;i++)
     {
        sa[i]=rnd_range(0,10);
        va[i]=rnd_range(0,2); 
        sb[i]=rnd_range(0,2);
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
     int i,k,j,cent1;
     double ga[5],gb[5],gc[5],ss,ee,ex,mare=0,temp=0;
    
     for(i=0;i<val;i++)
     {  
       scanf("%lf %lf %lf\n",&size[i],&eval[i],&eaf[i]);
     }
   
     kmean(kmeans);
     for(i=0;i<kmeans;i++)
     
     {count[i]=0;}
      /* hasht is a hash table which stores the corresponding values with their key as the cluster number */
      for(i=0;i<val;i++)
      { 
        for(k=0;k<kmeans;k++)
         {
            if(allc[k][i]==1)
              { 
                hasht[count[k]][k]=i;
                count[k]++;
              }
         }
     }
    
    // To view the allocated matrix remove the comments delimiter from the below lines.
    /*printf("\n Allocated matrixes \n");
    for(i=0;i<kmeans;i++)
      {  printf(" Cluster No. : %d ",i);
        for(j=0;j<val;j++)
            {
               printf("  %d",allc[i][j]);
            }
          printf("\n");
      }
   */
    /* cent denotes the centroid value. */
    for(cent1=0;cent1<kmeans;cent1++)
     { 
         cent=cent1;    
         init();                    /* initialize values*/
         for(i=0;i<noi;i++)
         {
            for(k=0;k<nop;k++)
            {
               updatea(k);	    /*update to find new a value*/
               updateb(k);          /*update particle to new b value*/
               updatec(k);          /*update particle to new c value*/     
               cal_fn(k);           /* calculate function fn value*/
               find_pbest(k);       /* find the personal best for this particle*/
        
            }
             
           find_gbest();            /* find global best position traversed so far*/
        }
   

        ga[cent]=gbesta;
        gb[cent]=gbestb;
        gc[cent]=gbestc;
        
        printf("Cluster  %d values: \n a=%lf \n b=%lf\n c=%lf\n",cent,gbesta,gbestb,gbestc);
        printf("\n------------------------\n");
    }
 
   //Printing the values.
   printf("no\t size            eaf      expected val     calculated val");
   for(i=0;i<kmeans;i++)
   {
      for(j=0;j<count[i];j++)
      { 
         ss=size[hasht[j][i]];
         ee=eaf[hasht[j][i]];
         ex=eval[hasht[j][i]];
         gfunc=ga[i]*pow(ss,gb[i])*ee+gc[i];
         temp=gfunc-ex;
         temp=temp<0?-temp:temp;
         temp=temp/ex;
         mare+=temp;
   
       printf("\n%d \t%lf \t%lf \t%lf \t %lf",i,ss,ee,ex,gfunc);
     }
  }
   
   printf("\n\n #### MARE =%lf###\n",mare*100/val);
   
   return 0;
} 
     

     

      












   














