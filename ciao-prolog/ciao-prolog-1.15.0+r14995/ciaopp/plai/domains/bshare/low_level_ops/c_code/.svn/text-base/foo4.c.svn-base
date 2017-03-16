#include <stdio.h>
#include <assert.h>

/* Gray <==> binary conversion routines */
/* written by Dan T. Abell, 7 October 1993 */
/* please send any comments or suggestions */
/* to dabell@quark.umd.edu */

typedef unsigned int allele;

void gray_to_binary (Cg, Cb, n)
     /* convert chromosome of length n+1 */
     /*      from    Gray code Cg[0...n] */
     /*        to  binary code Cb[0...n] */

     allele    *Cg,*Cb;
     int  n;
{
  int j;
#if 0
  allele * cbsav, *cgsav;
  cbsav = Cb;
  cgsav = Cg;
  printf("b2g: cb is ");
  print_allele(cbsav,n);
  printf("b2g: cg is ");
  print_allele(cgsav,n);
#endif
  
  *Cb = *Cg;               /* copy the high-order bit */

#if 0
    printf("b2g: cb is ");
    print_allele(cbsav,n);
    printf("b2g: cg is ");
    print_allele(cgsav,n);
#endif

  for (j = 0; j < n; j++) {
    Cb++; Cg++;         /* for the remaining bits */
    *Cb = *(Cb-1)^*Cg;   /* do the appropriate XOR */
#if 0
    printf("b2g: cb is ");
    print_allele(cbsav,n);
    printf("b2g: cg is ");
    print_allele(cgsav,n);
#endif
  }
}


void binary_to_gray(Cb, Cg, n)
     /* convert chromosome of length n+1 */
     /*      from  binary code Cb[0...n] */
     /*        to    Gray code Cg[0...n] */
     allele    *Cb, *Cg; 
     int  n;
{
  int j;
#if 0
  allele * cbsav, *cgsav;
  cbsav = Cb;
  cgsav = Cg;
  printf("b2g: cb is ");
  print_allele(cbsav,n);
  printf("b2g: cg is ");
  print_allele(cgsav,n);
#endif

  *Cg = *Cb;               /* copy the high-order bit */

#if 0
  printf("b2g: cb is ");
  print_allele(cbsav,n);
  printf("b2g: cg is ");
  print_allele(cgsav,n);
#endif

  for (j = 0; j < n; j++) {
    Cg++; Cb++;         /* for the remaining bits */
    *Cg= *(Cb-1)^*Cb;   /* do the appropriate XOR */
#if 0
    printf("b2g: cb is ");
    print_allele(cbsav,n);
    printf("b2g: cg is ");
    print_allele(cgsav,n);
#endif
  }
}


void print_allele (targ, narg)
     allele *targ; 
     int  narg;
{
  int i = 0;
  while(i <= narg) {
    printf("<%u>",*targ);
    i++;
    targ++;
  }
  printf("\n");
}


 int main(){
   int t = 9;
   allele gt[4];
   allele bt[4];
   allele rt[4];
   int sz = 3;    /* 0...sz, length sz+1 */

   for(t=0;t<8;t++){
     int i=0;
     /*initialize bt to t, gt to 0000 */
     while(i <= sz){
       unsigned int c = (t >> i) & 0x1;
       bt[sz-i] = c;
       gt[sz-i] = 0;
       rt[sz-i] = 0;
       i++;
     }
     fprintf(stdout,"\n");
#if 0
     fprintf(stdout,"start: t is %d, sz is %d\n",t,sz);
     fprintf(stdout,"binary t is:");
     print_allele(&bt,sz);
     fprintf(stdout,"gray t is:");
     print_allele(&gt,sz);
#endif
     binary_to_gray(&bt,&gt,sz);       /* reverse back to original */
     
     fprintf(stdout,"after b2g: t is %d, sz is %d\n",t,sz);
     fprintf(stdout,"binary t is:");
     print_allele(&bt,sz);
     fprintf(stdout,"gray t is:");
     print_allele(&gt,sz);
     

#if 0
     gray_to_binary(&gt,&rt,sz);
     
     fprintf(stdout,"after g2b: t is %d, sz is %d\n",t,sz);
     fprintf(stdout,"gray t is:");
     print_allele(&gt,sz);
     fprintf(stdout,"result t is:");
     print_allele(&rt,sz);
#endif
   }
   return 0;
 }
