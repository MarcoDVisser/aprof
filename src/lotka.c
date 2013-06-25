/*Stochastic Lotka example in C*/
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>


SEXP simc(SEXP time, SEXP alphamean, SEXP alphasd, 
SEXP rmean, SEXP rsd, SEXP K)
{

int n=length(time);
int i;

/* creat new R objects in C. */
SEXP P1, P2, result;

/* protect and allocate R objects in C. */
PROTECT(P1=allocVector(REALSXP,n));
PROTECT(P2=allocVector(REALSXP,n));


/* set list that returns results to R*/
PROTECT(result = allocVector(VECSXP, 2));


/* assign pointers to R objects */
double *p1=REAL(P1);
double *p2=REAL(P2);
double *am=REAL(alphamean);
double *as=REAL(alphasd);
double *rm=REAL(rmean);
double *rs=REAL(rsd);
double *k=REAL(K);
double R, A;

/* initial population sizes*/
p2[0]=1.0;
p1[0]=1.0;

for(i=1; i<n; i++) {
/* actual simulation, in which we update the randomnumber generator's state*/

	GetRNGstate();
	R = rnorm(rm[0],rs[0]);
	PutRNGstate();
	GetRNGstate();	
	A = rnorm(am[1],as[1]);
	PutRNGstate();

 	p1[i] = p1[i-1] * R *(1-(p1[i-1]+(A*p2[i-1]))/k[0]);
	
	GetRNGstate();
	R = rnorm(rm[1],rs[1]);
	PutRNGstate();
	GetRNGstate();	
	A = rnorm(am[0],as[0]);
	PutRNGstate();

	p2[i] = p2[i-1] * R *(1-(p2[i-1]+(A*p1[i-1]))/k[0]);
}

/* unprotect R objects in C. */

SET_VECTOR_ELT(result, 0, P1);
SET_VECTOR_ELT(result, 1, P2);
UNPROTECT(3);

return result;
}

