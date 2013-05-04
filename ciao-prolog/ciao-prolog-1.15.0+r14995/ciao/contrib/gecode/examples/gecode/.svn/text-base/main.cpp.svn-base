#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include <gecode/search.hh>
#include <cstdlib>
#include <time.h>

using namespace Gecode;

// #include "all-interval.hpp"
#include "crypta.hpp"
#include "labeling.hpp"
#include "alpha.hpp"

int main(int argc, char* argv[])
{
  int pasa;
  double secs;
  Search::Options opt;
  opt.c_d = 1;
  clock_t inicio, final;
  inicio = clock();
  pasa = 0;

//   {
//     AllInterval *ini = new AllInterval;
//     DFS<AllInterval> e(ini);
//     delete ini;
    
//     printf("All-Interval:\n");
//     while (AllInterval *s = e.next()) {
//       if (!pasa)
// 	{
// 	  final = clock();
// 	  secs = (double)(final - inicio) / CLOCKS_PER_SEC;
// 	  printf("First answer: %.16g milisegundos\n", secs * 1000.0);
// 	}
//       pasa = 1;
//       delete s;
//     }
//     final = clock();
//     secs = (double)(final - inicio) / CLOCKS_PER_SEC;
//     printf("All answers: %.16g milisegundos\n", secs * 1000.0);
//   }
  
  {
    inicio = clock();
    pasa = 0;
    Crypta *ini = new Crypta;
    ini->make_dist();
    DFS<Crypta> e(ini,opt);
    delete ini;
 
    printf("Crypta:\n"); fflush(stdout);
    while (Crypta *s = e.next()) {
      if (!pasa)
	{
	  final = clock();
	  secs = (double)(final - inicio) / CLOCKS_PER_SEC;
	  printf("First answer: %.16g milisegundos\n", secs * 1000.0);
	}
      pasa = 1;
      delete s;
    }
    final = clock();
    secs = (double)(final - inicio) / CLOCKS_PER_SEC;
    printf("All answers: %.16g milisegundos\n", secs * 1000.0);    
  }

  {
    inicio = clock();
    pasa = 0;
    Labeling *ini = new Labeling;
    DFS<Labeling> e(ini,opt);
    delete ini;
 
    printf("Labeling:\n");
    while (Labeling *s = e.next()) {
      if (!pasa)
	{
	  final = clock();
	  secs = (double)(final - inicio) / CLOCKS_PER_SEC;
	  printf("First answer: %.16g milisegundos\n", secs * 1000.0);
	}
      pasa = 1;
      delete s;
    }
    final = clock();
    secs = (double)(final - inicio) / CLOCKS_PER_SEC;
    printf("All answers: %.16g milisegundos\n", secs * 1000.0);    
  }

  {
    inicio = clock();
    pasa = 0;
    Alpha *ini = new Alpha;
    DFS<Alpha> e(ini,opt);
    delete ini;
    
    printf("Alpha:\n");
    while (Alpha *s = e.next()) {
      if (!pasa)
	{
	  final = clock();
	  secs = (double)(final - inicio) / CLOCKS_PER_SEC;
	  printf("First answer: %.16g milisegundos\n", secs * 1000.0);
	}
      pasa = 1;
      delete s;
    }
    final = clock();
    secs = (double)(final - inicio) / CLOCKS_PER_SEC;
    printf("All answers: %.16g milisegundos\n", secs * 1000.0);    
  }

  return 0;
}

