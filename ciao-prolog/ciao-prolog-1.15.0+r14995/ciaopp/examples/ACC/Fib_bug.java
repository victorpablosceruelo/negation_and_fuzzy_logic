

public class Fib_bug{

 // Fibonacci (with some assertions)

/*@  
  @  requires true ;
  @
  @
  @*/

/*@
  @  ensures basic_props:string(\result) ;
  @
  @*/

/*@
  @  comp native_props:steps_o(fib(n,_),exp(5.0,int(n)));
  @
  @*/

  public static int fib (int n){
      if (n==0) return 0;
      else if (n==1) return 1;
      else return fib(n-1) + fib(n-2);

  }

    
}


