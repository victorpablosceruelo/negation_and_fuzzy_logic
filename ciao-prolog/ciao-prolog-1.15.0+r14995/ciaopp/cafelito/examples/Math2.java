public class Math2{

    /**
     * Math examples
     *
     **/

    // Sum from 1 to N
    public static int sum1toN(int n){
	int sum = 0;
        while (n>=1){
	    sum=sum+n;
	    n=n-1;
	}
	return sum;
    }

    //Greatest common divisor
    public static int gcd(int m, int n) {

        if (m < n) {
            int t = m;
            m = n;
            n = t;
        }

        int r = m % n;

        if (r == 0) {
            return n;
        } else {
            return gcd(n, r);
        }
    }


    //Fibbonacci
    public static int fib (int n){
	if (n==0) return 0;
	else if (n==1) return 1;
	else return fib(n-1) + fib(n-2);

    }


    //recursive factorial
    public static int recfact(int n) {
	if ( n == 1)
	    return 1;
	else
	    return n * recfact(n-1);
    }

    // TODO
    //factorial iterative version still to be done (for!)

   
              
    // integer log
    public static int log (int n) {
	if (n==1) {
	    return 0;
	} else {
	    return 1 + log(n/2);
	}
    }

}
