public class Math1{

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

    //Ackermman function
    public static long ack (long m, int n) {
	if (m==0) {
	    return 1;
	} else if (n==0) {
	    if (m==1) {
		return 2;
	    } else {
		return m+2;
	    }
	} else {
	    return ack(ack (m-1,n), n-1);
	}
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

   
    //pow, version 1 
    public static double pow1 (double x, int n) {
        if (n==0) {
	    return 1.0;
        } else {
	    return x * pow1 (x,n-1);
        }
    }
  
    //pow, version2
    public static double pow (double x, int n) {
        if (n==0) return 1.0;
        double p = pow (x,n/2);
        if (n%2==0) {
	    return p*p;
        } else {
	    return x*p*p;
        }
    }
              
    // integer log
    public static int log (int n) {
	if (n==1) {
	    return 0;
	} else {
	    return 1 + log(n/2);
	}
    }

}
