

public class GCD {

 
    public static int gcd(int m, int n) {

	int aux1 = max(m,n);
	int aux2 = min(m,n);

	m = aux1;
	n = aux2;

        int r = m % n;

	return gcd_aux(r,n);

    }


    public static int gcd_aux(int r, int n){

        if (r == 0) {
            return n;
        } else {
            return gcd(n, r);
        }

    }


    public static int max(int n1, int n2){
	if (n1 >= n2) return n1;
	else return n2;
    }

    public static int min(int n1, int n2){
	if (n1 <= n2) return n1;
	else return n2;
    }


}
