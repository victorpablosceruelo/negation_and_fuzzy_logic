public class Ack{

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

}
