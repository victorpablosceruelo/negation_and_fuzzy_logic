
public class example1{

    public static int callme(int a){
	int juan= 4;
	int x=0;

	if (juan!=2)
	    {
		x += (5*(callme(callme((8))))+5)*(8*(8));
	    }	
	else 
	    if (juan!=2)
		x = (a=6);
	    else
		juan = 9;
	x += juan;

	return x;
    }
}
