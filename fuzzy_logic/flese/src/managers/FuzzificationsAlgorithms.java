package managers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Set;

public class FuzzificationsAlgorithms {
	public static String[][] algo(ArrayList<HashMap<String,String>> a)
	{
		//return averageAlgo();
		return medianAlgo(a);
	}
	
	//make an average of the value of the fuzzifications
	public static String[][] averageAlgo(ArrayList<HashMap<String,String>> a)
    {
        //problem cases
        if ((a == null)||(a.size() == 0))
        {
            return new String[0][0];
        }
       
        Set<String> strSet = a.get(0).keySet();
        String[][] resul = new String[strSet.size()][2];
         double avg = 0;
         int i = 0;
        for (String key: strSet)
        {
             avg = 0;
             double v;
            for (HashMap<String, String> entry: a) {
                v = Double.parseDouble(entry.get(key));
                avg += v;
            }
            resul[i][0] = key;
            Double value = new Double(Math.round(avg*1000/a.size())/1000.);
              if (((value*10) % 10) == 0)
              {
              	resul[i][1] = String.valueOf(value.intValue());
              	} else {
              	resul[i][1] = String.valueOf(value);
              	}
            i++;
        }
            return resul;
        }
	
	//make an average of the value of the fuzzifications
	public static String[][] medianAlgo(ArrayList<HashMap<String,String>> a)
    {
        //problem cases
        if ((a == null)||(a.size() == 0))
        {
            return new String[0][0];
        }
       
        Set<String> strSet = a.get(0).keySet();
        String[][] resul = new String[strSet.size()][2];
         int i = 0;
        for (String key: strSet)
        {
             List<Double> contents = new ArrayList<Double>();
             double v;
            for (HashMap<String, String> entry: a) {
                v = Double.parseDouble(entry.get(key));
                contents.add(v);
            }
            resul[i][0] = key;
        	Collections.sort(contents, new Comparator<Double>() {
        	    @Override
        	    public int compare(Double c1, Double c2) {
        	        return Double.compare(c1, c2);
        	    }
        	});
            resul[i][1]=contents.get(Math.round((contents.size()+1)/2)-1).toString();
            i++;
        }
		//LOG.info("Default rule updated with: " + resul);
            return resul;
        }

	//use this method to test the fuzzification algorithm
	/*
	public static void main(String [ ] args)
	{
	ArrayList<HashMap<String,String>> a = new ArrayList<HashMap<String,String>>();
	HashMap<String,String> h1 = new HashMap<String,String>();
	h1.put("50","0.1");
	h1.put("100","0.4");
	h1.put("150","0.7");
	a.add(h1);
	HashMap<String,String> h2 = new HashMap<String,String>();
	h2.put("50","0.2");
	h2.put("100","0.4");
	h2.put("150","0.6");
	a.add(h2);
	HashMap<String,String> h3 = new HashMap<String,String>();
	h3.put("50","0.3");
	h3.put("100","0.4");
	h3.put("150","0.5");
	a.add(h3);
	HashMap<String,String> h4 = new HashMap<String,String>();
	h4.put("50","0.4");
	h4.put("100","0.4");
	h4.put("150","0.4");
	a.add(h4);
	HashMap<String,String> h5 = new HashMap<String,String>();
	h5.put("50","0.5");
	h5.put("100","0.4");
	h5.put("150","0.3");
	a.add(h5);
	String[][] tab = medianAlgo(a);
	System.out.println(tab[0][0]);
	System.out.println(tab[0][1]);
	System.out.println(tab[1][0]);
	System.out.println(tab[1][1]);
	System.out.println(tab[2][0]);
	System.out.println(tab[2][1]);
	}*/
	
}
