package auxiliar;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class Dates {

	public static String shortFormat = "yyyyMMdd_HHmmss";
	public static String longFormat = "yyyyMMdd_HHmmss_S";
	
	public static String getCurrentDate() {
		DateFormat dateFormat = new SimpleDateFormat(shortFormat);
		Date date = new Date();
		// System.out.println(dateFormat.format(date));
		return (dateFormat.format(date));
	}

	public static String getCurrentDate(String format) {
		DateFormat dateFormat = new SimpleDateFormat(format);
		Date date = new Date();
		// System.out.println(dateFormat.format(date));
		return (dateFormat.format(date));
	}
	
}
