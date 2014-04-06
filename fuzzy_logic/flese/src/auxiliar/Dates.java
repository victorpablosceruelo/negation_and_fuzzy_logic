package auxiliar;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

public class Dates {

	public static String shortFormat = "yyyyMMdd_HHmmss";
	public static String longFormat = "yyyyMMdd_HHmmss_S";
	
	public static String getStringOfCurrentDate() {
		return getStringOfCurrentDate(null);
	}

	public static String getStringOfCurrentDate(String format) {
		Date date = new Date();
		return getStringOfDate(date, format);
	}

	public static String getStringOfDate(Date date) {
		return getStringOfDate(date, null);
	}
	
	public static String getStringOfDate(Date date, String format) {
		if ((format == null) || ("".equals(date))) {
			format = shortFormat;
		}
		DateFormat dateFormat = new SimpleDateFormat(format);
		// System.out.println(dateFormat.format(date));
		return (dateFormat.format(date));
	}

}
