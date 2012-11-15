package auxiliar;

public class FunctionPoint {

	private String coordinate_1 = null;
	private String coordinate_2 = null;
	
	public static String floatPattern = "([\\d]+(\\.[\\d]+)?){1}";
	
	public FunctionPoint(String coordinate_1, String coordinate_2) throws Exception {
		if (! coordinate_1.matches(floatPattern))
			throw new Exception("coordinate_1 is not a valid number. coordinate_1: " + coordinate_1);
		if (! coordinate_2.matches(floatPattern))
			throw new Exception("coordinate_2 is not a valid number. coordinate_2: " + coordinate_2);
		
		this.coordinate_1 = coordinate_1;
		this.coordinate_2 = coordinate_2;
	}
	
	public String toString () {
		return " coordinates: 1: " + coordinate_1 + " 2: " + coordinate_2 + " ";
	}
	
	public String toJavaScript () {
		return "new Array(" + coordinate_1 + ", " + coordinate_2 + ")";
	}
}
