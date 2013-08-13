package constants;

public class UrlMappingClass {
	
	private int key = 0; 
	private String keyString = null; 
	private String opValue = null;
	private String incompleteUrl = null;
	
	UrlMappingClass (int key, String keyString, String opValue, String incompleteUrl) throws Exception {
		if (key < 0) throw new Exception("key cannot be negative");
		if (keyString == null) throw new Exception("keyString cannot be null");
		if ("".equals(keyString)) throw new Exception("keyString cannot be empty string");
		if (opValue == null) throw new Exception("opValue cannot be null");
		if (incompleteUrl == null) throw new Exception("incompleteUrl cannot be null");
		
		this.key = key;
		this.keyString = keyString;
		this.opValue = opValue;
		this.incompleteUrl = incompleteUrl;
	}
	
	public int getKey () { return key; };
	public String getKeyString () { return keyString; };
	public String getOpValue () { return opValue; };
	public String getUrl () { 
		String url = incompleteUrl;
		if (! "".equals(opValue)) {
			url += "?op=" + opValue;
		}
		return url;
	};
	
}
