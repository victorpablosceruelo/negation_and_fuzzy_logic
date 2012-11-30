package auxiliar;

public class UrlMappingClass {
	
	private int key = 0; 
	private String keyString = null; 
	private String opValue = null;
	private String incompleteUrl = null;
	
	UrlMappingClass (int key, String keyString, String opValue, String incompleteUrl) throws Exception {
		if ((keyString == null) || (opValue == null) || (incompleteUrl == null)) 
			throw new Exception("no parameter can be null. keyString: " + keyString + " opValue: "+ opValue + 
								" incompleteUrl: " + incompleteUrl);
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
