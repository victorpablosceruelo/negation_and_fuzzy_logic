package managers;

import auxiliar.NextStep;
import urls.UrlMap;

public interface InterfaceManager {

	public void processRequest();
	public NextStep getNextPage();
	public NextStep getExceptionPage();
	
}
