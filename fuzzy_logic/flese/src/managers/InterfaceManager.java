package managers;

import storeHouse.SessionStoreHouse;
import auxiliar.NextStep;

public interface InterfaceManager {

	public NextStep processRequest() throws Exception;

	public NextStep getExceptionPage();
	
	public NextStep byDefaultMethod() throws Exception;

	public boolean createSessionIfNull();

	public void setSessionStoreHouse(SessionStoreHouse sessionStoreHouse);
}
