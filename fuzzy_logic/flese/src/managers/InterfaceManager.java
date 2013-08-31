package managers;

import storeHouse.RequestStoreHouse;
import auxiliar.NextStep;

public interface InterfaceManager {

	public NextStep processRequest() throws Exception;

	public NextStep getExceptionPage();
	
	public NextStep byDefaultMethod() throws Exception;

	public boolean createSessionIfNull();

	public void setSessionStoreHouse(RequestStoreHouse sessionStoreHouse);
}
