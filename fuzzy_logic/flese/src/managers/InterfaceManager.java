package managers;

import storeHouse.RequestStoreHouse;
import auxiliar.NextStep;

public interface InterfaceManager {

	public NextStep processRequest();

	public NextStep getNextStep();

	public void setNextStep(NextStep nextStep);

	public NextStep getExceptionPage();

	public void byDefaultMethod() throws Exception;

	public boolean createSessionIfNull();

	public void setSessionStoreHouse(RequestStoreHouse sessionStoreHouse);

}
