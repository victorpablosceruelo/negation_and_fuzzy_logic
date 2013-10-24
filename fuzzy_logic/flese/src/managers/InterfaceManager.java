package managers;

import storeHouse.RequestStoreHouse;
import auxiliar.NextStep;

public interface InterfaceManager {

	public NextStep processRequest();

	public NextStep getNextStep();

	public void setNextStep(NextStep nextStep);

	public void byDefaultMethod() throws Exception;

	public boolean createSessionIfNull();

	public void setRequestStoreHouse(RequestStoreHouse requestStoreHouse);

	public boolean exceptionIfSessionIsNull();

	public boolean exceptionIfLocalUserInfoIsNull();

	public void actionWhenExceptionInTargetMethodInvocation(String methodName);
	
	public boolean reinitializeResultsStoreHouse();

}
