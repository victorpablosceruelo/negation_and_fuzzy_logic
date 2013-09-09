package prologConnector.moreInfo;

import prologConnector.CiaoPrologTermInJava;

public class PredMoreInfoDefinedOperators extends PredMoreInfoAbstract {

	public PredMoreInfoDefinedOperators() {
	}

	@Override
	public void setMoreInfo(String predMoreInfoType, CiaoPrologTermInJava predMoreInfoInfo) {
		setType(predMoreInfoType);
		setPredMoreInfoInfo(predMoreInfoInfo);		
	}
	
	private void setPredMoreInfoInfo(CiaoPrologTermInJava predMoreInfoInfo) {
		
	}
}


