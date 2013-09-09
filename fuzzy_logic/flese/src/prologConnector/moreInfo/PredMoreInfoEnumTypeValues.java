package prologConnector.moreInfo;

import prologConnector.CiaoPrologTermInJava;

public class PredMoreInfoEnumTypeValues extends PredMoreInfoAbstract {

	public PredMoreInfoEnumTypeValues() {
	}

	@Override
	public void setMoreInfo(String predMoreInfoType, CiaoPrologTermInJava predMoreInfoInfo) {
		setType(predMoreInfoType);
		setPredMoreInfoInfo(predMoreInfoInfo);		
	}
	
	private void setPredMoreInfoInfo(CiaoPrologTermInJava predMoreInfoInfo) {
		
	}
}


