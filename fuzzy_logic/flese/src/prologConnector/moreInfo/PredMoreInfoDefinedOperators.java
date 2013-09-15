package prologConnector.moreInfo;

import java.util.ArrayList;

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

		if (predMoreInfoInfo == null)
			return;
		if (predMoreInfoInfo.isList()) {
			int length = predMoreInfoInfo.length();
			moreInfo = new String[length][];
			for (int i = 0; i < length; i++) {
				CiaoPrologTermInJava term = predMoreInfoInfo.atPosition(i);
				if ((term != null) && (term.isArray())) {
					int numOperatorFields = term.length();
					moreInfo[i] = new String[numOperatorFields];
					for (int j = 0; j < numOperatorFields; j++) {
						CiaoPrologTermInJava subTerm = term.atPosition(j);
						if (subTerm != null) {
							moreInfo[i][j] = subTerm.toString();
						}
					}
				}
			}
		}
	}

	@Override
	public String[][] getOperatorsFor(String[][] types) {
		ArrayList<String[]> result = new ArrayList<String[]>();

		if ((types == null) || (types[0].length <= 0) || (types[0][types[0].length] == null)) {
			return new String[0][];
		}

		for (int i = 0; i < moreInfo.length; i++) {
			boolean valid = false;
			for (int j = 0; j < types.length; j++) {

				valid = testIfOperatorIsOkForType(moreInfo[i], types[j]);
			}

			if (valid) {
				result.add(moreInfo[i]);
			}
		}
		
		return result.toArray(new String [result.size()][]);
	}

	private boolean testIfOperatorIsOkForType(String[] typeOp, String[] typeIn) {
		boolean ok = false;
		String lastType = typeIn[typeIn.length];
		if ("rfuzzy_enum_type".equals(lastType)) {
			ok = (("rfuzzy_enum_type".equals(typeOp[1])) || ("rfuzzy_any_type".equals(typeOp[1])));
		} else {
			ok = (!"rfuzzy_enum_type".equals(typeOp[1]));
		}
		return ok;

	}

}
