package auxiliar;

public class RegistryEntry {

	private String dateIn;
	private String dateOut;
	private String manager;
	private String op;
	private String msg;
	private String nextStep;

	public RegistryEntry(String manager, String op, String msg) {
		if (manager == null) {
			manager = "";
		}
		if (op == null) {
			op = "";
		}
		if (msg == null) {
			msg = "";
		}

		this.dateIn = Dates.getCurrentDate(Dates.longFormat);
		this.dateOut = Dates.getCurrentDate(Dates.longFormat);
		this.manager = manager;
		this.op = op;
		this.msg = msg;
		this.nextStep = "";
	}

	public RegistryEntry(RegistryEntry registryEntry, NextStep nextStep, boolean isAjax) {
		this.dateIn = (registryEntry == null) ? "" : registryEntry.getDateIn();
		this.dateOut = Dates.getCurrentDate(Dates.longFormat);
		this.manager = (registryEntry == null) ? "" : registryEntry.getManager();
		this.op = (registryEntry == null) ? "" : registryEntry.getOp();
		this.msg = (registryEntry == null) ? "" : registryEntry.getMsg();
		this.nextStep = "";

		if (nextStep != null) {
			this.nextStep = nextStep.getLoggingInformation(isAjax);
		}
	}

	public void setMsg(String msg) {
		if ((this.msg != null) && (!"".equals(msg))) {
			this.msg = msg;
		}
	}
	
	public String getDateIn() {
		return this.dateIn;
	}

	public String getDateOut() {
		return this.dateOut;
	}

	public String getManager() {
		return this.manager;
	}

	public String getOp() {
		return this.op;
	}

	public String getMsg() {
		return this.msg;
	}

	public String getNextStep() {
		return this.nextStep;
	}

}
