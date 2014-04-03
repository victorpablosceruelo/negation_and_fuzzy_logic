package auxiliar;

public class RegistryEntry {

	private String date;
	private String manager;
	private String op;
	private String msg;
	
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
		
		date = Dates.getCurrentDate();
		this.manager = manager;
		this.op = op;
		this.msg = msg;
	}
	
	public String getDate() {
		return date;
	}

	public String getManager() {
		return manager;
	}

	public String getOp() {
		return op;
	}

	public String getMsg() {
		return msg;
	}
	
}
