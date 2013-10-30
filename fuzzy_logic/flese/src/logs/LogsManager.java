package logs;

import auxiliar.Dates;
import constants.KConstants;

public class LogsManager {

	private static final String spaces = "     ";
	static LogsFiles logFileForQueries = null;
	static LogsFiles logFileForSignedUsers = null;

	public static void logSignedUser(String userName) {
		String date = Dates.getCurrentDate();
		if (logFileForSignedUsers == null) {
			newLogsFileForSignedUsers(date);
		}
		logFileForQueries.append(date + spaces + userName);
	}

	private synchronized static void newLogsFileForSignedUsers(String date) {
		if (logFileForSignedUsers == null) {
			String fileName = KConstants.Application.LogsFileForSignedUsers + date;
			logFileForSignedUsers = new LogsFiles(fileName);
		}
	}
	
	public static void logQuery(String query) {
		String date = Dates.getCurrentDate();
		if (logFileForQueries == null) {
			newLogsFileForQueries(date);
		}
		logFileForQueries.append(date + spaces + query);
	}

	private synchronized static void newLogsFileForQueries(String date) {
		if (logFileForQueries == null) {
			String fileName = KConstants.Application.LogsFileForQueries + date;
			logFileForQueries = new LogsFiles(fileName);
		}
	}
}
