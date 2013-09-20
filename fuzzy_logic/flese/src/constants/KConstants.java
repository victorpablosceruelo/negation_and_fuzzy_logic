package constants;

public class KConstants {

	public static final String appPath = "/flese/";
	public static final String servletName = "/Servlet";

	public static final class Managers {
		public static final String managersPackage = "managers";
		public static final String managerSuffix = "Manager";
	}

	public static class Values {
		public static final String True = "true";
		public static final String False = "false";
	}

	public static class Request {
		// Objects stored in the request.
		public static final String resultsStoreHouse = "resultsStoreHouse";
		
		// General parameters
		public static final String managerParam = "manager";
		public static final String operationParam = "op";
		public static final String fileNameParam = "fileName";
		public static final String fileOwnerParam = "fileOwner";
		public static final String providerId = "id";
		public static final String isAjaxParam = "ajax";

		// Query parameters.
		public static final String databaseParam = "db";
		public static final String predicateParam = "pred";
		public static final String linesCounterParam = "linesCount";
		public static final String lineNumberParam = "lineNum";
		public static final String lineIdParam = "lineId";
		public static final String aggregatorParam = "queryLines.selectAggregator";
		public static final String programParam = "chooseProgram";
		public static final String negationParam = "negation";
		public static final String quantifierParam = "quantifier";
		public static final String operatorParam = "operator";
		public static final String valueParam = "value";

	}

	public static class PathsMgmt {
		public static final String[] programFilesValidPaths = { "/home/java-apps/fuzzy-search/",
				System.getProperty("java.io.tmpdir") + "/java-apps/fuzzy-search/",
				// servlet.getServletContext().getInitParameter("working-folder-fuzzy-search"),
				"/tmp/java-apps/fuzzy-search/" };

		public static final String[] plServerValidSubPaths = { "/home/tomcat/ciao-prolog-1.15.0+r14854/ciao/library/javall/plserver",
				"/usr/share/CiaoDE/ciao/library/javall/plserver", "/usr/lib/ciao", "/usr/share/CiaoDE", "/usr", "/opt", "/home", "/" };

		public static final String plServerProgramFileName = "plserver";
	}

	public static class Communications {
		public static final int BUFSIZE = 4096;
		public static final int maxFileSize = 50000 * 1024;
		public static final int maxMemSize = 50000 * 1024;
	}

	public static class CiaoPrologQuery {
		public static final long maximumNumberOfRetries = 9223372036854775807L;
		public static final long maximumNumberOfAnswers = 9223372036854775807L;

	}

	public static class PlConnectionsPool {
		public static final int maxNumOfConnections = 10;
	}

	public static class Session {
		public static final String swAppInTestingMode = "swAppInTestingMode";
		public static final String socialAuthManager = "socialAuthManager";
		public static final String socialAuthProvider = "socialAuthProvider";
		public static final String socialAuthProviderId = "socialAuthProviderId";
		public static final String socialAuthProfile = "socialAuthProfile";
		public static final String localUserInfo = "localUserInfo";

	}

	public static class NextStep {
		public static final int none = 0;
		public static final int forward_to = 1;
		public static final int redirect_to = 2;
		public static final int redirect_to_with_session = 3;
		public static final int sendRedirect_to = 4;
		public static final int invalidAction = 5;
	}

	public static class Fuzzifications {
		public static final String predDefined = "predDefined";
		public static final String predNecessary = "predNecessary";
		public static final String predOwner = "predOwner";
	}

	public static class ProgramIntrospectionFields {
		public static final String predicateName = "predicateName";
		public static final String predicateArity = "predicateArity";
		public static final String predicateTypes = "predicateTypes";
		public static final String predicateMoreInfo = "predicateMoreInfo";
	}

	public static class PrologTypes {
		public static final String rfuzzy_any_type = "rfuzzy_any_type";
		public static final String rfuzzy_truth_value_type = "rfuzzy_truth_value_type";
		
		public static final String rfuzzy_enum_type = "rfuzzy_enum_type";
		public static final String rfuzzy_string_type = "rfuzzy_string_type";

		public static final String rfuzzy_number_type = "rfuzzy_number_type";
		public static final String rfuzzy_integer_type = "rfuzzy_integer_type";
		public static final String rfuzzy_float_type = "rfuzzy_float_type";

	}

	public static class MoreInfoTypes {
		public static final String fuzzyRule = "fuzzy_rule";
		public static final String database = "database";
		public static final String enumTypeValues = "rfuzzy_enum_type_values";
		public static final String dbField = "rfuzzy_db_field";
		public static final String definedOperators = "defined_operators";
		public static final String similarityClause = "rfuzzy_similarity_clause";
	}

	public static class JavaScriptScripts {
		public static final String jsRegex = "<script[^>]*>[^<]*</script>";
		public static final String jsStart = "<script type=\"text/javascript\">";
		public static final String jsEnd = "</script>";
	}

	public static class JspsDivsIds {
		public static final String queryStartContainerId = "queryStartContainer";
		public static final String selectQueryDivId = "selectQueryDiv";
		public static final String runQueryDivId = "runQueryDiv";
		public static final String queryLinesContainerId = "queryLinesContainer";
		public static final String searchOrPersonalizeTableId = "searchOrPersonalizeTable";
		public static final String queryLinesTableId = "queryLinesTable";
		public static final String queryLinesAggregatorTableId = "queryLinesAggregatorTable";		
	}
}

// **********************************
// **********************************
// **********************************

