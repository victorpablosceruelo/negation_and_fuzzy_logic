package prologConnector;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.PLConnection;
import CiaoJava.PLException;
import CiaoJava.PLGoal;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import constants.KConstants;
import filesAndPaths.FilesAndPathsException;

public class PLConnectionUtils {

	final static private Log LOG = LogFactory.getLog(PLConnectionUtils.class);

	public static void runPrologQuery(CiaoPrologQueryInterface query, PLConnection plConnection)
			throws PlConnectionEnvelopeException, CiaoPrologConnectorException, FilesAndPathsException {

		changeCiaoPrologWorkingFolder(query, plConnection);
		runPrologQueryAux(query, plConnection);

	}

	private static void changeCiaoPrologWorkingFolder(CiaoPrologQueryInterface realQuery, PLConnection plConnection)
			throws CiaoPrologConnectorException, FilesAndPathsException, PlConnectionEnvelopeException {

		CiaoPrologQueryInterface folderChangeQuery = CiaoPrologChangeWorkingFolderQuery.getInstance(realQuery.getProgramFileInfo());
		runPrologQueryAux(folderChangeQuery, plConnection);
	}

	private static void runPrologQueryAux(CiaoPrologQueryInterface query, PLConnection plConnection)
			throws PlConnectionEnvelopeException, CiaoPrologConnectorException, FilesAndPathsException {
		LOG.info(query.getQuery().toString());
		if (plConnection == null) {
			throw new PlConnectionEnvelopeException("ERROR: plConnection is null.");
		}

		PLGoal evaluatedGoal = evaluateGoal(query, plConnection);
		long answersCounter = 0;

		LOG.info("performQueryAux: getting answers ... ");
		PLTerm prologQueryAnswer;
		long timesCounter;
		boolean queryIsStillRunning = true;

		String msgsAccumulator = "";
		do { // Get all the answers you can.
			prologQueryAnswer = null;
			timesCounter = 0;
			// Save the current answer.
			answersCounter++;
			msgsAccumulator += "getting answer number: " + answersCounter + "\n";
			// LOG.info(msgsAccumulator);
			// msgsAccumulator = "";

			do { // Get the current answer.
				prologQueryAnswer = getNextSolution(evaluatedGoal);
				queryIsStillRunning = getQueryIsStillRunning(evaluatedGoal);
				timesCounter++;

			} while ((prologQueryAnswer == null) && queryIsStillRunning
					&& (timesCounter < KConstants.CiaoPrologQuery.maximumNumberOfRetries));

			if (timesCounter >= KConstants.CiaoPrologQuery.maximumNumberOfRetries) {
				LOG.info("performQueryAux: reached maxNumberOfTries: " + timesCounter + " >= "
						+ KConstants.CiaoPrologQuery.maximumNumberOfRetries);
			}

			msgsAccumulator += "goal: " + evaluatedGoal.toString() + "\n";
			if (prologQueryAnswer != null) {
				CiaoPrologQueryAnswer ciaoPrologQueryAnswer = new CiaoPrologQueryAnswer();
				int variablesLength = query.getVariablesLength();
				for (int i = 0; i < variablesLength; i++) {
					msgsAccumulator += "      var[" + i + "]: ";

					PLVariable variable = query.getVariables()[i];
					String variableName = query.getVariablesNames()[i];
					CiaoPrologTermInJava ciaoPrologTermInJava = null;

					if (variable != null) {
						msgsAccumulator += (variable.toString() + " bind: " + variable.getBinding());
						ciaoPrologTermInJava = new CiaoPrologTermInJava(variable, prologQueryAnswer);
						msgsAccumulator += " -> " + ciaoPrologTermInJava.toString() + " \n";
					} else {
						ciaoPrologTermInJava = null;
						msgsAccumulator += "null -> null \n";
					}

					ciaoPrologQueryAnswer.addCiaoPrologVariableAnswer(variableName, ciaoPrologTermInJava);
				}
				query.addQueryAnswer(ciaoPrologQueryAnswer);
				// LOG.info(msgsAccumulator);
			} else {
				// LOG.info("performQueryAux: answer obtained: null ");
				msgsAccumulator += "answer obtained: null \n";
			}

		} while ((prologQueryAnswer != null) && (answersCounter < KConstants.CiaoPrologQuery.maximumNumberOfAnswers));

		LOG.info(msgsAccumulator);
		// LOG.info("performQueryAux: terminating goal execution ...");
		if (evaluatedGoal != null) {
			try {
				evaluatedGoal.terminate();
			} catch (Exception e) {
				e.printStackTrace();
			}
			evaluatedGoal = null;
			answersCounter = -1; // Notify that there is no currentGoal.
		}

		// LOG.info("performQueryAux: end.");

	}

	private static PLGoal evaluateGoal(CiaoPrologQueryInterface query, PLConnection plConnection)
			throws PlConnectionEnvelopeException, CiaoPrologConnectorException, FilesAndPathsException {
		LOG.info("runQuery: executing query: " + query.toString() + " .... ");
		PLGoal evaluatedGoal = new PLGoal(plConnection, query.getQuery());
		String programFileName = null;

		programFileName = query.getProgramFileInfo().getFileName();

		LOG.info("runQuery: changing programFile to: " + programFileName + ".");
		try {
			evaluatedGoal.useModule(programFileName);
			evaluatedGoal.query();
		} catch (IOException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("IOException");
		} catch (PLException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("PLException");
		}
		return evaluatedGoal;
	}

	private static PLTerm getNextSolution(PLGoal evaluatedGoal) throws PlConnectionEnvelopeException {
		PLTerm prologQueryAnswer = null;
		try {
			prologQueryAnswer = evaluatedGoal.nextSolution();
		} catch (IOException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("IOException");
		} catch (PLException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("PLException");
		}
		return prologQueryAnswer;
	}

	private static boolean getQueryIsStillRunning(PLGoal evaluatedGoal) throws PlConnectionEnvelopeException {
		boolean queryIsStillRunning = false;
		try {
			queryIsStillRunning = evaluatedGoal.isStillRunning();
		} catch (IOException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("IOException");
		} catch (PLException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("PLException");
		}
		return queryIsStillRunning;
	}
}
