package prologConnector;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.PLConnection;
import CiaoJava.PLException;
import CiaoJava.PLGoal;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import constants.KConstants;
import filesAndPaths.PathsMgmt;

public class PlConnectionEnvelope {

	final static private Log LOG = LogFactory.getLog(PlConnectionEnvelope.class);

	private int connectionId = 0;
	private PLConnection plConnection = null;
	private boolean isAvailable = false;
	PathsMgmt pathsMgmt = null;

	public PlConnectionEnvelope(int connectionId) throws Exception {
		if (connectionId <= 0) {
			throw new Exception("connectionId cannot be <= 0");
		}

		this.connectionId = connectionId;
		this.pathsMgmt = new PathsMgmt();

		String[] argv = new String[1];
		argv[0] = this.pathsMgmt.getPlServerPath();
		this.plConnection = new PLConnection(argv);

		this.isAvailable = true;
	}

	synchronized public boolean testAndSet(boolean setAvailableTo) {
		if (this.isAvailable) {
			if (!setAvailableTo) {
				// Capture it
				this.isAvailable = setAvailableTo;
				return true;
			} else {
				// Re-free it (stupid but valid).
				this.isAvailable = setAvailableTo;
				return true;
			}
		} else {
			if (setAvailableTo) {
				// Free it
				this.isAvailable = setAvailableTo;
				return true;
			} else {
				// Impossible to re-capture it (it is in use).
			}
		}
		return false;
	}

	public int getConnectionId() {
		return this.connectionId;
	}

	public void runPrologQuery(CiaoPrologQuery query) throws Exception {
		if (this.plConnection == null)
			throw new PLException("runQuery: plConnection is null.");

		PLGoal currentGoal = null;
		long answersCounter = 0;

		LOG.info("runQuery: executing query: " + query.toString() + " .... ");
		currentGoal = new PLGoal(this.plConnection, query.getQuery());

		LOG.info("runQuery: changing programFile to: " + query.getProgramFileName() + ".");
		currentGoal.useModule(query.getProgramFileName());
		currentGoal.query();

		LOG.info("performQueryAux: getting answers ... ");
		PLTerm prologQueryAnswer;
		AnswerTermInJavaClass[] answerTermInJava = null;
		long timesCounter;

		String msgsAccumulator = "";
		do { // Get all the answers you can.
			prologQueryAnswer = null;
			answerTermInJava = null;
			timesCounter = 0;
			// Save the current answer.
			answersCounter++;
			msgsAccumulator += "getting answer number: " + answersCounter + "\n";
			// LOG.info(msgsAccumulator);
			// msgsAccumulator = "";

			do { // Get the current answer.
				prologQueryAnswer = currentGoal.nextSolution();
				timesCounter++;
			} while ((prologQueryAnswer == null) && (currentGoal.isStillRunning())
					&& (timesCounter < KConstants.Queries.maximumNumberOfRetries));

			if (timesCounter >= KConstants.Queries.maximumNumberOfRetries) {
				LOG.info("performQueryAux: reached maxNumberOfTries: " + timesCounter + " >= " + KConstants.Queries.maximumNumberOfRetries);
			}

			msgsAccumulator += "goal: " + currentGoal.toString() + "\n";
			if (prologQueryAnswer != null) {
				int variablesLength = query.getVariablesLength();
				answerTermInJava = new AnswerTermInJavaClass[variablesLength];
				for (int i = 0; i < variablesLength; i++) {
					// if (i != 0) msgsAccumulator += "\t";
					msgsAccumulator += "      var[" + i + "]: ";

					PLVariable variable = query.getVariables()[i];
					if (variable != null) {
						msgsAccumulator += (variable.toString() + " bind: " + variable.getBinding());
						answerTermInJava[i] = new AnswerTermInJavaClass(variable, prologQueryAnswer);
						msgsAccumulator += " -> " + answerTermInJava[i].toString() + " \n";
					} else {
						answerTermInJava[i] = null;
						msgsAccumulator += "null -> null \n";
					}
				}
				// msgsAccumulator += "\n";
				/*
				 * preMsg += "\n   Creation MSGS: "; for (int i=0;
				 * i<variables.length; i++) { if (answerTermInJava[i] != null) {
				 * preMsg += answerTermInJava[i].getCreationMsgs(); } else {
				 * preMsg += " answerTermInJava["+i+"] is null "; } }
				 */
				query.addQueryAnswer(answerTermInJava);
				// LOG.info(msgsAccumulator);
			} else {
				// LOG.info("performQueryAux: answer obtained: null ");
				msgsAccumulator += "answer obtained: null \n";
			}

		} while ((prologQueryAnswer != null) && (answersCounter < KConstants.Queries.maximumNumberOfAnswers));

		LOG.info(msgsAccumulator);
		// LOG.info("performQueryAux: terminating goal execution ...");
		if (currentGoal != null) {
			try {
				currentGoal.terminate();
			} catch (Exception e) {
				e.printStackTrace();
			}
			currentGoal = null;
			answersCounter = -1; // Notify that there is no currentGoal.
		}

		// LOG.info("performQueryAux: end.");

	}
}
