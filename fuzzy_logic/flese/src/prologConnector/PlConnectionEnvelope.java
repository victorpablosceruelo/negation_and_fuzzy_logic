package prologConnector;

import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.PLConnection;
import CiaoJava.PLException;
import CiaoJava.PLGoal;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import auxiliar.LocalUserInfoException;
import constants.KConstants;
import filesAndPaths.PathsMgmt;
import filesAndPaths.PathsMgmtException;

public class PlConnectionEnvelope {

	final static private Log LOG = LogFactory.getLog(PlConnectionEnvelope.class);

	private int connectionId = 0;
	private PLConnection plConnection = null;
	private boolean isAvailable = false;
	PathsMgmt pathsMgmt = null;

	public PlConnectionEnvelope(int connectionId) throws PlConnectionEnvelopeException, PathsMgmtException {
		if (connectionId <= 0) {
			throw new PlConnectionEnvelopeException("connectionId cannot be <= 0");
		}

		this.connectionId = connectionId;
		this.pathsMgmt = new PathsMgmt();
		this.isAvailable = true;

		createPlConnection();
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

	public void runPrologQuery(CiaoPrologQuery query) throws PlConnectionEnvelopeException, AnswerTermInJavaClassException {
		
		changeCiaoPrologWorkingFolder(query);
		runPrologQueryAux(query);

	}
	
	private void changeCiaoPrologWorkingFolder(CiaoPrologQuery realQuery) {

		
		CiaoPrologQuery folderChangeQuery = new CiaoPrologQuery(query, programFilesPath, fileOwner, fileName, variables, variablesNames);
		
	}
	
	private void runPrologQueryAux(CiaoPrologQuery query) throws PlConnectionEnvelopeException, AnswerTermInJavaClassException {
		if (this.isAvailable) {

		}
		if (this.plConnection == null)
			throw new PlConnectionEnvelopeException("runQuery: plConnection is null.");

		PLGoal evaluatedGoal = evaluateGoal(query);
		long answersCounter = 0;

		LOG.info("performQueryAux: getting answers ... ");
		PLTerm prologQueryAnswer;
		AnswerTermInJavaClass[] answerTermInJava = null;
		long timesCounter;
		boolean queryIsStillRunning = true;

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
				prologQueryAnswer = getNextSolution(evaluatedGoal);
				queryIsStillRunning = getQueryIsStillRunning(evaluatedGoal);
				timesCounter++;

			} while ((prologQueryAnswer == null) && queryIsStillRunning && (timesCounter < KConstants.Queries.maximumNumberOfRetries));

			if (timesCounter >= KConstants.Queries.maximumNumberOfRetries) {
				LOG.info("performQueryAux: reached maxNumberOfTries: " + timesCounter + " >= " + KConstants.Queries.maximumNumberOfRetries);
			}

			msgsAccumulator += "goal: " + evaluatedGoal.toString() + "\n";
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

	private void createPlConnection() throws PlConnectionEnvelopeException {
		String[] argv = new String[1];
		argv[0] = this.pathsMgmt.getPlServerPath();

		try {
			this.plConnection = new PLConnection(argv);
		} catch (IOException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("IOException");
		} catch (PLException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("PLException");
		}
	}

	private PLGoal evaluateGoal(CiaoPrologQuery query) throws PlConnectionEnvelopeException {
		LOG.info("runQuery: executing query: " + query.toString() + " .... ");
		PLGoal evaluatedGoal = new PLGoal(this.plConnection, query.getQuery());
		String programFileName = null;

		try {
			programFileName = query.getProgramFileName();
		} catch (PathsMgmtException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("PathsMgmtException");
		} catch (LocalUserInfoException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("LocalUserInfoException");
		}

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

	private PLTerm getNextSolution(PLGoal evaluatedGoal) throws PlConnectionEnvelopeException {
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

	private boolean getQueryIsStillRunning(PLGoal evaluatedGoal) throws PlConnectionEnvelopeException {
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
