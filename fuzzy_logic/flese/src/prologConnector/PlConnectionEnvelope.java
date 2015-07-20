package prologConnector;

import java.io.IOException;
import java.util.ArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import CiaoJava.PLConnection;
import CiaoJava.PLException;
import CiaoJava.PLGoal;
import CiaoJava.PLTerm;
import CiaoJava.PLVariable;
import auxiliar.InterruptTimerTask;
import auxiliar.LocalUserInfo;
import auxiliar.RunCommand;
import constants.KConstants;
import filesAndPaths.FilesAndPathsException;
import filesAndPaths.PathsMgmt;
import logs.LogsManager;

public class PlConnectionEnvelope {

	final static private Log LOG = LogFactory.getLog(PlConnectionEnvelope.class);

	private PLGoal evaluatedGoal = null;
	private PLConnection plConnection = null;
	private PathsMgmt pathsMgmt = null;

	public PlConnectionEnvelope()
			throws PlConnectionEnvelopeException, FilesAndPathsException, CiaoPrologConnectorException {
		this.pathsMgmt = new PathsMgmt();
		this.plConnection = null;
	}

	private void createPlConnection() throws PlConnectionEnvelopeException, FilesAndPathsException {
		String[] argv = new String[1];
		argv[0] = this.pathsMgmt.getPlServerPath();

		try {
			this.plConnection = new PLConnection(argv);
		} catch (IOException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("IOException: " + e.getMessage());
		} catch (PLException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("PLException: " + e.getMessage());
		}
	}

	private void destroyPlConnection() throws PlConnectionEnvelopeException {
		if (this.plConnection != null) {
			try {
				this.plConnection.stop();
			} catch (InterruptedException e) {
				e.printStackTrace();
				throw new PlConnectionEnvelopeException("InterruptedException: " + e.getMessage());
			} catch (IOException e) {
				e.printStackTrace();
				throw new PlConnectionEnvelopeException("IOException: " + e.getMessage());
			} catch (PLException e) {
				e.printStackTrace();
				throw new PlConnectionEnvelopeException("PLException: " + e.getMessage());
			}
		}
	}

	public synchronized void runPrologQuery(CiaoPrologQueryInterface query, LocalUserInfo localUserInfo)
			throws PlConnectionEnvelopeException, CiaoPrologConnectorException, FilesAndPathsException {

		RunCommand.run("killall -q ciaoengine.LINUXi86_64 ciaoengine.LINUXi86 ciaoengine", LOG);
		InterruptTimerTask.getInstance(Thread.currentThread()).reschedule(0);
		try {
			runPrologQueryWithTimer(query, localUserInfo);
		} catch (InterruptedException e) {
			throw new PlConnectionEnvelopeException("PLConnectionException: timeout exeeded");
		} finally {
			InterruptTimerTask.getInstance(Thread.currentThread()).deactivate();
		}
	}

	private void runPrologQueryWithTimer(CiaoPrologQueryInterface query, LocalUserInfo localUserInfo)
			throws PlConnectionEnvelopeException, CiaoPrologConnectorException, FilesAndPathsException,
			InterruptedException {
		try {
			createPlConnection();
			InterruptTimerTask.getInstance(Thread.currentThread()).reschedule(0);
			changeCiaoPrologWorkingFolder(query, localUserInfo);
			InterruptTimerTask.getInstance(Thread.currentThread()).reschedule(0);
			runPrologQueryAux(query, localUserInfo);
		} finally {
			destroyPlConnection();
		}
	}

	private void changeCiaoPrologWorkingFolder(CiaoPrologQueryInterface realQuery, LocalUserInfo localUserInfo)
			throws CiaoPrologConnectorException, FilesAndPathsException, PlConnectionEnvelopeException,
			InterruptedException {

		CiaoPrologQueryInterface folderChangeQuery = CiaoPrologChangeWorkingFolderQuery
				.getInstance(realQuery.getProgramFileInfo());
		runPrologQueryAux(folderChangeQuery, localUserInfo);

		CiaoPrologQueryInterface folderCheckQuery = CiaoPrologCheckWorkingFolderQuery
				.getInstance(realQuery.getProgramFileInfo());
		runPrologQueryAux(folderCheckQuery, localUserInfo);
	}

	private void runPrologQueryAux(CiaoPrologQueryInterface query, LocalUserInfo localUserInfo)
			throws PlConnectionEnvelopeException, CiaoPrologConnectorException, InterruptedException,
			FilesAndPathsException {
		LOG.info(query.getQuery().toString());
		if (plConnection == null) {
			throw new PlConnectionEnvelopeException("ERROR: plConnection is null.");
		}

		createGoal(query, localUserInfo);
		if (!query.isOfType(CiaoPrologQueryAbstract.Constants.ChangeWorkingFolderQuery)) {
			// Thread.sleep(1000);
			changeProgramFileTo(query);
		}
		// Thread.sleep(1000);
		evaluateGoal();

		long answersCounter = 0;

		LOG.info("performQueryAux: getting answers ... ");
		PLTerm prologQueryAnswer;
		long timesCounter;
		boolean queryIsStillRunning = true;

		ArrayList<CiaoPrologQueryAnswer> ciaoPrologQueryAnswers = new ArrayList<CiaoPrologQueryAnswer>();

		StringBuilder msgsAccumulator = new StringBuilder();
		do { // Get all the answers you can.
			prologQueryAnswer = null;
			timesCounter = 0;
			// Save the current answer.
			answersCounter++;
			msgsAccumulator.append("getting answer number: " + answersCounter + "\n");
			// LOG.info(msgsAccumulator);
			// msgsAccumulator = "";

			do { // Get the current answer.
				prologQueryAnswer = getNextSolution();
				queryIsStillRunning = getQueryIsStillRunning();
				timesCounter++;
				InterruptTimerTask.getInstance(Thread.currentThread()).reschedule(0);

			} while ((prologQueryAnswer == null) && queryIsStillRunning
					&& (timesCounter < KConstants.CiaoPrologQuery.maximumNumberOfRetries));

			if (timesCounter >= KConstants.CiaoPrologQuery.maximumNumberOfRetries) {
				LOG.info("performQueryAux: reached maxNumberOfTries: " + timesCounter + " >= "
						+ KConstants.CiaoPrologQuery.maximumNumberOfRetries);
			}

			msgsAccumulator.append("goal: " + evaluatedGoal.toString() + "\n");
			if (prologQueryAnswer != null) {
				CiaoPrologQueryAnswer ciaoPrologQueryAnswer = new CiaoPrologQueryAnswer();
				int variablesLength = query.getVariablesLength();
				for (int i = 0; i < variablesLength; i++) {
					msgsAccumulator.append("      var[" + i + "]: ");

					PLVariable variable = query.getVariables()[i];
					String variableName = query.getVariablesNames()[i];
					CiaoPrologTermInJava ciaoPrologTermInJava = null;

					if (variable != null) {
						msgsAccumulator.append(variable.toString());
						msgsAccumulator.append(" bind: ");
						msgsAccumulator.append(variable.getBinding());
						ciaoPrologTermInJava = new CiaoPrologTermInJava(variable, prologQueryAnswer);
						msgsAccumulator.append(" -> " + ciaoPrologTermInJava.toString() + " \n");
					} else {
						ciaoPrologTermInJava = null;
						msgsAccumulator.append("null -> null \n");
					}

					ciaoPrologQueryAnswer.addCiaoPrologVariableAnswer(variableName, ciaoPrologTermInJava);
					InterruptTimerTask.getInstance(Thread.currentThread()).reschedule(0);
				}
				ciaoPrologQueryAnswers.add(ciaoPrologQueryAnswer);
				// LOG.info(msgsAccumulator);
			} else {
				// LOG.info("performQueryAux: answer obtained: null ");
				msgsAccumulator.append("answer obtained: null \n");
			}

			InterruptTimerTask.getInstance(Thread.currentThread()).reschedule(0);

		} while ((prologQueryAnswer != null) && (answersCounter < KConstants.CiaoPrologQuery.maximumNumberOfAnswers));

		InterruptTimerTask.getInstance(Thread.currentThread()).deactivate();

		CiaoPrologQueryAnswer[] ciaoPrologQueryAnswersArray = ciaoPrologQueryAnswers
				.toArray(new CiaoPrologQueryAnswer[ciaoPrologQueryAnswers.size()]);
		query.setQueryAnswers(ciaoPrologQueryAnswersArray);

		LOG.info(msgsAccumulator.toString());
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

	private void createGoal(CiaoPrologQueryInterface query, LocalUserInfo localUserInfo)
			throws PlConnectionEnvelopeException, CiaoPrologConnectorException {
		LOG.info("runQuery: creating goal for query: " + query.toString() + " .... ");
		LogsManager.logQuery(query, localUserInfo);
		evaluatedGoal = new PLGoal(plConnection, query.getQuery());
	}

	private void changeProgramFileTo(CiaoPrologQueryInterface query)
			throws PlConnectionEnvelopeException, CiaoPrologConnectorException, FilesAndPathsException {
		query.getProgramFileInfo().ensureFolderIsAccessibleAndCleanCiaoPrologTmpFiles();

		String programFileName = query.getProgramFileInfo().getFileName();
		LOG.info("runQuery: changing programFile to: " + programFileName + " .... ");
		try {
			evaluatedGoal.useModule(programFileName);
			LOG.info("runQuery: changed programFile to: " + programFileName + " .... ");
		} catch (IOException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("IOException");
		} catch (PLException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("PLException");
		}
	}

	private void evaluateGoal() throws PlConnectionEnvelopeException, CiaoPrologConnectorException {
		LOG.info("runQuery: executing query .... ");
		try {
			evaluatedGoal.query();
		} catch (IOException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("IOException");
		} catch (PLException e) {
			e.printStackTrace();
			throw new PlConnectionEnvelopeException("PLException");
		}
	}

	private PLTerm getNextSolution() throws PlConnectionEnvelopeException {
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

	private boolean getQueryIsStillRunning() throws PlConnectionEnvelopeException {
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
