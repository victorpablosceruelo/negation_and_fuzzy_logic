package auxiliar;

import java.util.TimerTask;

public class InterruptTimerTask extends TimerTask {

	/*
	 * A TimerTask that interrupts the specified thread when run.
	 * http://stackoverflow.com/questions/5243233/set-running-time-limit-on-a-method-in-java
	 */

	private Thread theTread;

	public InterruptTimerTask(Thread theTread) {
		this.theTread = theTread;
	}

	@Override
	public void run() {
		theTread.interrupt();
	}

}
