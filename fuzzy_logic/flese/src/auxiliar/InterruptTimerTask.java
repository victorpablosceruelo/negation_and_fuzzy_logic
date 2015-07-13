package auxiliar;

import java.util.Timer;
import java.util.TimerTask;

public class InterruptTimerTask extends TimerTask {

	/*
	 * A TimerTask that interrupts the specified thread when run.
	 * http://stackoverflow.com/questions/5243233/set-running-time-limit-on-a-method-in-java
	 */

	private Thread theTread;
	boolean active;
	boolean delayAgain;

	public static InterruptTimerTask getInstance(Thread theTread) {
		return new InterruptTimerTask(theTread);
	}

	private InterruptTimerTask(Thread theTread) {
		this.theTread = theTread;
		this.active = true;
		reschedule(true);
	}

	public void reschedule() {
		reschedule(false);
	}

	public void deactivate() {
		active = false;
		this.cancel();
	}

	private void reschedule(boolean fullReschedule) {
		if (fullReschedule) {
			long delay = 120000; // 2 minutes.
			Timer timer = new Timer(true);
			timer.schedule(this, delay);
		}
		updateDelayAgain(true);
	}

	@Override
	public void run() {
		if (active) {
			if (delayAgain) {
				reschedule(true);
				updateDelayAgain(false);
			} else {
				theTread.interrupt();
			}
		}
	}

	private synchronized void updateDelayAgain(boolean value) {
		this.delayAgain = value;
	}

}
