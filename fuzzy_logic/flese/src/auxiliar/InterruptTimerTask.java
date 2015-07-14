package auxiliar;

import java.util.Timer;
import java.util.TimerTask;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class InterruptTimerTask extends TimerTask {

	final static private Log LOG = LogFactory.getLog(InterruptTimerTask.class);
	
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
		LOG.info("New InterruptTimerTask");
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
		LOG.info("Deactivated InterruptTimerTask");
	}

	private void reschedule(boolean fullReschedule) {
		if (fullReschedule) {
			long delay = 120000; // 2 minutes.
			Timer timer = new Timer(true);
			timer.schedule(this, delay);
			LOG.info("Full reschedule");
		}
		else {
			LOG.info("Partial reschedule");
		}
		updateDelayAgain(true);
	}

	@Override
	public void run() {
		if (active) {
			if (delayAgain) {
				LOG.info("Not interrupting due to delayAgain");
				reschedule(true);
				updateDelayAgain(false);
			} else {
				LOG.info("Interrupting");
				theTread.interrupt();
			}
		}
	}

	private synchronized void updateDelayAgain(boolean value) {
		this.delayAgain = value;
	}

}
