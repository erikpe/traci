package se.ejp.traci.render;

import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicInteger;

import se.ejp.traci.util.Log;
import se.ejp.traci.util.Utilities;

public class ProgressReporter
{
    private final long reportIntervalMs;
    private final int blocks;

    private final AtomicInteger blocksDone = new AtomicInteger(0);

    private Timer reportTimer = null;
    private long startTime;

    public ProgressReporter(final long reportIntervalMs, final int blocks)
    {
        this.reportIntervalMs = reportIntervalMs;
        this.blocks = blocks;
    }

    public void reportBlockDone()
    {
        blocksDone.incrementAndGet();
    }

    private void report()
    {
        final StringBuilder sb = new StringBuilder();

        final int done = blocksDone.get();

        sb.append((100 * done) / blocks).append("% done (");
        sb.append(done).append('/').append(blocks).append(" blocks)");

        if (done > 0 && done < blocks)
        {
            final long currentTime = System.currentTimeMillis();
            final long timeTaken = currentTime - startTime;
            final long estimatedTotalTime = (long) (timeTaken / (((double) done) / blocks));
            final long estimatedRemainingTime = estimatedTotalTime - timeTaken;

            sb.append(" Estimated time remaining: ").append(Utilities.millisecondsToString(estimatedRemainingTime));
        }

        Log.INFO(sb.toString());
    }

    public void start()
    {
        final TimerTask task = new TimerTask()
        {
            @Override
            public void run()
            {
                report();
            }
        };

        reportTimer = new Timer();
        reportTimer.schedule(task, reportIntervalMs, reportIntervalMs);
        startTime = System.currentTimeMillis();
    }

    public void finish()
    {
        reportTimer.cancel();
        report();
    }
}
