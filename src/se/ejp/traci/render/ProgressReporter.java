package se.ejp.traci.render;

import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicInteger;

import se.ejp.traci.util.Log;

public class ProgressReporter
{
    private final long reportIntervalMs;
    private final int blocks;

    private final AtomicInteger blocksDone = new AtomicInteger(0);

    private Timer reportTimer = null;

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
        final int done = blocksDone.get();
        Log.INFO(((100 * done) / blocks) + "% done (" + done + "/" + blocks + " blocks)");
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
    }

    public void finish()
    {
        reportTimer.cancel();
        report();
    }
}
