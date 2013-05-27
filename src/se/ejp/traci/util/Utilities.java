package se.ejp.traci.util;


public class Utilities
{
    public static String millisecondsToString(long ms)
    {
        if (ms < 100)
        {
            return ms + " ms";
        }
        else if (ms < 1000 * 60)
        {
            return (ms / 1000.0) + " seconds";
        }

        final long days = ms / (24 * 60 * 60 * 1000);
        ms = ms - days * 24 * 60 * 60 * 1000;

        final long hours = ms / (60 * 60 * 1000);
        ms = ms - hours * 60 * 60 * 1000;

        final long minutes = ms / (60 * 1000);
        ms = ms - minutes * 60 * 1000;

        final long seconds = ms / 1000;

        final StringBuilder sb = new StringBuilder();

        if (days > 0)
        {
            sb.append(days).append(" days, ");
        }

        if (days > 0 || hours > 0)
        {
            sb.append(hours).append(" hours, ");
        }

        sb.append(minutes).append(" minutes, ");
        sb.append(seconds + " seconds");

        return sb.toString();
    }
}
