package se.ejp.traci.util;


public class Utilities
{
    public static String millisecondsToString(final long ms)
    {
        if (ms < 100)
        {
            return ms + " ms";
        }
        else
        {
            return (ms / 1000.0) + " s";
        }
    }
}
