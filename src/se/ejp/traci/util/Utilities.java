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

    public static int approxCompareDouble(final Double d0, final Double d1)
    {
        if (!(d0.isInfinite() || d1.isInfinite() || d0.isNaN() || d1.isNaN()))
        {
            if (Math.abs(d0 - d1) <= Math.min(Math.ulp(d0), Math.ulp(d1)))
            {
                return 0;
            }
        }

        return d0.compareTo(d1);
    }

    public static boolean approxEqualsDouble(final Double d0, final Double d1)
    {
        if (!(d0.isInfinite() || d1.isInfinite() || d0.isNaN() || d1.isNaN()))
        {
            if (Math.abs(d0 - d1) <= Math.min(Math.ulp(d0), Math.ulp(d1)))
            {
                return true;
            }
        }

        return d0.equals(d1);
    }
}
