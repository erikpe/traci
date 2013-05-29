package se.ejp.traci.util;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class UtilitiesTest
{
    @Test
    public void testMillisecondsToString()
    {
        assertEquals("0 ms", Utilities.millisecondsToString(0));
        assertEquals("1 ms", Utilities.millisecondsToString(1));
        assertEquals("99 ms", Utilities.millisecondsToString(99));
        assertEquals("0.1 seconds", Utilities.millisecondsToString(100));
        assertEquals("0.1 seconds", Utilities.millisecondsToString(199));
        assertEquals("0.2 seconds", Utilities.millisecondsToString(200));
        assertEquals("59.9 seconds", Utilities.millisecondsToString(59999));
        assertEquals("1 minute, 0 seconds", Utilities.millisecondsToString(60000));
        assertEquals("1 minute, 0 seconds", Utilities.millisecondsToString(60999));
        assertEquals("1 minute, 1 second", Utilities.millisecondsToString(61000));
        assertEquals("2 minutes, 1 second", Utilities.millisecondsToString(60000*2 + 1000));
        assertEquals("3 days, 0 hours, 0 minutes, 0 seconds", Utilities.millisecondsToString(3*24*60*60000));
        assertEquals("3 days, 4 hours, 5 minutes, 6 seconds",
                Utilities.millisecondsToString(3*24*60*60000 + 4*60*60000 + 5*60000 + 6000));
        assertEquals("1 day, 1 hour, 1 minute, 1 second",
                Utilities.millisecondsToString(24*60*60000 + 60*60000 + 60000 + 1000));
    }
}
