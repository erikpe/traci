package se.ejp.traci.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;

import org.junit.Test;

public class WeakCacheTest
{
    @Test
    public void testWeakCache()
    {
        final WeakCache<String> cache = new WeakCache<String>();

        final String s0 = new String("hej");
        final String s1 = new String("hej");
        final String s2 = new String("hopp");

        assertNotSame(s0, s1);
        assertEquals(s0, s1);
        String res = cache.get(s0);
        assertSame(s0, res);
        res = cache.get(s1);
        assertSame(s0, res);
        res = cache.get(s2);
        assertSame(s2, res);
    }
}
