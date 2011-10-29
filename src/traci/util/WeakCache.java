package traci.util;

import java.lang.ref.WeakReference;
import java.util.WeakHashMap;

public class WeakCache<T>
{
    private final WeakHashMap<T, WeakReference<T>> map;

    public WeakCache()
    {
        map = new WeakHashMap<T, WeakReference<T>>();
    }

    public synchronized T get(final T obj)
    {
        final WeakReference<T> ref = map.get(obj);

        if (ref == null)
        {
            map.put(obj, new WeakReference<T>(obj));
            return obj;
        }
        else
        {
            return ref.get();
        }
    }
}
