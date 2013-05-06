package se.ejp.traci.math;

import java.lang.reflect.Array;

abstract public class ObjectPool<T>
{
    private static final int DEFAULT_INITIAL_SIZE = 16;

    private final Class<T> objClass;

    private int index;
    private T[] pool;
    private int size;

    public ObjectPool()
    {
        this(DEFAULT_INITIAL_SIZE);
    }

    @SuppressWarnings("unchecked")
    public ObjectPool(final int initialSize)
    {
        this.objClass = (Class<T>) makeNew().getClass();
        this.index = 0;
        this.pool = (T[]) Array.newInstance(objClass, initialSize);
        this.size = pool.length;

        for (int i = 0; i < pool.length; ++i)
        {
            pool[i] = makeNew();
        }
    }

    public void reset()
    {
        index = 0;
    }

    public final T getFree()
    {
        if (index == size)
        {
            increaseSize();
        }

        return pool[index++];
    }

    @SuppressWarnings("unchecked")
    private void increaseSize()
    {
        final T[] oldPool = pool;
        pool = (T[]) Array.newInstance(objClass, oldPool.length * 2);
        size = pool.length;

        System.arraycopy(oldPool, 0, pool, 0, oldPool.length);

        for (int i = oldPool.length; i < pool.length; ++i)
        {
            pool[i] = makeNew();
        }
    }

    protected abstract T makeNew();
}
