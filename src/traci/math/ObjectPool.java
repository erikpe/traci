package traci.math;

import java.lang.reflect.Array;

import traci.render.Settings;

abstract public class ObjectPool<T>
{
    private static final int DEFAULT_INITIAL_SIZE = 128;
    
    private final Class<T> objClass;
    
    private int index;
    private T[] pool;
    
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
        
        for (int i = 0; i < pool.length; ++i)
        {
            pool[i] = makeNew();
        }
    }
    
    public void reset()
    {
        index = 0;
    }
    
    public T make()
    {
        if (index == pool.length)
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
        
        System.arraycopy(oldPool, 0, pool, 0, oldPool.length);
        
        for (int i = oldPool.length; i < pool.length; ++i)
        {
            pool[i] = makeNew();
        }
        
        if (Settings.DEBUG == true)
        {
            System.out.println(Thread.currentThread().getName()
                    + " increasing " + makeNew().getClass().getSimpleName()
                    + " pool size to " + pool.length);
        }
    }
    
    protected abstract T makeNew();
}
