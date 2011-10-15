package traci.math;

public class Vector
{
    public static final class VectorPool extends ObjectPool<Vector>
    {
        @Override
        protected final Vector makeNew()
        {
            return new Vector(0, 0, 0);
        }
        
        private final Vector make(final double x, final double y, final double z)
        {
            final Vector vec = getFree();
            
            vec.x = x;
            vec.y = y;
            vec.z = z;
            
            return vec;
        }
    }
    
    private double x, y, z;
    
    public static final Vector ORIGO = new Vector(0, 0, 0);
    
    public static final Vector UNIT_X = new Vector(1, 0, 0);
    public static final Vector UNIT_Y = new Vector(0, 1, 0);
    public static final Vector UNIT_Z = new Vector(0, 0, 1);
    
    public static final Vector UNIT_NEG_X = new Vector(-1, 0, 0);
    public static final Vector UNIT_NEG_Y = new Vector(0, -1, 0);
    public static final Vector UNIT_NEG_Z = new Vector(0, 0, -1);
    
    private static class ThreadLocalVectorPool extends ThreadLocal<VectorPool>
    {
        @Override
        public VectorPool initialValue()
        {
            return new VectorPool();
        }
    }
    
    private static final ThreadLocalVectorPool vectorPool = new ThreadLocalVectorPool();
    
    public static void reset()
    {
        vectorPool.get().reset();
    }
    
    private Vector(final double x, final double y, final double z)
    {
        this.x = x;
        this.y = y;
        this.z = z;
    }
    
    //public static Map<String, Long> locMap = new HashMap<String, Long>();
    
    public static Vector make(final double x, final double y, final double z)
    {
//        return vectorPool.get().make(x, y, z);
//        
//        final Thread thisThread = Thread.currentThread();
//        
//        if (thisThread instanceof RenderingThread)
//        {
//            return ((RenderingThread) thisThread).vectorPool.make(x, y, z);
//        }
        
        return new Vector(x, y, z);
    }
    
    public double x()
    {
        return x;
    }
    
    public double y()
    {
        return y;
    }
    
    public double z()
    {
        return z;
    }
    
    public double length()
    {
        return Math.sqrt(x * x + y * y + z * z);
    }
    
    public Vector normalize()
    {
        return div(length());
    }
    
    public double dot(final Vector vec)
    {
        return x * vec.x + y * vec.y + z * vec.z;
    }
    
    public Vector add(final Vector vec)
    {
        return make(x + vec.x, y + vec.y, z + vec.z);
    }
    
    public Vector sub(final Vector vec)
    {
        return make(x - vec.x, y - vec.y, z - vec.z);
    }
    
    public Vector neg()
    {
        return make(-x, -y, -z);
    }
    
    public Vector mul(final double val)
    {
        return make(val * x, val * y, val * z);
    }
    
    public Vector div(final double val)
    {
        return make(x / val, y / val, z / val);
    }
    
    public Vector cross(final Vector vec)
    {
        return make(y * vec.z - z * vec.y, z * vec.x - x * vec.z, x * vec.y - y
                * vec.x);
    }
    
    @Override
    public String toString()
    {
        return "<" + x + ", " + y + ", " + z + ">";
    }
}
