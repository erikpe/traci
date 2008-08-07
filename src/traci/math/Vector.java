package traci.math;

import traci.render.RenderingThread;

public class Vector
{
    public static class VectorPool extends ObjectPool<Vector>
    {
        @Override
        protected Vector makeNew()
        {
            return new Vector(0, 0, 0);
        }
        
        public Vector make(final double x, final double y, final double z)
        {
            final Vector vec = getFree();
            
            vec.x = x;
            vec.y = y;
            vec.z = z;
            
            return vec;
        }
    }
    
    public double x, y, z;
    
    public static final Vector ORIGO = new Vector(0, 0, 0);
    
    public static final Vector UNIT_X = new Vector(1, 0, 0);
    public static final Vector UNIT_Y = new Vector(0, 1, 0);
    public static final Vector UNIT_Z = new Vector(0, 0, 1);
    
    public static final Vector UNIT_NEG_X = new Vector(-1, 0, 0);
    public static final Vector UNIT_NEG_Y = new Vector(0, -1, 0);
    public static final Vector UNIT_NEG_Z = new Vector(0, 0, -1);
    
    private Vector(final double x, final double y, final double z)
    {
        this.x = x;
        this.y = y;
        this.z = z;
    }
    
    public static Vector make(final double x, final double y, final double z)
    {
        final Thread thisThread = Thread.currentThread();
        
        if (thisThread instanceof RenderingThread)
        {
            return ((RenderingThread) thisThread).vectorPool.make(x, y, z);
        }
        
        return new Vector(x, y, z);
    }
    
    public double length()
    {
        return Math.sqrt(x * x + y * y + z * z);
    }
    
    public Vector normalize()
    {
        final double len = length();
        return make(x / len, y / len, z / len);
    }
    
    public double dot(final Vector vec)
    {
        return (x * vec.x) + (y * vec.y) + (z * vec.z);
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
        return mul(1.0 / val);
    }
    
    public Vector cross(final Vector vec)
    {
        return make(y * vec.z - z * vec.y, z * vec.x - x * vec.z, x * vec.y - y
                * vec.x);
    }
    
    public double cosTheta(final Vector vec)
    {
        return dot(vec) / (length() * vec.length());
    }
    
    public double sinTheta(final Vector vec)
    {
        return cross(vec).div(length() * vec.length()).length();
    }
    
    @Override
    public String toString()
    {
        return "<" + x + ", " + y + ", " + z + ">";
    }
}
