package traci.render;

import traci.math.ObjectPool;
import traci.model.shape.Shape;
import traci.model.shape.primitive.Primitive;
import traci.render.Point2.Type;

public class Ray2
{
    private static final int INITIAL_SIZE = 128;
    
    public Point2[] points;
    public int size;
    
    public static final class Ray2Pool extends ObjectPool<Ray2>
    {
        @Override
        protected final Ray2 makeNew()
        {
            return new Ray2();
        }
        
        private final Ray2 make()
        {
            final Ray2 ray = getFree();
            ray.size = 0;
            return ray;
        }
    }
    
    private Ray2()
    {
        points = new Point2[INITIAL_SIZE];
        size = 0;
    }
    
    public Point2 first()
    {
        assert size > 0;
        
        for (int i = 0; i < size; ++i)
        {
            final Point2 p = points[i];
            
            if (p.dist > Shape.EPSILON)
            {
                return p;
            }
        }
        
        return null;
    }
    
    public static Ray2 make()
    {
        final Thread thisThread = Thread.currentThread();
        
        if (thisThread instanceof RenderingThread)
        {
            return ((RenderingThread) thisThread).ray2Pool.make();
        }
        
        return new Ray2();
    }
    
    public static boolean checkRay(final Ray2 ray)
    {
        if (ray == null)
        {
            return true;
        }
        
        if (ray.size == 0)
        {
            return false;
        }
        
        if (ray.points[0].type == Type.LEAVE ||
                ray.points[ray.size - 1].type == Type.ENTER)
        {
            return false;
        }
        
        for (int i = 1; i < ray.size; ++i)
        {
            final Point2 pPrev = ray.points[i - 1];
            final Point2 pThis = ray.points[i];
            
            if (pPrev.dist >= pThis.dist)
            {
                return false;
            }
            
            switch (pPrev.type)
            {
            case ENTER:
                if (pThis.type != Type.LEAVE)
                {
                    return false;
                }
                break;
                
            case LEAVE:
            case INTERSECT:
                if (pThis.type == Type.LEAVE)
                {
                    return false;
                }
                break;
            }
        }
        
        return true;
    }
    
    private double nearest()
    {
        assert size > 0;
        return points[0].dist;
    }
    
    private double farest()
    {
        assert size > 0;
        return points[size - 1].dist;
    }
    
    public void add(final double dist, final Primitive obj, final Type type)
    {
        points[size++] = Point2.make(dist, obj, type);
    }
    
    private void add(final Point2 p)
    {
        if (size == 0)
        {
            assert p.type == Type.ENTER || p.type == Type.INTERSECT;
            points[0] = p;
            size = 1;
            return;
        }
        
        final Point2 pLast = points[size - 1];
        
        if (p.dist == pLast.dist)
        {
            if (p.type == Type.INTERSECT)
            {
                return;
            }
            else if (pLast.type == Type.INTERSECT)
            {
                points[size - 1] = p;
                return;
            }
            
            size--;
            return;
        }
        
        assert p.dist > pLast.dist;
        
        switch (p.type)
        {
        case ENTER: assert pLast.type != Type.ENTER; break;
        case LEAVE: assert pLast.type == Type.ENTER; break;
        case INTERSECT: assert pLast.type != Type.ENTER; break;
        }
        
        points[size++] = p;
    }
    
    public static Ray2 union(final Ray2 ray0, final Ray2 ray1)
    {
        assert checkRay(ray0);
        assert checkRay(ray1);
        
        if (ray0 == null)
        {
            return ray1;
        }
        else if (ray1 == null)
        {
            return ray0;
        }
        else if (ray0.farest() < ray1.nearest())
        {
            System.arraycopy(ray1.points, 0, ray0.points, ray0.size, ray1.size);
            ray0.size += ray1.size;
            
            assert checkRay(ray0);
            return ray0;
        }
        else if (ray1.farest() < ray0.nearest())
        {
            System.arraycopy(ray0.points, 0, ray1.points, ray1.size, ray0.size);
            ray1.size += ray0.size;
            
            assert checkRay(ray1);
            return ray1;
        }
        
        final Ray2 newRay = Ray2.make();
        assert newRay.size == 0;
        
        int i0 = 0;
        int i1 = 0;
        int insideMask = 0;
        
        while (i0 < ray0.size && i1 < ray1.size)
        {
            final Point2 p0 = ray0.points[i0];
            final Point2 p1 = ray1.points[i1];
            
            final Point2 pNear;
            final int pointMask;
            
            if (p0.dist < p1.dist)
            {
                pNear = p0;
                pointMask = 0x01;
                i0++;
            }
            else
            {
                pNear = p1;
                pointMask = 0x02;
                i1++;
            }
            
            switch (pNear.type)
            {
            case ENTER:
                assert (insideMask & pointMask) == 0;
                if (insideMask == 0)
                {
                    newRay.add(pNear);
                }
                insideMask |= pointMask;
                break;
                
            case LEAVE:
                assert (insideMask & pointMask) != 0;
                insideMask &= ~pointMask;
                if (insideMask == 0)
                {
                    newRay.add(pNear);
                }
                break;
                
            case INTERSECT:
                assert (insideMask & pointMask) == 0;
                if (insideMask == 0)
                {
                    newRay.add(pNear);
                }
                break;
            }
        }
        
        while (i0 < ray0.size)
        {
            newRay.add(ray0.points[i0++]);
        }
        
        while (i1 < ray1.size)
        {
            newRay.add(ray1.points[i1++]);
        }
        
        if (newRay.size == 0)
        {
            return null;
        }
        
        assert checkRay(newRay);
        return newRay;
    }
    
    public static Ray2 intersect(final Ray2 ray0, final Ray2 ray1)
    {
        if (ray0 == null ||
            ray1 == null ||
            ray0.farest() < ray1.nearest() ||
            ray1.farest() < ray0.nearest())
        {
            return null;
        }
        
        final Ray2 newRay = Ray2.make();
        
        int i0 = 0;
        int i1 = 0;
        int insideMask = 0;
        
        while (i0 < ray0.size && i1 < ray1.size)
        {
            final Point2 p0 = ray0.points[i0];
            final Point2 p1 = ray1.points[i1];
            
            final Point2 pNear;
            final int pointMask;
            
            if (p0.dist < p1.dist)
            {
                pNear = p0;
                pointMask = 0x01;
                i0++;
            }
            else
            {
                pNear = p1;
                pointMask = 0x02;
                i1++;
            }
            
            switch (pNear.type)
            {
            case ENTER:
                assert (insideMask & pointMask) == 0;
                insideMask |= pointMask;
                if (insideMask == 0x03)
                {
                    newRay.add(pNear);
                }
                break;
                
            case LEAVE:
                assert (insideMask & pointMask) != 0;
                if (insideMask != 0x03)
                {
                    newRay.add(pNear);
                }
                insideMask &= ~pointMask;
                break;
                
            case INTERSECT:
                assert (insideMask & pointMask) == 0;
                if ((insideMask |= pointMask) == 0x03)
                {
                    newRay.add(pNear);
                }
                break;
            }
        }
        
        if (newRay.size == 0)
        {
            return null;
        }
        
        assert checkRay(newRay);
        return newRay;
    }
    
    public static Ray2 difference(final Ray2 ray0, final Ray2 ray1)
    {
        if (ray0 == null)
        {
            return null;
        }
        else if (ray1 == null ||
                 ray0.farest() < ray1.nearest() ||
                 ray1.farest() < ray0.nearest())
        {
            return ray0;
        }
        
        final Ray2 newRay = Ray2.make();
        
        int i0 = 0;
        int i1 = 0;
        int insideMask = 0x02;
        
        while (i0 < ray0.size && i1 < ray1.size)
        {
            final Point2 p0 = ray0.points[i0];
            final Point2 p1 = ray1.points[i1];
            
            if (p1.type == Type.INTERSECT)
            {
                i1++;
                continue;
            }
            
            final Point2 pNear;
            final int pointMask;
            
            if (p0.dist < p1.dist)
            {
                pNear = p0;
                pointMask = 0x01;
                i0++;
            }
            else
            {
                pNear = p1.invert();
                pointMask = 0x02;
                i1++;
            }
            
            switch (pNear.type)
            {
            case ENTER:
                assert (insideMask & pointMask) == 0;
                insideMask |= pointMask;
                if (insideMask == 0x03)
                {
                    newRay.add(pNear);
                }
                break;
                
            case LEAVE:
                assert (insideMask & pointMask) != 0;
                if (insideMask == 0x03)
                {
                    newRay.add(pNear);
                }
                insideMask &= ~pointMask;
                break;
                
            case INTERSECT:
                assert (insideMask & pointMask) == 0;
                if ((insideMask |= pointMask) == 0x03)
                {
                    newRay.add(pNear);
                }
                break;
            }
        }
        
        while (i0 < ray0.size)
        {
            newRay.add(ray0.points[i0++]);
        }
        
        if (newRay.size == 0)
        {
            return null;
        }
        
        assert checkRay(newRay);
        return newRay;
    }
    
    @Override
    public String toString()
    {
        final StringBuilder sb = new StringBuilder();
        
        sb.append("[");
        
        for (int i = 0; i < size; ++i)
        {
            sb.append(points[i]);
        }
        
        sb.append("]");
        
        return sb.toString();
    }
}
