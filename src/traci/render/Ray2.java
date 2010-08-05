package traci.render;

import java.util.ArrayList;

import traci.math.ObjectPool;
import traci.render.Point2.Type;

public class Ray2 extends ArrayList<Point2>
{
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
            ray.clear();
            return ray;
        }
    }
    
    private Ray2() { }
    
    public static Ray2 make()
    {
        return ((RenderingThread) Thread.currentThread()).ray2Pool.make();
    }
    
    private static boolean checkRay(final Ray2 ray)
    {
        if (ray == null)
        {
            return true;
        }
        
        if (ray.get(0).type() == Type.LEAVE ||
                ray.get(ray.size() - 1).type() == Type.ENTER)
        {
            return false;
        }
        
        for (int i = 1; i < ray.size(); ++i)
        {
            final Point2 pPrev = ray.get(i - 1);
            final Point2 pThis = ray.get(i);
            
            if (pPrev.dist() >= pThis.dist())
            {
                return false;
            }
            
            switch (pPrev.type())
            {
            case ENTER:
                if (pThis.type() != Type.LEAVE)
                {
                    return false;
                }
                break;
                
            case LEAVE:
            case INTERSECT:
                if (pThis.type() == Type.LEAVE)
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
        if (size() == 0)
        {
            throw new IllegalStateException();
        }
        
        return get(0).dist();
    }
    
    private double farest()
    {
        final int size = size();
        
        if (size == 0)
        {
            throw new IllegalStateException();
        }
        
        return get(size - 1).dist();
    }
    
    @Override
    public boolean add(final Point2 p)
    {
        final int size = size();
        
        if (size == 0)
        {
            assert p.type() == Type.ENTER || p.type() == Type.INTERSECT;
            return super.add(p);
        }
        
        final int idxLast = size - 1;
        final Point2 pLast = get(idxLast);
        
        if (p.dist() == pLast.dist())
        {
            if (p.type() == Type.INTERSECT)
            {
                return false;
            }
            else if (pLast.type() == Type.INTERSECT)
            {
                set(idxLast, p);
                return true;
            }
            
            remove(idxLast);
            return true;
        }
        
        assert p.dist() > pLast.dist();
        
        switch (p.type())
        {
        case ENTER: assert pLast.type() != Type.ENTER; break;
        case LEAVE: assert pLast.type() == Type.ENTER; break;
        case INTERSECT: assert pLast.type() != Type.ENTER; break;
        }
        
        return super.add(p);
    }
    
    public static Ray2 union(final Ray2 ray0, final Ray2 ray1)
    {
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
            ray0.addAll(ray1);
            
            assert checkRay(ray0);
            return ray0;
        }
        else if (ray1.farest() < ray0.nearest())
        {
            ray1.addAll(ray0);
            
            assert checkRay(ray1);
            return ray1;
        }
        
        final Ray2 newRay = new Ray2();
        
        int i0 = 0;
        int i1 = 0;
        int insideMask = 0;
        
        while (i0 < ray0.size() && i1 < ray1.size())
        {
            final Point2 p0 = ray0.get(i0);
            final Point2 p1 = ray1.get(i1);
            
            final Point2 pNear;
            final int pointMask;
            
            if (p0.dist() < p1.dist())
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
            
            switch (pNear.type())
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
        
        while (i0 < ray0.size())
        {
            newRay.add(ray0.get(i0++));
        }
        
        while (i1 < ray1.size())
        {
            newRay.add(ray1.get(i1++));
        }
        
        assert !newRay.isEmpty();
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
        
        final Ray2 newRay = new Ray2();
        
        int i0 = 0;
        int i1 = 0;
        int insideMask = 0;
        
        while (i0 < ray0.size() && i1 < ray1.size())
        {
            final Point2 p0 = ray0.get(i0);
            final Point2 p1 = ray1.get(i1);
            
            final Point2 pNear;
            final int pointMask;
            
            if (p0.dist() < p1.dist())
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
            
            switch (pNear.type())
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
        
        if (newRay.isEmpty())
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
        
        final Ray2 newRay = new Ray2();
        
        int i0 = 0;
        int i1 = 0;
        int insideMask = 0x02;
        
        while (i0 < ray0.size() && i1 < ray1.size())
        {
            final Point2 p0 = ray0.get(i0);
            final Point2 p1 = ray1.get(i1);
            
            if (p1.type() == Type.INTERSECT)
            {
                i1++;
                continue;
            }
            
            final Point2 pNear;
            final int pointMask;
            
            if (p0.dist() < p1.dist())
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
            
            switch (pNear.type())
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
        
        while (i0 < ray0.size())
        {
            newRay.add(ray0.get(i0++));
        }
        
        if (newRay.isEmpty())
        {
            return null;
        }
        
        assert checkRay(newRay);
        return newRay;
    }
}
