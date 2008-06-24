package traci.render;

import java.util.AbstractList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

public class Ray extends AbstractList<Interval> implements List<Interval>
{
    private final LinkedList<Interval> ray;
    
    public Ray()
    {
        ray = new LinkedList<Interval>();
        ray.listIterator();
    }
    
    public Ray(final Interval ival)
    {
        this();
        ray.add(ival);
    }
    
    public void merge(final Ray otherRay)
    {
        assert otherRay != null;
        
        final ListIterator<Interval> iter0 = ray.listIterator();
        final ListIterator<Interval> iter1 = otherRay.ray.listIterator();
        
        while (iter1.hasNext())
        {
            final Interval i1 = iter1.next();
            
            if (!iter0.hasNext())
            {
                iter0.add(i1);
                continue;
            }
            
            final Interval i0 = iter0.next();
            
            if (i0.p1.dist < i1.p0.dist)
            {
                iter1.previous();
            }
            else if (i0.p0.dist > i1.p1.dist)
            {
                iter0.previous();
                iter0.add(i1);
            }
            else
            {
                iter0.remove();
                iter0.add(new Interval(Point.nearest(i0.p0, i1.p0),
                                       Point.farest(i0.p1, i1.p1)));
            }
        }
    }
    
    public void subtract(final Ray otherRay)
    {
        assert otherRay != null;
        
        final ListIterator<Interval> iter0 = ray.listIterator();
        final ListIterator<Interval> iter1 = otherRay.ray.listIterator();
        
        while (iter0.hasNext() && iter1.hasNext())
        {
            final Interval i0 = iter0.next();
            final Interval i1 = iter1.next();
            
            if (i0.p1.dist < i1.p0.dist)
            {
                iter1.previous();
            }
            else if (i0.p0.dist > i1.p1.dist)
            {
                iter0.previous();
            }
            else if (i0.p0.dist < i1.p0.dist)
            {
                iter0.remove();
                
                if (i0.p1.dist < i1.p1.dist)
                {
                    iter0.add(new Interval(i0.p0, i1.p0.invNormal()));
                    iter1.previous();
                }
                else
                {
                    iter0.add(new Interval(i0.p0, i1.p0.invNormal()));
                    iter0.add(new Interval(i1.p1.invNormal(), i0.p1));
                    iter0.previous();
                }
            }
            else
            {
                iter0.remove();
                
                if (i1.p1.dist < i0.p1.dist)
                {
                    iter0.add(new Interval(i1.p1.invNormal(), i0.p1));
                    iter0.previous();
                }
                else
                {
                    iter1.previous();
                }
            }
        }
    }
    
    public void intersect(final Ray otherRay)
    {
        assert otherRay != null;
        
        final ListIterator<Interval> iter0 = ray.listIterator();
        final ListIterator<Interval> iter1 = otherRay.ray.listIterator();
        
        while (iter0.hasNext() && iter1.hasNext())
        {
            final Interval i0 = iter0.next();
            final Interval i1 = iter1.next();
            
            if (i0.p1.dist < i1.p0.dist)
            {
                iter1.previous();
            }
            else if (i0.p0.dist > i1.p1.dist)
            {
                iter0.previous();
            }
            else if (i0.p0.dist < i1.p0.dist)
            {
                iter0.remove();
                
                if (i0.p1.dist < i1.p1.dist)
                {
                    iter0.add(new Interval(i1.p0, i0.p1));
                    iter1.previous();
                }
//                else
//                {
//                    iter0.add(new Interval())
//                }
            }
        }
    }
    
    @Override
    public Iterator<Interval> iterator()
    {
        return ray.iterator();
    }
    
    @Override
    public int size()
    {
        return ray.size();
    }
    
    @Override
    public Interval get(int index)
    {
        return ray.get(index);
    }
}
