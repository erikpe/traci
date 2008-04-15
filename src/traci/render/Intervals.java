package traci.render;

import java.util.AbstractList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

import traci.math.Matrix;

public class Intervals extends AbstractList<Ival> implements List<Ival>
{
    private final LinkedList<Ival> intervals;
    
    public Intervals()
    {
        intervals = new LinkedList<Ival>();
    }
    
    public Intervals(final Ival ival)
    {
        this();
        intervals.add(ival);
    }
    
    public boolean add(final Ival ival)
    {
        if (intervals.isEmpty() || ival.p1.dist < intervals.getFirst().p0.dist)
        {
            intervals.addFirst(ival);
            return true;
        }
        else if (ival.p0.dist > intervals.getLast().p1.dist)
        {
            intervals.addLast(ival);
            return true;
        }
        
        assert false;
        return false;
    }
    
    public void add(final Intervals other)
    {
        if (other == null)
        {
            return;
        }
        
        final ListIterator<Ival> iter0 = intervals.listIterator();
        final ListIterator<Ival> iter1 = other.intervals.listIterator();
        
        while (iter1.hasNext())
        {
            final Ival i1 = iter1.next();
            
            if (!iter0.hasNext())
            {
                iter0.add(i1);
                continue;
            }
            
            final Ival i0 = iter0.next();
            
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
                iter0.add(new Ival(Point.nearest(i0.p0, i1.p0),
                                   Point.farest(i0.p1, i1.p1)));
            }
        }
    }
    
    public void subtract(final Intervals other)
    {
        if (other == null)
        {
            return;
        }
        
        final ListIterator<Ival> iter0 = intervals.listIterator();
        final ListIterator<Ival> iter1 = other.intervals.listIterator();
        
        while (iter0.hasNext() && iter1.hasNext())
        {
            final Ival i0 = iter0.next();
            final Ival i1 = iter1.next();
            
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
                    iter0.add(new Ival(i0.p0, i1.p0.invNormal()));
                    iter1.previous();
                }
                else
                {
                    iter0.add(new Ival(i0.p0, i1.p0.invNormal()));
                    iter0.add(new Ival(i1.p1.invNormal(), i0.p1));
                    iter0.previous();
                }
            }
            else
            {
                iter0.remove();
                
                if (i1.p1.dist < i0.p1.dist)
                {
                    iter0.add(new Ival(i1.p1.invNormal(), i0.p1));
                    iter0.previous();
                }
                else
                {
                    iter1.previous();
                }
            }
        }
    }
    
    public void transformNormals(final Matrix mat)
    {
        for (final Ival ival : this)
        {
            ival.p0.transformNormal(mat);
            ival.p1.transformNormal(mat);
        }
    }
    
    public void intersect(final Intervals other)
    {
    }
    
    @Override
    public Iterator<Ival> iterator()
    {
        return intervals.iterator();
    }
    
    @Override
    public int size()
    {
        return intervals.size();
    }
    
    @Override
    public Ival get(int index)
    {
        return intervals.get(index);
    }
}
