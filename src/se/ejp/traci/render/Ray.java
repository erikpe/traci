package se.ejp.traci.render;

import se.ejp.traci.model.shape.Shape;
import se.ejp.traci.model.shape.primitive.Primitive;
import se.ejp.traci.render.Point.Type;

public class Ray
{
    private static final int INITIAL_SIZE = 32;

    private Point[] points;
    private int size;
    private int maxSize;

    private Ray()
    {
        points = new Point[INITIAL_SIZE];
        size = 0;
        maxSize = points.length;
    }

    public static Ray make()
    {
        return new Ray();
    }

    private void increaseSize()
    {
        final Point[] oldPoints = points;
        points = new Point[oldPoints.length * 2];
        System.arraycopy(oldPoints, 0, points, 0, oldPoints.length);
        maxSize = points.length;
    }

    public Point first()
    {
        assert size > 0;

        for (int i = 0; i < size; ++i)
        {
            final Point p = points[i];

            if (p.dist > Shape.EPSILON)
            {
                return p;
            }
        }

        return null;
    }

    public static boolean checkRay(final Ray ray)
    {
        if (ray == null)
        {
            return true;
        }
        else if (ray.size == 0)
        {
            return false;
        }
        else if (ray.points[0].type == Type.LEAVE || ray.points[ray.size - 1].type == Type.ENTER)
        {
            return false;
        }

        for (int i = 1; i < ray.size; ++i)
        {
            final Point pPrev = ray.points[i - 1];
            final Point pThis = ray.points[i];

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
        if (size == maxSize)
        {
            increaseSize();
        }

        points[size++] = Point.make(dist, obj, type);
    }

    private void add(final Point p)
    {
        if (size == 0)
        {
            assert p.type == Type.ENTER || p.type == Type.INTERSECT;
            points[0] = p;
            size = 1;
            return;
        }

        final Point pLast = points[size - 1];

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

        if (size == maxSize)
        {
            increaseSize();
        }

        points[size++] = p;
    }

    public static Ray union(final Ray ray0, final Ray ray1)
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
            while (ray0.maxSize < ray0.size + ray1.size)
            {
                ray0.increaseSize();
            }

            System.arraycopy(ray1.points, 0, ray0.points, ray0.size, ray1.size);
            ray0.size += ray1.size;

            assert checkRay(ray0);
            return ray0;
        }
        else if (ray1.farest() < ray0.nearest())
        {
            while (ray1.maxSize < ray0.size + ray1.size)
            {
                ray1.increaseSize();
            }

            System.arraycopy(ray0.points, 0, ray1.points, ray1.size, ray0.size);
            ray1.size += ray0.size;

            assert checkRay(ray1);
            return ray1;
        }

        final Ray newRay = Ray.make();
        assert newRay.size == 0;

        int i0 = 0;
        int i1 = 0;
        int insideMask = 0;

        while (i0 < ray0.size && i1 < ray1.size)
        {
            final Point p0 = ray0.points[i0];
            final Point p1 = ray1.points[i1];

            final Point pNear;
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

    public static Ray intersect(final Ray ray0, final Ray ray1)
    {
        if (ray0 == null || ray1 == null || ray0.farest() < ray1.nearest() || ray1.farest() < ray0.nearest())
        {
            return null;
        }

        final Ray newRay = Ray.make();

        int i0 = 0;
        int i1 = 0;
        int insideMask = 0;

        while (i0 < ray0.size && i1 < ray1.size)
        {
            final Point p0 = ray0.points[i0];
            final Point p1 = ray1.points[i1];

            final Point pNear;
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

        if (newRay.size == 0)
        {
            return null;
        }

        assert checkRay(newRay);
        return newRay;
    }

    public static Ray difference(final Ray ray0, final Ray ray1)
    {
        if (ray0 == null)
        {
            return null;
        }
        else if (ray1 == null || ray0.farest() < ray1.nearest() || ray1.farest() < ray0.nearest())
        {
            return ray0;
        }

        final Ray newRay = Ray.make();

        int i0 = 0;
        int i1 = 0;
        int insideMask = 0x02;

        while (i0 < ray0.size && i1 < ray1.size)
        {
            final Point p0 = ray0.points[i0];
            final Point p1 = ray1.points[i1];

            if (p1.type == Type.INTERSECT)
            {
                i1++;
                continue;
            }

            final Point pNear;
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
