package traci.model.material.pigment;

import traci.math.Transformation;
import traci.math.Vector;
import traci.model.material.Color;
import traci.util.WeakCache;

public class Solid extends Pigment
{
    private static final WeakCache<Solid> cache = new WeakCache<Solid>();

    public final Color color;

    private Solid(final Color color)
    {
        this.color = color;
    }

    public static Solid make(final Color color)
    {
        return cache.get(new Solid(color));
    }

    @Override
    public Color getColor(final Vector p)
    {
        return color;
    }

    @Override
    public Solid transform(final Transformation transformation)
    {
        return this;
    }

    @Override
    public int hashCode()
    {
        return color.hashCode();
    }

    @Override
    public boolean equals(final Object other)
    {
        if (other == null)
        {
            return false;
        }
        else if (other == this)
        {
            return true;
        }
        else if (other.getClass() != getClass())
        {
            return false;
        }

        final Solid otherSolid = (Solid) other;
        return color.equals(otherSolid.color);
    }

    @Override
    public String toString()
    {
        return "Solid:" + color.toString();
    }
}
