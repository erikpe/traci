package se.ejp.traci.model.material.pigment;

import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.Color;
import se.ejp.traci.model.material.MaterialColor;
import se.ejp.traci.util.WeakCache;

public class Solid extends Pigment
{
    private static final WeakCache<Solid> cache = new WeakCache<Solid>();
    private final int hash;

    private final MaterialColor color;

    private Solid(final Color color)
    {
        this.color = MaterialColor.make(color);
        this.hash = calcHash();
    }

    public static Solid make(final Color color)
    {
        return cache.get(new Solid(color));
    }

    @Override
    public Color getColor(final Vector p)
    {
        return color.color;
    }

    @Override
    public Solid transform(final Transformation transformation)
    {
        return this;
    }

    @Override
    public int hashCode()
    {
        return hash;
    }

    private int calcHash()
    {
        int hash = getClass().hashCode();
        hash = 31 * hash + color.hashCode();
        return hash;
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
        else if (getClass() != other.getClass())
        {
            return false;
        }
        else if (hashCode() != other.hashCode())
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
