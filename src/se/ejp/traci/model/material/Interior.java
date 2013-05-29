package se.ejp.traci.model.material;

import se.ejp.traci.model.Color;
import se.ejp.traci.util.WeakCache;

public class Interior
{
    private static final WeakCache<Interior> cache = new WeakCache<Interior>();
    private final int hash;

    public static final Interior SURROUNDING_INTERIOR = getDefault();

    public final double ior;
    public final double falloff;
    private final MaterialColor color;

    private Interior(final double ior, final Color color, final double falloff)
    {
        this.ior = ior;
        this.falloff = falloff;
        this.color = MaterialColor.make(color);
        this.hash = calcHash();
    }

    public static Interior make(final Double ior, final Color color, final Double falloff)
    {
        return cache.get(new Interior(ior, color, falloff));
    }

    public static Interior make(final Double ior)
    {
        return make(ior, Color.BLACK, 0.0);
    }

    public static Interior getDefault()
    {
        return make(1.0, Color.BLACK, 0.0);
    }

    @Override
    public int hashCode()
    {
        return hash;
    }

    private int calcHash()
    {
        int hash = getClass().hashCode();
        hash = 31 * hash + Double.valueOf(ior).hashCode();
        hash = 31 * hash + Double.valueOf(falloff).hashCode();
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

        final Interior otherInterior = (Interior) other;

        return Double.valueOf(ior).equals(Double.valueOf(otherInterior.ior)) &&
               Double.valueOf(falloff).equals(Double.valueOf(otherInterior.falloff)) &&
               color.equals(otherInterior.color);
    }

    @Override
    public String toString()
    {
        return "Interior";
    }
}
