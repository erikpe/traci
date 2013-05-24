package se.ejp.traci.model.material;

import se.ejp.traci.util.WeakCache;

public class Interior
{
    private static final WeakCache<Interior> cache = new WeakCache<Interior>();

    public static final Interior SURROUNDING_INTERIOR = make(1.0);

    public final double ior;

    private Interior(final double ior)
    {
        this.ior = ior;
    }

    public static Interior make(final Double ior)
    {
        return cache.get(new Interior(ior));
    }

    public static Interior getDefault()
    {
        return make(1.0);
    }

    @Override
    public int hashCode()
    {
        int hash = getClass().hashCode();
        hash = 31 * hash + Double.valueOf(ior).hashCode();
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
        else if (other.getClass() != getClass())
        {
            return false;
        }

        final Interior otherInterior = (Interior) other;
        return Double.valueOf(ior).equals(Double.valueOf(otherInterior.ior));
    }

    @Override
    public String toString()
    {
        return "Interior";
    }
}
