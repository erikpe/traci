package se.ejp.traci.model.material;

import se.ejp.traci.model.Color;
import se.ejp.traci.util.WeakCache;

public class Interior
{
    private static final WeakCache<Interior> cache = new WeakCache<Interior>();
    private final int hash;

    public static final Interior OPAQUE = cache.get(new Interior(1.0, Color.BLACK, 1.0));

    public final double ior;
    private final MaterialColor color;
    public final double falloff;

    private final Double rExp, gExp, bExp;

    private Interior(final double ior, final Color color, final double falloff)
    {
        this.ior = ior;
        this.color = MaterialColor.make(color);
        this.falloff = falloff;

        this.rExp = calcExp(color.r, falloff);
        this.gExp = calcExp(color.g, falloff);
        this.bExp = calcExp(color.b, falloff);

        this.hash = calcHash();
    }

    public static Interior make(final Double ior, final Color color, final Double falloff)
    {
        return cache.get(new Interior(ior, color, falloff));
    }

    public static Interior make(final Double ior)
    {
        return cache.get(new Interior(ior, Color.WHITE, 1.0));
    }

    public static Interior make()
    {
        return OPAQUE;
    }

    public static Interior getDefault()
    {
        return OPAQUE;
    }

    private static Double calcExp(final double val, final double falloff)
    {
        if (Double.valueOf(0.0).equals(Double.valueOf(val)))
        {
            return null;
        }

        return Math.log(val) / falloff;
    }

    public Color filterThrough(final Color color, final double length)
    {
        if (this == OPAQUE)
        {
            throw new UnsupportedOperationException("Don't do this on something opaque.");
        }

        final double r = (rExp == null ? 0.0 : color.r * Math.exp(rExp * length));
        final double g = (gExp == null ? 0.0 : color.g * Math.exp(gExp * length));
        final double b = (bExp == null ? 0.0 : color.b * Math.exp(bExp * length));

        return Color.make(r, g, b);
    }

    @Override
    public int hashCode()
    {
        return hash;
    }

    private int calcHash()
    {
        int hash = getClass().hashCode() | 0x0000001;
        hash = 31 * hash + Double.valueOf(ior).hashCode();
        hash = 31 * hash + color.hashCode();
        hash = 31 * hash + Double.valueOf(falloff).hashCode();
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
               color.equals(otherInterior.color) &&
               Double.valueOf(falloff).equals(Double.valueOf(otherInterior.falloff));
    }

    @Override
    public String toString()
    {
        return "Interior";
    }
}
