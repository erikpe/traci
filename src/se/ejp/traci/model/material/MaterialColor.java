package se.ejp.traci.model.material;

import se.ejp.traci.util.WeakCache;

public class MaterialColor
{
    private static final WeakCache<MaterialColor> cache = new WeakCache<MaterialColor>();
    private final int hash;

    public static final MaterialColor BLACK = make(0.0, 0.0, 0.0);
    public static final MaterialColor WHITE = make(1.0, 1.0, 1.0);

    private final Color color;

    public MaterialColor(final Color color)
    {
        this.color = color;
        hash = calcHash();
    }

    public static MaterialColor make(final Color color)
    {
        return cache.get(new MaterialColor(color));
    }

    public static MaterialColor make(final Double r, final Double g, final Double b, final Double transmit)
    {
        return make(Color.make(r, g, b, transmit));
    }

    public static MaterialColor make(final Double r, final Double g, final Double b)
    {
        return make(Color.make(r, g, b));
    }

    public Color getColor()
    {
        return color;
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

        final MaterialColor otherMaterialColor = (MaterialColor) other;

        return color.equals(otherMaterialColor.color);
    }

}
