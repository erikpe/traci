package se.ejp.traci.model.material;

import se.ejp.traci.model.Color;
import se.ejp.traci.util.WeakCache;

public class MaterialColor
{
    private static final WeakCache<MaterialColor> cache = new WeakCache<MaterialColor>();
    private final int hash;

    public final Color color;

    private MaterialColor(final Color color)
    {
        this.color = color;
        hash = calcHash();
    }

    public static MaterialColor make(final Color color)
    {
        return cache.get(new MaterialColor(color));
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
