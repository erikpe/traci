package se.ejp.traci.model.material.pigment;

import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.material.Color;
import se.ejp.traci.util.WeakCache;

public class Checker extends Pattern
{
    private static WeakCache<Checker> cache = new WeakCache<Checker>();

    public final Color color1;
    public final Color color2;

    private Checker(final Color color1, final Color color2, final Transformation transformation)
    {
        super(transformation);
        this.color1 = color1;
        this.color2 = color2;
    }

    public static Checker make(final Color color1, final Color color2)
    {
        return cache.get(new Checker(color1, color2, Transformations.identity()));
    }

    @Override
    public Checker transform(final Transformation newTr)
    {
        return cache.get(new Checker(color1, color2, transformation.compose(newTr)));
    }

    @Override
    public Color getColorTransformed(final Vector p)
    {
        if ((Math.round(p.x()) + Math.round(p.y()) + Math.round(p.z())) % 2 == 0)
        {
            return color1;
        }

        return color2;
    }

    @Override
    public int hashCode()
    {
        int hash = getClass().hashCode();
        hash = 31 * hash + transformation.hashCode();
        hash = 31 * hash + color1.hashCode();
        hash = 31 * hash + color2.hashCode();
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

        final Checker otherChecker = (Checker) other;

        return transformation.equals(otherChecker.transformation) &&
               color1.equals(otherChecker.color1) &&
               color2.equals(otherChecker.color2);
    }

    @Override
    public String toString()
    {
        return "checker";
    }
}
