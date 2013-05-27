package se.ejp.traci.model.material.pigment;

import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.material.Color;
import se.ejp.traci.model.material.MaterialColor;
import se.ejp.traci.util.WeakCache;

public class Checker extends Pattern
{
    private static WeakCache<Checker> cache = new WeakCache<Checker>();
    private final int hash;

    private final MaterialColor color1;
    private final MaterialColor color2;

    private Checker(final Color color1, final Color color2, final Transformation transformation)
    {
        super(transformation);
        this.color1 = MaterialColor.make(color1);
        this.color2 = MaterialColor.make(color2);
        this.hash = calcHash();
    }

    public static Checker make(final Color color1, final Color color2)
    {
        return cache.get(new Checker(color1, color2, Transformations.identity()));
    }

    @Override
    public Checker transform(final Transformation newTr)
    {
        return cache.get(new Checker(color1.getColor(), color2.getColor(), transformation.compose(newTr)));
    }

    @Override
    public Color getColorTransformed(final Vector p)
    {
        if ((Math.round(p.x()) + Math.round(p.y()) + Math.round(p.z())) % 2 == 0)
        {
            return color1.getColor();
        }

        return color2.getColor();
    }

    public Color getColor1()
    {
        return color1.getColor();
    }

    public Color getColor2()
    {
        return color2.getColor();
    }

    @Override
    public int hashCode()
    {
        return hash;
    }

    private int calcHash()
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
        else if (getClass() != other.getClass())
        {
            return false;
        }
        else if (hashCode() != other.hashCode())
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
