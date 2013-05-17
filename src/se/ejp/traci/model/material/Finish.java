package se.ejp.traci.model.material;

import se.ejp.traci.util.WeakCache;

public class Finish
{
    private static final WeakCache<Finish> cache = new WeakCache<Finish>();

    public final double specCoeff;
    public final double diffCoeff;
    public final double shininess;
    public final double reflectiveness;

    private Finish(final double specCoeff, final double diffCoeff, final double shininess, final double reflectiveness)
    {
        this.specCoeff = specCoeff;
        this.diffCoeff = diffCoeff;
        this.shininess = shininess;
        this.reflectiveness = reflectiveness;
    }

    public static Finish make(final double specCoeff, final double diffCoeff, final double shininess,
            final double reflectiveness)
    {
        return cache.get(new Finish(specCoeff, diffCoeff, shininess, reflectiveness));
    }

    public static Finish getDefault()
    {
        return make(0.3, 0.3, 50, 0.1);
    }

    @Override
    public int hashCode()
    {
        int hash = getClass().hashCode();
        hash = 31 * hash + Double.valueOf(specCoeff).hashCode();
        hash = 31 * hash + Double.valueOf(diffCoeff).hashCode();
        hash = 31 * hash + Double.valueOf(shininess).hashCode();
        hash = 31 * hash + Double.valueOf(reflectiveness).hashCode();
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

        final Finish otherFinish = (Finish) other;

        return Double.valueOf(specCoeff).equals(Double.valueOf(otherFinish.specCoeff)) &&
               Double.valueOf(diffCoeff).equals(Double.valueOf(otherFinish.diffCoeff)) &&
               Double.valueOf(shininess).equals(Double.valueOf(otherFinish.shininess)) &&
               Double.valueOf(reflectiveness).equals(Double.valueOf(otherFinish.reflectiveness));
    }

    @Override
    public String toString()
    {
        return "[" + specCoeff + "," + diffCoeff + "," + shininess + "," + reflectiveness + "]";
    }
}
