package traci.model.material;

import traci.util.WeakCache;

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
        return Double.valueOf(specCoeff).hashCode() ^
               Double.valueOf(diffCoeff).hashCode() ^
               Double.valueOf(shininess).hashCode() ^
               Double.valueOf(reflectiveness).hashCode();
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
        
        return specCoeff == otherFinish.specCoeff &&
               diffCoeff == otherFinish.diffCoeff &&
               shininess == otherFinish.shininess &&
               reflectiveness == otherFinish.reflectiveness;
    }
}
