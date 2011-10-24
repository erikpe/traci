package traci.model.material;

import traci.model.material.pigment.Pigment;
import traci.util.WeakCache;


public class Texture
{
    private static WeakCache<Texture> cache = new WeakCache<Texture>();
    
    public final Pigment pigment;
    public final Finish finish;
    
    private Texture(final Pigment pigment, final Finish finish)
    {
        this.pigment = pigment;
        this.finish = finish;
    }
    
    public static Texture make(final Pigment pigment, final Finish finish)
    {
        return cache.get(new Texture(pigment, finish));
    }
    
    public static Texture getDefault()
    {
        return make(Pigment.getDefault(), Finish.getDefault());
    }
    
    @Override
    public int hashCode()
    {
        return pigment.hashCode() ^ finish.hashCode();
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
        
        final Texture otherTexture = (Texture) other;
        
        return pigment.equals(otherTexture.pigment) && finish.equals(otherTexture.finish);
    }
}
