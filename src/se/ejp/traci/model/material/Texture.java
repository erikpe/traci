package se.ejp.traci.model.material;

import se.ejp.traci.math.Transformation;
import se.ejp.traci.model.material.pigment.Pigment;
import se.ejp.traci.util.WeakCache;

public class Texture
{
    private static WeakCache<Texture> cache = new WeakCache<Texture>();
    private final int hash;

    public final Pigment pigment;
    public final Finish finish;

    private Texture(final Pigment pigment, final Finish finish)
    {
        this.pigment = pigment;
        this.finish = finish;
        this.hash = calcHash();
    }

    public static Texture make(final Pigment pigment, final Finish finish)
    {
        return cache.get(new Texture(pigment, finish));
    }

    public static Texture make()
    {
        return getDefault();
    }

    public static Texture getDefault()
    {
        return make(Pigment.getDefault(), Finish.getDefault());
    }

    public Texture transform(final Transformation transformation)
    {
        return make(pigment.transform(transformation), finish);
    }

    public Texture setPigment(final Pigment newPigment)
    {
        return make(newPigment, finish);
    }

    public Texture setFinish(final Finish newFinish)
    {
        return make(pigment, newFinish);
    }

    @Override
    public int hashCode()
    {
        return hash;
    }

    private int calcHash()
    {
        int hash = getClass().hashCode() | 0x0000001;
        hash = 31 * hash + pigment.hashCode();
        hash = 31 * hash + finish.hashCode();
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

        final Texture otherTexture = (Texture) other;
        return pigment.equals(otherTexture.pigment) && finish.equals(otherTexture.finish);
    }

    @Override
    public String toString()
    {
        return "Texture";
    }
}
