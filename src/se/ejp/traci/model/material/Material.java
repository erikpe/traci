package se.ejp.traci.model.material;

import se.ejp.traci.math.Transformation;
import se.ejp.traci.model.material.pigment.Pigment;
import se.ejp.traci.util.WeakCache;

public class Material
{
    private static final WeakCache<Material> cache = new WeakCache<Material>();
    private final int hash;

    public final Texture texture;
    public final Interior interior;

    private Material(final Texture texture, final Interior interior)
    {
        this.texture = texture;
        this.interior = interior;
        this.hash = calcHash();
    }

    public static Material make(final Texture texture, final Interior interior)
    {
        return cache.get(new Material(texture, interior));
    }

    public static Material getDefault()
    {
        return make(Texture.getDefault(), Interior.getDefault());
    }

    public Material transform(final Transformation transformation)
    {
        return Material.make(texture.transform(transformation), interior);
    }

    public Material setTexture(final Texture newTexture)
    {
        return Material.make(newTexture, interior);
    }

    public Material setPigment(final Pigment newPigment)
    {
        return Material.make(texture.setPigment(newPigment), interior);
    }

    public Material setFinish(final Finish newFinish)
    {
        return Material.make(texture.setFinish(newFinish), interior);
    }

    public Material setInterior(final Interior newInterior)
    {
        return Material.make(texture, newInterior);
    }

    @Override
    public int hashCode()
    {
        return hash;
    }

    private int calcHash()
    {
        int hash = getClass().hashCode();
        hash = 31 * hash + texture.hashCode();
        hash = 31 * hash + interior.hashCode();
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

        final Material otherMaterial = (Material) other;
        return texture.equals(otherMaterial.texture) && interior.equals(otherMaterial.interior);
    }

    @Override
    public String toString()
    {
        return "Material";
    }
}
