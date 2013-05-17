package se.ejp.traci.model.material;

import se.ejp.traci.math.Transformation;
import se.ejp.traci.model.material.pigment.Pigment;
import se.ejp.traci.util.WeakCache;

public class Material
{
    private static final WeakCache<Material> cache = new WeakCache<Material>();

    public final Texture texture;

    private Material(final Texture texture)
    {
        this.texture = texture;
    }

    public static Material make(final Texture texture)
    {
        return cache.get(new Material(texture));
    }

    public static Material getDefault()
    {
        return make(Texture.getDefault());
    }

    public Material transform(final Transformation transformation)
    {
        return Material.make(texture.transform(transformation));
    }

    public Material setTexture(final Texture newTexture)
    {
        return Material.make(newTexture);
    }

    public Material setPigment(final Pigment newPigment)
    {
        return Material.make(texture.setPigment(newPigment));
    }

    public Material setFinish(final Finish newFinish)
    {
        return Material.make(texture.setFinish(newFinish));
    }

    @Override
    public int hashCode()
    {
        int hash = getClass().hashCode();
        hash = 31 * hash + texture.hashCode();
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

        final Material otherMaterial = (Material) other;
        return texture.equals(otherMaterial.texture);
    }

    @Override
    public String toString()
    {
        return "Material";
    }
}
