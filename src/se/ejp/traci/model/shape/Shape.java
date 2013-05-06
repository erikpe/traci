package se.ejp.traci.model.shape;

import se.ejp.traci.math.Transformable;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.material.Finish;
import se.ejp.traci.model.material.Material;
import se.ejp.traci.model.material.Texture;
import se.ejp.traci.model.material.pigment.Pigment;
import se.ejp.traci.render.Ray;

public abstract class Shape implements Transformable, Cloneable
{
    public static final double EPSILON = 0.000001;

    public abstract Ray shootRay(final Vector p, final Vector dir);
    public abstract void setMaterial(final Material material);
    public abstract void setTexture(final Texture texture);
    public abstract void setPigment(final Pigment pigment);
    public abstract void setFinish(final Finish finish);
    public abstract Shape optimize();

    @Override
    public Object clone()
    {
        try
        {
            return super.clone();
        }
        catch (final CloneNotSupportedException e)
        {
            e.printStackTrace();
            return null;
        }
    }
}
