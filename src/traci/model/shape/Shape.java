package traci.model.shape;

import traci.math.Transformable;
import traci.math.Vector;
import traci.model.material.Finish;
import traci.model.material.Material;
import traci.model.material.Texture;
import traci.model.material.pigment.Pigment;
import traci.render.Ray;

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
