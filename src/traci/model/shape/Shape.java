package traci.model.shape;

import traci.math.Transformable;
import traci.math.Transformation;
import traci.math.Vector;
import traci.model.material.Color;
import traci.model.material.Material;
import traci.render.Ray2;

public abstract class Shape implements Transformable, Cloneable
{
    public static final double EPSILON = 0.000001;
    
    public Material material;
    
    public Shape(final Material material)
    {
        if (material == null)
        {
            this.material = Material.newDefault();
        }
        else
        {
            this.material = material;
        }
    }
    
    @Override
    public void transform(final Transformation transformation)
    {
        material.getPigment().transform(transformation);
    }
    
    public abstract Ray2 shootRay2(final Vector p, final Vector dir);
    
    public abstract void setColor(final Color color);
    
    @Override
    public Object clone()
    {
        try
        {
            final Shape res = (Shape) super.clone();
            res.material = (Material) material.clone();
            
            return res;
        }
        catch (final CloneNotSupportedException e)
        {
            e.printStackTrace();
            return null;
        }
    }
}
