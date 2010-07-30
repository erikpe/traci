package traci.model.shape;

import traci.math.Transformable;
import traci.math.TransformableHelper;
import traci.math.Transformation;
import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Ray;

public abstract class Shape extends TransformableHelper implements Transformable
{
    protected static final double EPSILON = 0.000001;
    
    public final Material material;
    
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
    
    public abstract Ray shootRay(final Vector p, final Vector dir);
    
    public abstract boolean isInside(final Vector p);
}
