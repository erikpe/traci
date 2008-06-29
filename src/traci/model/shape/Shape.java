package traci.model.shape;

import traci.math.Matrix;
import traci.math.Transformable;
import traci.math.Vector;
import traci.model.material.Material;
import traci.model.shape.csg.Csg;
import traci.model.shape.primitive.Primitive;
import traci.render.Ray;

public abstract class Shape extends Transformable
{
    protected static final double EPSILON = 0.000001;
    
    public final Material material;
    
    public static boolean isShape(final String str)
    {
        return Csg.isCsg(str) || Primitive.isPrimitive(str);
    }
    
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
    public void transform(final Matrix mat, final Matrix invMat)
    {
        material.getPigment().transform(mat, invMat);
    }
    
    public abstract Ray shootRay(final Vector p, final Vector dir);
}
