package traci.model.shape;

import traci.math.Transformable;
import traci.math.TransformableHelper;
import traci.math.Transformation;
import traci.math.Vector;
import traci.model.material.Material;
import traci.model.shape.primitive.Primitive;
import traci.render.IntersectionStack;
import traci.render.Ray;
import traci.render.Ray2;

public abstract class Shape extends TransformableHelper implements Transformable
{
    public static final double EPSILON = 0.000001;
    
    protected static final double INSIDE_MARIGIN = 1e-7;
    
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
    
    public abstract int numCsgs();
    
    public abstract int numPrimitives();
    
    @Override
    public void transform(final Transformation transformation)
    {
        material.getPigment().transform(transformation);
    }
    
    public abstract Ray2 shootRay2(final Vector p, final Vector dir);
    
    @Deprecated
    public abstract Ray shootRay(final Vector p, final Vector dir);
    
    public abstract boolean isInside(final Vector p, final Primitive primitive);
    
    public abstract boolean isOutside(final Vector p, final Primitive primitive);
    
    public abstract void allIntersections(final IntersectionStack iStack,
            final Vector p, final Vector dir);
    
    public boolean anyIntersection(final Vector p, final Vector dir)
    {
        final IntersectionStack localStack = IntersectionStack.make();
        allIntersections(localStack, p, dir);
        
        return !localStack.isEmpty();
    }
}
