package traci.model.shape.primitive;

import traci.math.Transformation;
import traci.math.Transformations;
import traci.math.Vector;
import traci.model.material.Material;
import traci.model.shape.Shape;
import traci.render.IntersectionStack;
import traci.render.Ray;

public abstract class Primitive extends Shape
{
    public Transformation transformation;
    
    public Primitive(final Material material)
    {
        super(material);
        transformation = Transformations.identity();
    }
    
    protected abstract Vector primitiveGetNormalAt(final Vector p);
    
    public Vector getNormalAt(final Vector p, final Vector dir)
    {
        Vector normal = primitiveGetNormalAt(transformation.pointInv(p));
        normal = transformation.normal(normal).normalize();
        
        if (dir.dot(normal) > 0)
        {
            normal = normal.neg();
        }
        
        return normal;
    }
    
    @Deprecated
    protected abstract Ray primitiveShootRay(final Vector p, final Vector dir);
    
    @Deprecated
    @Override
    public Ray shootRay(final Vector p, final Vector dir)
    {
        final Vector transP = transformation.pointInv(p);
        final Vector transDir = transformation.dirInv(dir);
        
        return primitiveShootRay(transP, transDir);
    }
    
    protected abstract boolean primitiveIsInside(final Vector p);
    
    @Override
    public boolean isInside(final Vector p, final Primitive primitive)
    {
        if (primitive == this)
        {
            return true;
        }
        
        final Vector transP = transformation.pointInv(p);
        return primitiveIsInside(transP);
    }
    
    protected abstract boolean primitiveIsOutside(final Vector p);
    
    @Override
    public boolean isOutside(final Vector p, final Primitive primitive)
    {
        if (primitive == this)
        {
            return true;
        }
        
        final Vector transP = transformation.pointInv(p);

        return primitiveIsOutside(transP);
    }
    
    protected abstract void primitiveAllIntersections(
            final IntersectionStack iStack, final Vector p, final Vector dir);
    
    @Override
    public void allIntersections(final IntersectionStack iStack,
            final Vector p, final Vector dir)
    {
        final Vector transP = transformation.pointInv(p);
        final Vector transDir = transformation.dirInv(dir);
        
        primitiveAllIntersections(iStack, transP, transDir);
    }
    
    @Override
    public void transform(final Transformation tr)
    {
        super.transform(tr);
        transformation = transformation.compose(tr);
    }
}
