package traci.model.shape.primitive;

import traci.math.Transformation;
import traci.math.Transformations;
import traci.math.Vector;
import traci.model.material.Material;
import traci.model.shape.Shape;
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
        
        if (dir.dot(normal) > 0)
        {
            normal = normal.neg();
        }
        
        return normal.normalize();
    }
    
    protected abstract Ray primitiveShootRay(final Vector p, final Vector dir);
    
    @Override
    public Ray shootRay(final Vector p, final Vector dir)
    {
        final Vector transP = transformation.pointInv(p);
        final Vector transDir = transformation.dirInv(dir);
        
        return primitiveShootRay(transP, transDir);
    }
    
    @Override
    public void transform(final Transformation tr)
    {
        super.transform(tr);
        transformation = transformation.compose(tr);
    }
}
