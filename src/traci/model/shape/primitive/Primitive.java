package traci.model.shape.primitive;

import traci.math.Transformation;
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
        transformation = Transformation.identity();
    }
    
    public abstract Ray primitiveShootRay(final Vector p, final Vector dir);
    
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
        transformation = transformation.transform(tr);
    }
}
