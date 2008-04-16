package traci.model.csg;

import traci.math.Matrix;
import traci.math.Transformation;
import traci.math.Vector;
import traci.model.texture.Texture;
import traci.render.Ray;

public abstract class Primitive extends Shape
{
    public final Transformation transform;
    
    public Primitive(final Texture texture)
    {
        super(texture);
        transform = new Transformation();
    }
    
    public abstract Ray primitiveShootRay(final Vector p, final Vector dir);
    
    public Ray shootRay(final Vector p, final Vector dir)
    {
        final Vector transP = transform.pointInv(p);
        final Vector transDir = transform.dirInv(dir);
        
        return primitiveShootRay(transP, transDir);
    }
    
    public void transform(final Matrix mat, final Matrix invMat)
    {
        transform.transform(mat, invMat);
    }
}
