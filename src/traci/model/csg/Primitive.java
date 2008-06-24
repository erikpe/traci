package traci.model.csg;

import traci.math.Matrix;
import traci.math.Transformation;
import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Ray;

public abstract class Primitive extends Shape
{
    public final Transformation transform;
    
    public static boolean isPrimitive(final String str)
    {
        return str.equals("sphere")
            || str.equals("box")
            || str.equals("cylinder")
            || str.equals("plane")
            || str.equals("torus")
            || str.equals("quadric")
            || str.equals("cubic")
            || str.equals("quartic");
    }
    
    public Primitive(final Material material)
    {
        super(material);
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
