package traci.model.csg;

import traci.math.Matrix;
import traci.math.Vector;
import traci.model.texture.Texture;
import traci.render.Intervals;

public abstract class Primitive extends Shape
{
    private Matrix invMat;
    
    private Matrix normalMat;
    
    public Primitive(final Texture material)
    {
        super(material);
        
        this.invMat = Matrix.eye();
        this.normalMat = Matrix.eye();
    }
    
    public Intervals primitiveShootRay(final Vector p, final Vector dir)
    {
        throw new UnsupportedOperationException();
    }
    
    public Intervals shootRay(final Vector p, final Vector lookAt)
    {
        final Vector transP = invMat.mul(p);
        final Vector transLookAt = invMat.mul(lookAt);
        final Vector dir = transLookAt.sub(transP);
        
        final Intervals ivals = primitiveShootRay(transP, dir);
        
        if (ivals != null)
        {
            ivals.transformNormals(normalMat);
        }
        
        return ivals;
    }
    
    public void transform(final Matrix invMat, final Matrix normalMat)
    {
        this.invMat = this.invMat.mul(invMat);
        
        if (normalMat != null)
        {
            this.normalMat = normalMat.mul(this.normalMat);
        }
    }
}
