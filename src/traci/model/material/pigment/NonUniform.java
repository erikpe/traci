package traci.model.material.pigment;

import traci.math.Matrix;
import traci.math.Transformation;
import traci.math.Vector;
import traci.model.material.Color;

public abstract class NonUniform extends Pigment
{
    protected Transformation transform = new Transformation();
    
    protected abstract Color getColorTransformed(final Vector p);
    
    @Override
    public void transform(final Matrix mat, final Matrix invMat)
    {
        transform.transform(mat, invMat);
    }
    
    @Override
    public Color getColor(final Vector p)
    {
        return getColorTransformed(transform.pointInv(p));
    }
}
