package traci.model.material.pigment;

import traci.math.Transformation;
import traci.math.Vector;
import traci.model.material.Color;

public abstract class NonUniform extends Pigment
{
    private Transformation transformation = Transformation.identity();
    
    protected abstract Color getColorTransformed(final Vector p);
    
    @Override
    public void transform(Transformation tr)
    {
        transformation = transformation.transform(tr);
    }
    
    @Override
    public Color getColor(final Vector p)
    {
        return getColorTransformed(transformation.pointInv(p));
    }
}
