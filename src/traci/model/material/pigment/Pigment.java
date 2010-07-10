package traci.model.material.pigment;

import traci.math.Transformable;
import traci.math.TransformableHelper;
import traci.math.Vector;
import traci.model.material.Color;

public abstract class Pigment extends TransformableHelper
        implements Transformable, Cloneable
{
    public abstract Color getColor(final Vector p);
    
    @Override
    public Object clone() throws CloneNotSupportedException
    {
        return super.clone();
    }
}
