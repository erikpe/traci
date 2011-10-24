package traci.model.material.pigment;

import traci.math.Transformation;
import traci.math.Vector;
import traci.model.material.Color;

public abstract class Pigment
{
    public static Pigment getDefault()
    {
        return Solid.make(Color.WHITE);
    }
    
    public abstract Pigment transform(final Transformation transformation);
    
    public abstract Color getColor(final Vector p);
}
