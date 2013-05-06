package se.ejp.traci.model.material.pigment;

import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.material.Color;

public abstract class Pigment
{
    public static Pigment getDefault()
    {
        return Solid.make(Color.WHITE);
    }

    public abstract Pigment transform(final Transformation transformation);
    public abstract Color getColor(final Vector p);
}
