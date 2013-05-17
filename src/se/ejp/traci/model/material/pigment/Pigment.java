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

    @Override
    public int hashCode()
    {
        // All concrete subclasses should override this method
        throw new RuntimeException("Pigment.hashCode() called");
    }

    @Override
    public boolean equals(final Object other)
    {
        // All concrete subclasses should override this method
        throw new RuntimeException("Pigment.equals(Object) called");
    }
}
