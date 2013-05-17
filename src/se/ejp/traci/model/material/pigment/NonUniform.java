package se.ejp.traci.model.material.pigment;

import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.material.Color;

public abstract class NonUniform extends Pigment
{
    protected final Transformation transformation;

    protected NonUniform(final Transformation transformation)
    {
        this.transformation = transformation;
    }

    protected abstract Color getColorTransformed(final Vector vec);

    @Override
    public Color getColor(final Vector p)
    {
        return getColorTransformed(transformation.pointInv(p));
    }
}
