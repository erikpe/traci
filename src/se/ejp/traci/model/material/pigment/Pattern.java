package se.ejp.traci.model.material.pigment;

import se.ejp.traci.math.Transformation;

public abstract class Pattern extends NonUniform
{
    protected Pattern(final Transformation transformation)
    {
        super(transformation);
    }
}
