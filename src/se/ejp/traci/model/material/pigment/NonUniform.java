package se.ejp.traci.model.material.pigment;

import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.material.Color;

public class NonUniform extends Pigment
{
    public static interface NonUniformPigment
    {
        public Color getColorTransformed(final Vector p);
    }

    private final NonUniformPigment nonUniformPigment;
    private final Transformation transformation;

    public NonUniform(final Transformation transformation, final NonUniformPigment nonUniformPigment)
    {
        this.nonUniformPigment = nonUniformPigment;
        this.transformation = transformation;
    }

    @Override
    public NonUniform transform(final Transformation tr)
    {
        return new NonUniform(transformation.compose(tr), nonUniformPigment);
    }

    @Override
    public Color getColor(final Vector p)
    {
        return nonUniformPigment.getColorTransformed(transformation.pointInv(p));
    }
}