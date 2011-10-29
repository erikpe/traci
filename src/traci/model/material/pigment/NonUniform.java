package traci.model.material.pigment;

import traci.math.Transformation;
import traci.math.Vector;
import traci.model.material.Color;

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
