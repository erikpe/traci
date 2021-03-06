package se.ejp.traci.math;

public class Transformations
{
    private static final Transformation IDENTITY = make(Matrix.eye(), Matrix.eye());

    private static Transformation make(final Matrix mat, final Matrix invMat)
    {
        return Transformation.make(mat, invMat);
    }

    public static Transformation identity()
    {
        return IDENTITY;
    }

    public static Transformation translate(final Double x, final Double y, final Double z)
    {
        return translate(Vector.make(x, y, z));
    }

    public static Transformation translate(final Vector vec)
    {
        return make(Matrix.translate(vec), Matrix.translate(vec.neg()));
    }

    public static Transformation scale(final Double val)
    {
        return scale(Vector.make(val, val, val));
    }

    public static Transformation scale(final Double x, final Double y, final Double z)
    {
        return scale(Vector.make(x, y, z));
    }

    public static Transformation scale(final Vector vec)
    {
        final Vector invVec = Vector.make(1.0 / vec.x(), 1.0 / vec.y(), 1.0 / vec.z());
        return make(Matrix.scale(vec), Matrix.scale(invVec));
    }

    public static Transformation scalex(final Double val)
    {
        return scale(val, 1.0, 1.0);
    }

    public static Transformation scaley(final Double val)
    {
        return scale(1.0, val, 1.0);
    }

    public static Transformation scalez(final Double val)
    {
        return scale(1.0, 1.0, val);
    }

    public static Transformation rotx(final Double theta)
    {
        return make(Matrix.rotx(theta), Matrix.rotx(-theta));
    }

    public static Transformation roty(final Double theta)
    {
        return make(Matrix.roty(theta), Matrix.roty(-theta));
    }

    public static Transformation rotz(final Double theta)
    {
        return make(Matrix.rotz(theta), Matrix.rotz(-theta));
    }

    public static Transformation rotVecToZ(final Vector vec)
    {
        final Vector vecNorm = vec.normalize();
        final Vector yzVec = Vector.make(0, vecNorm.y(), vecNorm.z());
        final double yzLen = yzVec.length();

        Transformation res = identity();

        if (yzLen > 1.0e-15)
        {
            final double cosAlpha = vecNorm.z() / yzLen;
            final double sinAlpha = vecNorm.y() / yzLen;

            res = res.compose(make(Matrix.rotx(sinAlpha, cosAlpha), Matrix.rotx(-sinAlpha, cosAlpha)));
        }

        final double cosBeta = yzLen;
        final double sinBeta = -vecNorm.x();

        res = res.compose(make(Matrix.roty(sinBeta, cosBeta), Matrix.roty(-sinBeta, cosBeta)));

        return res;
    }

    public static Transformation rotZToVec(final Vector vec)
    {
        return rotVecToZ(vec).invert();
    }

    public static Transformation rotVecToVec(final Vector vec1, final Vector vec2)
    {
        return rotVecToZ(vec1).compose(rotZToVec(vec2));
    }

    public static Transformation rotAround(final Vector vec1, final Vector vec2, final Double theta)
    {
        final Vector dir = vec2.sub(vec1).normalize();

        final Transformation rotToZ = rotVecToZ(dir);
        Transformation res = identity();

        res = res.compose(translate(vec1.neg()));
        res = res.compose(rotToZ);
        res = res.compose(rotz(theta));
        res = res.compose(rotToZ.invert());
        res = res.compose(translate(vec1));

        return res;
    }

    /**
     * Initial camera position is:
     * right = {@link Vector.UNIT_X}
     * up = {@link Vector.UNIT_Y}
     * forward = {@ Vector.UNIT_Z}
     */
    public static Transformation camera(final Vector location,
            final Vector lookAt, Vector up)
    {
        final Vector forward = lookAt.sub(location).normalize();
        final Vector right = forward.cross(up).normalize();
        up = right.cross(forward).normalize();

        Transformation res = identity();

        res = res.compose(make(Matrix.make(right, up, forward), Matrix.eye()));
        res = res.compose(translate(location));

        return res;
    }
}
