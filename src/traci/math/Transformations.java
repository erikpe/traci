package traci.math;

public class Transformations
{
    private static final Transformation IDENTITY = Transformation.make(Matrix
            .eye(), Matrix.eye());
    
    private static Transformation make(final Matrix mat, final Matrix invMat)
    {
        return Transformation.make(mat, invMat);
    }
    
    public static Transformation identity()
    {
        return IDENTITY;
    }
    
    public static Transformation translate(final Vector vec)
    {
        return make(Matrix.translate(vec), Matrix.translate(vec.neg()));
    }
    
    public static Transformation scale(final Vector vec)
    {
        final Vector invVec = Vector
                .make(1.0 / vec.x(), 1.0 / vec.y(), 1.0 / vec.z());
        return make(Matrix.scale(vec), Matrix.scale(invVec));
    }
    
    public static Transformation rotx(final double theta)
    {
        return make(Matrix.rotx(theta), Matrix.rotx(-theta));
    }
    
    public static Transformation roty(final double theta)
    {
        return make(Matrix.roty(theta), Matrix.roty(-theta));
    }
    
    public static Transformation rotz(final double theta)
    {
        return make(Matrix.rotz(theta), Matrix.rotz(-theta));
    }
    
    public static Transformation rotVecToZ(final Vector vec)
    {
        final Vector vecNorm = vec.normalize();
        final Vector yzVec = Vector.make(0, vecNorm.y(), vecNorm.z());
        final double yzLen = yzVec.length();
        
        final Transformation rotX;
        
        if (yzLen > 1.0e-15)
        {
            final double cosAlpha = vecNorm.z() / yzLen;
            final double sinAlpha = vecNorm.y() / yzLen;

            rotX = make(Matrix.rotx(sinAlpha, cosAlpha), Matrix.rotx(-sinAlpha,
                    cosAlpha));
        }
        else
        {
            rotX = identity();
        }
        
        final double cosBeta = yzLen;
        final double sinBeta = -vecNorm.x();
        
        final Transformation rotY = make(Matrix.roty(sinBeta, cosBeta),
                Matrix.roty(-sinBeta, cosBeta));
        
        return rotX.transform(rotY);
    }
    
    public static Transformation rotZToVec(final Vector vec)
    {
        final Vector vecNorm = vec.normalize();
        final Vector yzVec = Vector.make(0, vecNorm.y(), vecNorm.z());
        final double yzLen = yzVec.length();
        
        final Transformation rotX;
        
        if (yzLen > 1.0e-15)
        {
            final double cosAlpha = vecNorm.z() / yzLen;
            final double sinAlpha = vecNorm.y() / yzLen;
            
            rotX = make(Matrix.rotx(-sinAlpha, cosAlpha), Matrix.rotx(sinAlpha,
                    cosAlpha));
        }
        else
        {
            rotX = identity();
        }
        
        final double cosBeta = yzLen;
        final double sinBeta = -vecNorm.x();
        
        final Transformation rotY = make(Matrix.roty(-sinBeta, cosBeta),
                Matrix.roty(sinBeta, cosBeta));
        
        return rotY.transform(rotX);
    }
    
    public static Transformation rotVecToVec(final Vector vec1, final Vector vec2)
    {
        return rotVecToZ(vec1).transform(rotZToVec(vec2));
    }
    
    public static Transformation camera(final Vector location,
            final Vector lookAt, Vector up)
    {
        final Vector forward = lookAt.sub(location).normalize();
        final Vector right = forward.cross(up).normalize();
        up = right.cross(forward).normalize();
        
        Transformation res = Transformations.scale(Vector.make(1, 1, -1));
        res = res.transform(make(Matrix.make(right, up, forward), Matrix.eye()));
        res = res.transform(translate(location));
        
        return res;
    }
}
