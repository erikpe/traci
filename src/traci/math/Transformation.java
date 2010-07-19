package traci.math;

public class Transformation
{
    private final Matrix mat;
    
    private final Matrix invMat;
    
    private static final Transformation IDENTITY =
        make(Matrix.eye(), Matrix.eye());
    
    private Transformation(final Matrix mat, final Matrix invMat)
    {
        this.mat = mat;
        this.invMat = invMat;
    }
    
    public static Transformation make(final Matrix mat, final Matrix invMat)
    {
        return new Transformation(mat, invMat);
    }
    
    public static Transformation identity()
    {
        return IDENTITY;
    }
    
    public static Transformation rotateX(final double theta)
    {
        return make(Matrix.rotX(theta), Matrix.rotX(-theta));
    }
    
    public static Transformation rotateY(final double theta)
    {
        return make(Matrix.rotY(theta), Matrix.rotY(-theta));
    }
    
    public static Transformation rotateZ(final double theta)
    {
        return make(Matrix.rotZ(theta), Matrix.rotZ(-theta));
    }
    
    public static Transformation scale(final Vector vec)
    {
        final Vector invVec = Vector
                .make(1.0 / vec.x, 1.0 / vec.y, 1.0 / vec.z);
        return make(Matrix.scale(vec), Matrix.scale(invVec));
    }
    
    public static Transformation scale(final double val)
    {
        return scale(Vector.make(val, val, val));
    }
    
    public static Transformation scale(final double x, final double y, final double z)
    {
        return scale(Vector.make(x, y, z));
    }
    
    public static Transformation scaleX(final double x)
    {
        return scale(x, 1, 1);
    }
    
    public static Transformation scaleY(final double y)
    {
        return scale(1, y, 1);
    }
    
    public static Transformation scaleZ(final double z)
    {
        return scale(1, 1, z);
    }
    
    public static  Transformation translate(final Vector vec)
    {
        return make(Matrix.translate(vec), Matrix.translate(vec.neg()));
    }
    
    public static Transformation translate(final double x, final double y,
            final double z)
    {
        return translate(Vector.make(x, y, z));
    }
    
    public static Transformation translateX(final double x)
    {
        return translate(x, 0, 0);
    }
    
    public static Transformation translateY(final double y)
    {
        return translate(0, y, 0);
    }
    
    public static Transformation translateZ(final double z)
    {
        return translate(0, 0, z);
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

            rotX = make(Matrix.rotX(sinAlpha, cosAlpha), Matrix.rotX(-sinAlpha,
                    cosAlpha));
        }
        else
        {
            rotX = identity();
        }
        
        final double cosBeta = yzLen;
        final double sinBeta = -vecNorm.x();
        
        final Transformation rotY = make(Matrix.rotY(sinBeta, cosBeta),
                Matrix.rotY(-sinBeta, cosBeta));
        
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
            
            rotX = make(Matrix.rotX(-sinAlpha, cosAlpha), Matrix.rotX(sinAlpha,
                    cosAlpha));
        }
        else
        {
            rotX = identity();
        }
        
        final double cosBeta = yzLen;
        final double sinBeta = -vecNorm.x();
        
        final Transformation rotY = make(Matrix.rotY(-sinBeta, cosBeta),
                Matrix.rotY(sinBeta, cosBeta));
        
        return rotY.transform(rotX);
    }
    
    public static Transformation rotVecToVec(final Vector vec1, final Vector vec2)
    {
        return rotVecToZ(vec1).transform(rotZToVec(vec2));
    }
    
    public static Transformation camera(final Vector location,
            final Vector lookAt, Vector up)
    {
        if (up == null)
        {
            up = Vector.UNIT_Y;
        }
        
        final Vector forward = lookAt.sub(location).normalize();
        final Vector right = forward.cross(up).normalize();
        up = right.cross(forward).normalize();
        
        Transformation res = Transformation.scaleZ(-1);
        res = res.transform(make(Matrix.make(right, up, forward), Matrix.eye()));
        res = res.transform(translate(location));
        
        return res;
    }
    
    public Transformation transform(final Transformation transformation)
    {
        Matrix newMat = transformation.mat.mul(mat);
        Matrix newInvMat = invMat.mul(transformation.invMat);
        
        return make(newMat, newInvMat);
    }
    
    public Vector point(final Vector vec)
    {
        return mat.mul(vec);
    }
    
    public Vector pointInv(final Vector vec)
    {
        return invMat.mul(vec);
    }
    
    public Vector dir(final Vector vec)
    {
        return mat.mulDir(vec);
    }
    
    public Vector dirInv(final Vector vec)
    {
        return invMat.mulDir(vec);
    }
    
    public Vector normal(final Vector vec)
    {
        return invMat.mulNormal(vec);
    }
    
    public Vector normalInv(final Vector vec)
    {
        return mat.mulNormal(vec);
    }
}
