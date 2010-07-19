package traci.math;

public class Transformation
{
    private final Matrix mat;
    
    private final Matrix invMat;
    
    private Transformation(final Matrix mat, final Matrix invMat)
    {
        this.mat = mat;
        this.invMat = invMat;
    }
    
    static Transformation make(final Matrix mat, final Matrix invMat)
    {
        return new Transformation(mat, invMat);
    }
    
    public Transformation compose(final Transformation transformation)
    {
        final Matrix newMat = transformation.mat.mul(mat);
        final Matrix newInvMat = invMat.mul(transformation.invMat);
        
        return make(newMat, newInvMat);
    }
    
    public Transformation invert()
    {
        return make(invMat, mat);
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
