package traci.math;

public class Transformation
{
    private Matrix mat;
    
    private Matrix invMat;
    
    public Transformation()
    {
        mat = Matrix.eye();
        invMat = Matrix.eye();
    }
    
    public void transform(final Matrix bMat, final Matrix bInvMat)
    {
        mat = bMat.mul(mat);
        invMat = invMat.mul(bInvMat);
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
