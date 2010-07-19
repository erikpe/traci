package traci.math;

public abstract class TransformableHelper implements Transformable
{
    public void translate(final Vector vec)
    {
        transform(Transformations.translate(vec));
    }
    
    public void translate(final double x, final double y, final double z)
    {
        translate(Vector.make(x, y, z));
    }
    
    public void translatex(final double x)
    {
        translate(Vector.make(x, 0, 0));
    }
    
    public void translatey(final double y)
    {
        translate(Vector.make(0, y, 0));
    }
    
    public void translatez(final double z)
    {
        translate(Vector.make(0, 0, z));
    }
    
    public void scale(final Vector vec)
    {
        transform(Transformations.scale(vec));
    }
    
    public void scale(final double x, final double y, final double z)
    {
        scale(Vector.make(x, y, z));
    }
    
    public void scale(final double val)
    {
        scale(Vector.make(val, val, val));
    }
    
    public void scalex(final double x)
    {
        scale(Vector.make(x, 1, 1));
    }
    
    public void scaley(final double y)
    {
        scale(Vector.make(1, y, 1));
    }
    
    public void scalez(final double z)
    {
        scale(Vector.make(1, 1, z));
    }
    
    public void rotx(final double theta)
    {
        transform(Transformations.rotx(theta));
    }
    
    public void roty(final double theta)
    {
        transform(Transformations.roty(theta));
    }
    
    public void rotz(final double theta)
    {
        transform(Transformations.rotz(theta));
    }
}
