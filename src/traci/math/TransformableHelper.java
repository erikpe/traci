package traci.math;

public abstract class TransformableHelper implements Transformable
{
    public void rotateX(final double theta)
    {
        transform(Transformation.rotateX(theta));
    }
    
    public void rotateY(final double theta)
    {
        transform(Transformation.rotateY(theta));
    }
    
    public void rotateZ(final double theta)
    {
        transform(Transformation.rotateZ(theta));
    }
    
    public void scale(final Vector vec)
    {
        transform(Transformation.scale(vec));
    }
    
    public void scale(final double val)
    {
        transform(Transformation.scale(val));
    }
    
    public void scale(final double x, final double y, final double z)
    {
        transform(Transformation.scale(x, y, z));
    }
    
    public void scaleX(final double x)
    {
        transform(Transformation.scale(x, 1, 1));
    }
    
    public void scaleY(final double y)
    {
        transform(Transformation.scale(1, y, 1));
    }
    
    public void scaleZ(final double z)
    {
        transform(Transformation.scale(1, 1, z));
    }
    
    public void translate(final Vector vec)
    {
        transform(Transformation.translate(vec));
    }
    
    public void translate(final double x, final double y, final double z)
    {
        transform(Transformation.translate(x, y, z));
    }
    
    public void translateX(final double x)
    {
        transform(Transformation.translate(x, 0, 0));
    }
    
    public void translateY(final double y)
    {
        transform(Transformation.translate(0, y, 0));
    }
    
    public void translateZ(final double z)
    {
        transform(Transformation.translate(0, 0, z));
    }
}
