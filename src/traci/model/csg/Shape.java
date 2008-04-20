package traci.model.csg;

import traci.math.Matrix;
import traci.math.Vector;
import traci.model.material.Material;
import traci.render.Ray;

public abstract class Shape
{
    protected static final double EPSILON = 0.000001;
    
    public final Material material;
    
    public static boolean isShape(final String str)
    {
        return Csg.isCsg(str) || Primitive.isPrimitive(str);
    }
    
    public Shape(final Material material)
    {
        if (material == null)
        {
            this.material = Material.newDefault();
        }
        else
        {
            this.material = material;
        }
    }
    
    public void rotateX(final double theta)
    {
        transform(Matrix.rotX(theta), Matrix.rotX(-theta));
    }
    
    public void rotateY(final double theta)
    {
        transform(Matrix.rotY(theta), Matrix.rotY(-theta));
    }
    
    public void rotateZ(final double theta)
    {
        transform(Matrix.rotZ(theta), Matrix.rotZ(-theta));
    }
    
    public void scale(final Vector vec)
    {
        final Vector invVec = Vector.make(1.0/vec.x, 1.0/vec.y, 1.0/vec.z);
        transform(Matrix.scale(vec), Matrix.scale(invVec));
    }
    
    public void scale(final double val)
    {
        scale(Vector.make(val, val, val));
    }
    
    public void scale(final double x, final double y, final double z)
    {
        scale(Vector.make(x, y, z));
    }
    
    public void scaleX(final double x)
    {
        scale(x, 1, 1);
    }
    
    public void scaleY(final double y)
    {
        scale(1, y, 1);
    }
    
    public void scaleZ(final double z)
    {
        scale(1, 1, z);
    }
    
    public void translate(final Vector vec)
    {
        transform(Matrix.translate(vec), Matrix.translate(vec.neg()));
    }
    
    public void translate(final double x, final double y, final double z)
    {
        translate(Vector.make(x, y, z));
    }
    
    public void translateX(final double x)
    {
        translate(x, 0, 0);
    }
    
    public void translateY(final double y)
    {
        translate(0, y, 0);
    }
    
    public void translateZ(final double z)
    {
        translate(0, 0, z);
    }
    
    protected abstract void transform(final Matrix mat, final Matrix invMat);
    
    public abstract Ray shootRay(final Vector p, final Vector dir);
}
