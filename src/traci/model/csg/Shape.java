package traci.model.csg;

import traci.math.Matrix;
import traci.math.Vector;
import traci.model.texture.Color;
import traci.model.texture.Texture;
import traci.render.Intervals;

public abstract class Shape
{
    private final Texture material;
    
    public Shape(final Texture material)
    {
        this.material = (material == null ? Texture.DEFAULT : material);
    }
    
    public void rotateX(final double theta)
    {
        transform(Matrix.rotX(-theta), Matrix.rotX(theta));
    }
    
    public void rotateY(final double theta)
    {
        transform(Matrix.rotY(-theta), Matrix.rotY(theta));
    }
    
    public void rotateZ(final double theta)
    {
        transform(Matrix.rotZ(-theta), Matrix.rotZ(theta));
    }
    
    public void scale(final Vector vec)
    {
        final Vector trVec = Vector.make(1.0/vec.x, 1.0/vec.y, 1.0/vec.z);
        final Matrix mat = Matrix.scale(trVec);
        transform(mat, mat);
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
        transform(Matrix.translate(vec.neg()), null);
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
    
    protected abstract void transform(final Matrix invMat, final Matrix normalMat);
    
    public abstract Intervals shootRay(final Vector p, final Vector lookAt);
    
    public double ambCoeff()
    {
        return material.ambientCoeff;
    }
    
    public double diffCoeff()
    {
        return material.diffuseCoeff;
    }
    
    public double specCoeff()
    {
        return material.specularCoeff;
    }
    
    public double shininess()
    {
        return material.shininess;
    }
    
    public Color getColor()
    {
        return material.color;
    }
    
    public void setColor(final Color color)
    {
        material.color = color;
    }
}
