package traci.model.shape.primitive;

import traci.math.Transformation;
import traci.math.Transformations;
import traci.math.Vector;
import traci.model.material.Finish;
import traci.model.material.Material;
import traci.model.material.Texture;
import traci.model.material.pigment.Pigment;
import traci.model.shape.Shape;
import traci.render.Ray2;

public abstract class Primitive extends Shape
{
    private Transformation transformation;
    private Material material;
    
    protected Primitive()
    {
        this.transformation = Transformations.identity();
        this.material = Material.getDefault();
    }
    
    protected final static double min(final double val0, final double val1)
    {
        return val0 < val1 ? val0 : val1;
    }
    
    protected final static double max(final double val0, final double val1)
    {
        return val0 > val1 ? val0 : val1;
    }
    
    protected abstract Vector primitiveGetNormalAt(final Vector p);
    
    public Vector getNormalAt(final Vector p, final Vector dir)
    {
        Vector normal = primitiveGetNormalAt(transformation.pointInv(p));
        normal = transformation.normal(normal).normalize();
        
        if (dir.dot(normal) > 0)
        {
            normal = normal.neg();
        }
        
        return normal;
    }
    
    protected abstract Ray2 primitiveShootRay2(final Vector p, final Vector dir);
    
    public Ray2 shootRay2(final Vector p, final Vector dir)
    {
        final Vector transP = transformation.pointInv(p);
        final Vector transDir = transformation.dirInv(dir);
        
        return primitiveShootRay2(transP, transDir);
    }
    
    @Override
    public void transform(final Transformation tr)
    {
        transformation = transformation.compose(tr);
        material = material.transform(tr);
    }
    
    @Override
    public void setMaterial(final Material newMaterial)
    {
        material = newMaterial;
    }
    
    @Override
    public void setTexture(final Texture texture)
    {
        material = material.setTexture(texture);
    }
    
    @Override
    public void setPigment(final Pigment pigment)
    {
        material = material.setPigment(pigment);
    }
    
    @Override
    public void setFinish(final Finish finish)
    {
        material = material.setFinish(finish);
    }
    
    public Material getMaterial()
    {
        return material;
    }
    
    @Override
    public Object clone()
    {
        final Primitive res = (Primitive) super.clone();
        res.transformation = transformation;
        res.material = material;
        return res;
    }
}
