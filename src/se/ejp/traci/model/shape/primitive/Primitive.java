package se.ejp.traci.model.shape.primitive;

import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.material.Finish;
import se.ejp.traci.model.material.Interior;
import se.ejp.traci.model.material.Material;
import se.ejp.traci.model.material.Texture;
import se.ejp.traci.model.material.pigment.Pigment;
import se.ejp.traci.model.shape.Shape;
import se.ejp.traci.render.Ray;

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

    protected abstract Ray primitiveShootRay(final Vector p, final Vector dir);

    @Override
    public Ray shootRay(final Vector p, final Vector dir)
    {
        final Vector transP = transformation.pointInv(p);
        final Vector transDir = transformation.dirInv(dir);

        return primitiveShootRay(transP, transDir);
    }

    @Override
    public Shape optimize()
    {
        return this;
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

    @Override
    public void setInterior(final Interior interior)
    {
        material = material.setInterior(interior);
    }

    public Material getMaterial()
    {
        return material;
    }

    public Transformation getTransformation()
    {
        return transformation;
    }
}
