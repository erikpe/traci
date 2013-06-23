package se.ejp.traci.model.shape;

import se.ejp.traci.math.Transformable;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;

public class BoundingBox implements Transformable, Cloneable
{
    private Transformation transformation;

    private BoundingBox()
    {
        transformation = Transformations.identity();
    }

    public static BoundingBox make()
    {
        return new BoundingBox();
    }

    public static BoundingBox make(final Vector v0, final Vector v1)
    {
        final BoundingBox bbox = make();

        final double xSize = Math.abs(v1.x() - v0.x());
        final double ySize = Math.abs(v1.y() - v0.y());
        final double zSize = Math.abs(v1.z() - v0.z());

        final double x = Math.min(v0.x(), v1.x());
        final double y = Math.min(v0.y(), v1.y());
        final double z = Math.min(v0.z(), v1.z());

        bbox.transform(Transformations.scale(xSize, ySize, zSize));
        bbox.transform(Transformations.translate(x, y, z));

        return bbox;
    }

    @Override
    public void transform(final Transformation tr)
    {
        transformation = transformation.compose(tr);
    }

    private final double min(final double val0, final double val1)
    {
        return val0 < val1 ? val0 : val1;
    }

    private final double max(final double val0, final double val1)
    {
        return val0 > val1 ? val0 : val1;
    }

    public boolean test(final Vector p, final Vector dir)
    {
        final Vector transP = transformation.pointInv(p);
        final Vector transDir = transformation.dirInv(dir);

        /**
         * Plane x = 0 and x = 1
         */
        final double x0 = -transP.x() / transDir.x();
        final double x1 = (1.0 - transP.x()) / transDir.x();

        double near = min(x0, x1);
        double far = max(x0, x1);

        if (far < 0.0)
        {
            return false;
        }

        /**
         * Plane y = 0 and y = 1
         */
        final double y0 = -transP.y() / transDir.y();
        final double y1 = (1.0 - transP.y()) / transDir.y();

        near = max(near, min(y0, y1));
        far = min(far, max(y0, y1));

        if (far < 0.0 || far < near)
        {
            return false;
        }

        /**
         * Plane z = 0 and z = 1
         */
        final double z0 = -transP.z() / transDir.z();
        final double z1 = (1.0 - transP.z()) / transDir.z();

        near = max(near, min(z0, z1));
        far = min(far, max(z0, z1));

        if (far < 0.0 || far < near)
        {
            return false;
        }

        return true;
    }

    public Transformation getTransformation()
    {
        return transformation;
    }

    @Override
    public String toString()
    {
        return "BoundingBox";
    }

    @Override
    public BoundingBox clone() throws CloneNotSupportedException
    {
        return (BoundingBox) super.clone();
    }
}
