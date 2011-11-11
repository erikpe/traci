package traci.model.shape.csg;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import traci.math.Transformation;
import traci.model.material.Finish;
import traci.model.material.Material;
import traci.model.material.Texture;
import traci.model.material.pigment.Pigment;
import traci.model.shape.BoundingBox;
import traci.model.shape.Shape;

public abstract class Csg extends Shape implements Iterable<Shape>
{
    protected Shape[] shapes;
    protected int numShapes;
    protected BoundingBox bBox;

    public Csg()
    {
        this.shapes = new Shape[0];
        this.numShapes = 0;
    }

    public void add(final Shape shape)
    {
        final Shape[] oldShapes = shapes;
        shapes = new Shape[++numShapes];
        System.arraycopy(oldShapes, 0, shapes, 0, numShapes - 1);
        shapes[numShapes - 1] = shape;
    }

    public BoundingBox getBoundingBox()
    {
        return bBox;
    }

    public void setBoundingBox(final BoundingBox bBox)
    {
        this.bBox = bBox;
    }

    @Override
    public void transform(final Transformation tr)
    {
        if (bBox != null)
        {
            bBox.transform(tr);
        }

        for (final Shape shape : shapes)
        {
            shape.transform(tr);
        }
    }

    @Override
    public Iterator<Shape> iterator()
    {
        return Arrays.asList(shapes).iterator();
    }

    @Override
    public void setMaterial(final Material material)
    {
        for (final Shape shape : shapes)
        {
            shape.setMaterial(material);
        }
    }

    @Override
    public void setTexture(final Texture texture)
    {
        for (final Shape shape : shapes)
        {
            shape.setTexture(texture);
        }
    }

    @Override
    public void setPigment(final Pigment pigment)
    {
        for (final Shape shape : shapes)
        {
            shape.setPigment(pigment);
        }
    }

    @Override
    public void setFinish(final Finish finish)
    {
        for (final Shape shape : shapes)
        {
            shape.setFinish(finish);
        }
    }

    @Override
    public Object clone()
    {
        final Csg res = (Csg) super.clone();
        res.shapes = new Shape[numShapes];
        res.numShapes = numShapes;

        for (int i = 0; i < numShapes; ++i)
        {
            res.shapes[i] = (Shape) shapes[i].clone();
        }

        return res;
    }
}
