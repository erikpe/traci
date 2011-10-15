package traci.model.shape.csg;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import traci.math.Transformation;
import traci.model.material.Color;
import traci.model.material.Material;
import traci.model.shape.BoundingBox;
import traci.model.shape.Shape;

public abstract class Csg extends Shape implements Iterable<Shape>
{
    protected List<Shape> shapes;
    protected int numShapes;
    
    protected BoundingBox bBox;
    
    public Csg(final Material material)
    {
        super(material);
        
        this.shapes = new ArrayList<Shape>();
        this.numShapes = this.shapes.size();
    }
    
    public void add(final Shape shape)
    {
        shapes.add(shape);
        numShapes = shapes.size();
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
        super.transform(tr);
        
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
        return shapes.iterator();
    }
    
    @Override
    public void setColor(final Color color)
    {
        for (final Shape shape : shapes)
        {
            shape.setColor(color);
        }
    }
    
    @Override
    public Object clone()
    {
        final Csg res = (Csg) super.clone();
        res.shapes = new ArrayList<Shape>();
        res.numShapes = res.shapes.size();
        
        for (final Shape shape : shapes)
        {
            res.add((Shape) shape.clone());
        }
        
        return res;
    }
}
