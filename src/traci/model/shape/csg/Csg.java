package traci.model.shape.csg;

import java.util.ArrayList;
import java.util.List;

import traci.math.Transformation;
import traci.model.material.Material;
import traci.model.shape.BoundingBox;
import traci.model.shape.Shape;

public abstract class Csg extends Shape
{
    protected final List<Shape> shapes;
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
}
