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
    
    protected BoundingBox bBox;
    
    public Csg(final Material material)
    {
        super(material);
        shapes = new ArrayList<Shape>();
    }
    
    public void add(final Shape shape)
    {
        shapes.add(shape);
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
