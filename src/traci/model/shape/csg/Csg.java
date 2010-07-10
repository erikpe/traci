package traci.model.shape.csg;

import java.util.ArrayList;
import java.util.List;

import traci.math.Transformation;
import traci.model.material.Material;
import traci.model.shape.Shape;

public abstract class Csg extends Shape
{
    private final List<Shape> shapes;
    
    public Csg(final Material material)
    {
        super(material);
        shapes = new ArrayList<Shape>();
    }
    
    protected List<Shape> getShapes()
    {
        return shapes;
    }
    
    public void add(final Shape shape)
    {
        shapes.add(shape);
    }
    
    @Override
    public void transform(final Transformation tr)
    {
        super.transform(tr);
        
        for (final Shape shape : getShapes())
        {
            shape.transform(tr);
        }
    }
}
