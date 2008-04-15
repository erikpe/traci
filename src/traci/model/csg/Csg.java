package traci.model.csg;

import java.util.ArrayList;
import java.util.List;

import traci.math.Matrix;
import traci.model.texture.Texture;

public abstract class Csg extends Shape
{
    private final List<Shape> shapes;
    
    public Csg(final Texture material)
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
    public void transform(final Matrix invMat, final Matrix normalMat)
    {
        for (final Shape shape : getShapes())
        {
            shape.transform(invMat, normalMat);
        }
    }
}
