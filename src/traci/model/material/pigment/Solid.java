package traci.model.material.pigment;

import traci.math.Matrix;
import traci.math.Vector;
import traci.model.material.Color;

public class Solid extends Pigment
{
    protected Color color;
    
    public Solid(Color color)
    {
        this.color = color;
    }
    
    @Override
    public Color getColor(final Vector p)
    {
        return color;
    }
    
    public void setColor(final Color color)
    {
        this.color = color;
    }
    
    @Override
    public void transform(final Matrix mat, final Matrix invMat)
    {
    	// Nothing to be done
    }
    
    @Override
    public Object clone() throws CloneNotSupportedException
    {
        final Solid res = (Solid) super.clone();
        
        res.color = color;
        
        return res;
    }
}
