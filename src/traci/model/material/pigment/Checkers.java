package traci.model.material.pigment;

import traci.math.Vector;
import traci.model.material.Color;

public class Checkers extends Pattern
{
    protected Color color1 = Color.BLACK;
    
    protected Color color2 = Color.WHITE;
    
    public Checkers() { }
    
    @Override
    public Color getColorTransformed(final Vector p)
    {
        if ((Math.round(p.x) + Math.round(p.y) + Math.round(p.z)) % 2 == 0)
        {
            return color1;
        }
        else
        {
            return color2;
        }
    }
}
