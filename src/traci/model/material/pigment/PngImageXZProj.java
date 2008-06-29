package traci.model.material.pigment;

import traci.math.Vector;
import traci.model.material.Color;

public class PngImageXZProj extends PngImage
{
    public PngImageXZProj(final String filename)
    {
        super(filename, Repeater.STRETCH);
    }
    
    @Override
    protected Color getColorTransformed(final Vector p)
    {
        return getSample(p.x, p.z);
    }
}
