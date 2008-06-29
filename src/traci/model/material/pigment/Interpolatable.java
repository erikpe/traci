package traci.model.material.pigment;

import traci.model.material.Color;

interface Interpolatable
{
    public long getWidth();
    
    public long getHeight();
    
    public Color getAt(final long x, final long y);
}
