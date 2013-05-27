package se.ejp.traci.model.material.pigment;

import se.ejp.traci.model.Color;

interface Interpolatable
{
    public long getWidth();
    public long getHeight();
    public Color getAt(final long x, final long y);
}
