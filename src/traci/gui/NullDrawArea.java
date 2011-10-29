package traci.gui;

import traci.model.material.Color;

public class NullDrawArea extends AbstractDrawArea
{
    public NullDrawArea(final int width, final int height)
    {
        super(width, height);
    }

    @Override
    public void draw(final long x, final long y, final Color color)
    {
    	// Do nothing by default
    }
}
