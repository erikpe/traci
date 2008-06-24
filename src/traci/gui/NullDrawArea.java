package traci.gui;

import traci.model.material.Color;

public class NullDrawArea extends AbstractDrawArea implements DrawArea
{
    public NullDrawArea(final int width, final int height)
    {
        super(width, height);
    }
    
    @Override
    public void draw(final int x, final int y, final Color color)
    {
    }
}
