package traci.gui;

import traci.model.material.Color;

public interface DrawArea
{
    public void draw(final long x, final long y, final Color color);
    public void start();
    public void finish();
    public int width();
    public int height();
}
