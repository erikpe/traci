package traci.gui;

import traci.model.material.Color;

public interface DrawArea
{
    public void draw(final int x, final int y, final Color color);
    
    public void start();
    
    public void finish();
    
    public int width();
    
    public int height();
}
