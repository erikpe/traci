package traci.gui;

import traci.main.Result;
import traci.model.material.Color;

public interface DrawArea
{
    public void draw(final long x, final long y, final Color color);
    public Result start();
    public Result finish();
    public int width();
    public int height();
}
