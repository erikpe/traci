package se.ejp.traci.gui;

import se.ejp.traci.main.Result;
import se.ejp.traci.model.Color;

public interface DrawArea
{
    public void draw(final long x, final long y, final Color color);
    public Result start();
    public Result finish();
    public int width();
    public int height();
}
