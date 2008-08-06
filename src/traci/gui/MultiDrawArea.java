package traci.gui;

import java.util.ArrayList;
import java.util.Collection;

import traci.model.material.Color;

public class MultiDrawArea extends AbstractDrawArea implements DrawArea
{
    private final Collection<DrawArea> areas;
    
    public MultiDrawArea(final DrawArea area)
    {
        this(area.width(), area.height());
        areas.add(area);
    }
    
    public MultiDrawArea(final int width, final int height)
    {
        super(width, height);
        this.areas = new ArrayList<DrawArea>();
    }
    
    public void add(final DrawArea area)
    {
        assert width() == area.width();
        assert height() == area.height();
        
        areas.add(area);
    }
    
    @Override
    public void draw(final long x, final long y, final Color color)
    {
        for (final DrawArea area : areas)
        {
            area.draw(x, y, color);
        }
    }
    
    @Override
    public void start()
    {
        for (final DrawArea area : areas)
        {
            area.start();
        }
    }
    
    @Override
    public void finish()
    {
        for (final DrawArea area : areas)
        {
            area.finish();
        }
    }
}
