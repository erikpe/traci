package traci.gui;

import java.util.ArrayList;
import java.util.Collection;

import traci.main.Result;
import traci.model.material.Color;

public class MultiDrawArea extends AbstractDrawArea
{
    private final Collection<DrawArea> areas;

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
    public Result start()
    {
        for (final DrawArea area : areas)
        {
            final Result result = area.start();

            if (result != Result.SUCCESS)
            {
                return result;
            }
        }

        return Result.SUCCESS;
    }

    @Override
    public Result finish()
    {
        for (final DrawArea area : areas)
        {
            final Result result = area.finish();

            if (result != Result.SUCCESS)
            {
                return result;
            }
        }

        return Result.SUCCESS;
    }
}
