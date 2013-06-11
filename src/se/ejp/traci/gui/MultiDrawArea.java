package se.ejp.traci.gui;

import java.util.ArrayList;
import java.util.List;

import se.ejp.traci.main.Result;
import se.ejp.traci.model.Color;

public class MultiDrawArea extends AbstractDrawArea
{
    private final List<DrawArea> areas;

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
        final int size = areas.size();

        for (int i = 0; i < size; ++i)
        {
            areas.get(i).draw(x, y, color);
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
