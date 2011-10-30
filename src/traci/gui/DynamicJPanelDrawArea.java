package traci.gui;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.JPanel;

import traci.model.material.Color;

@SuppressWarnings("serial")
public class DynamicJPanelDrawArea extends JPanel implements DrawArea
{
    /**
     * Redraw period in milliseconds.
     */
    private static final long REDRAW_PERIOD_MS = 500;

    private final BufferedImageDrawArea area;

    private Timer redrawTimer = null;

    public DynamicJPanelDrawArea(final int width, final int height)
    {
        area = new BufferedImageDrawArea(width, height);
    }

    @Override
    public void draw(final long x, final long y, final Color color)
    {
        area.draw(x, y, color);
    }

    @Override
    public int width()
    {
        return area.width();
    }

    @Override
    public int height()
    {
        return area.height();
    }

    @Override
    public void start()
    {
        assert redrawTimer == null;

        area.start();

        final TimerTask task = new TimerTask()
        {
            @Override
            public void run()
            {
                repaint();
            }
        };

        redrawTimer = new Timer();
        redrawTimer.schedule(task, 0, REDRAW_PERIOD_MS);
    }

    @Override
    public void finish()
    {
        if (redrawTimer != null)
        {
            redrawTimer.cancel();
            redrawTimer = null;
        }

        area.finish();

        repaint();
    }

    @Override
    public void paintComponent(final Graphics g)
    {
        super.paintComponents(g);
        ((Graphics2D) g).drawImage(area.getBufferedImage(), null, 0, 0);
    }
}
