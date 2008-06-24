package traci.gui;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.JPanel;

import traci.model.material.Color;

public class DynamicJPanelDrawArea extends JPanel implements DrawArea
{
    private static final long serialVersionUID = 7195706708596046785L;
    
    /**
     * Redraw period in milliseconds.
     */
    private static final long REDRAW_PERIOD = 250;
    
    private final BufferedImage image;
    
    private Timer redrawTimer = null;
    
    private final int width;
    private final int height;
    
    public DynamicJPanelDrawArea(final int width, final int height)
    {
        this.width = width;
        this.height = height;
        
        image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    }
    
    public void draw(final int x, final int y, final Color color)
    {
        int r = (int) (color.r * 255);
        int g = (int) (color.g * 255);
        int b = (int) (color.b * 255);
        
        r = (r > 255 ? 255 : r);
        g = (g > 255 ? 255 : g);
        b = (b > 255 ? 255 : b);
        
        r = (r < 0 ? 0 : r);
        g = (g < 0 ? 0 : g);
        b = (b < 0 ? 0 : b);
        
        final int intColor = 0xff000000 | (r << 16) | (g << 8) | b;
        
        //image.setRGB(x, y, intColor);
        image.getRaster().setDataElements(x, y,
                image.getColorModel().getDataElements(intColor, null));
    }
    
    public void start()
    {
        assert redrawTimer == null;
        
        final TimerTask task = new TimerTask()
        {
            public void run()
            {
                repaint();
            }
        };
        
        redrawTimer = new Timer();
        redrawTimer.schedule(task, 0, REDRAW_PERIOD);
    }
    
    public void finish()
    {
        if (redrawTimer != null)
        {
            redrawTimer.cancel();
            redrawTimer = null;
        }
        
        repaint();
    }
    
    public int width()
    {
        return width;
    }
    
    public int height()
    {
        return height;
    }
    
    public void paintComponent(final Graphics g)
    {
        super.paintComponents(g);
        ((Graphics2D) g).drawImage(image, null, 0, 0);
    }
}
