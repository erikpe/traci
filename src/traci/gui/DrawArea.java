package traci.gui;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;

import javax.swing.JPanel;

import traci.model.material.Color;

public class DrawArea extends JPanel
{
    private static final long serialVersionUID = 7195706708596046785L;
    
    private long lastRedraw = 0;
    
    private final BufferedImage image;
    
    public final int width;
    public final int height;
    
    public DrawArea(final int width, final int height)
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
        
        draw(x, y, intColor);
    }
    
    public void draw(final int x, final int y, final int color)
    {
        image.setRGB(x, y, color);
        
        if (System.currentTimeMillis() > lastRedraw + 250)
        {
            repaint();
            lastRedraw = System.currentTimeMillis();
        }
    }
    
    public void finish()
    {
        repaint();
    }
    
    public void paintComponent(final Graphics g)
    {
        super.paintComponents(g);
        ((Graphics2D) g).drawImage(image, null, 0, 0);
    }
}
