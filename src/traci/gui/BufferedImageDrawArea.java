package traci.gui;

import java.awt.image.BufferedImage;

import traci.model.material.Color;

public class BufferedImageDrawArea extends AbstractDrawArea
{
    private final BufferedImage image;
    
    public BufferedImageDrawArea(final int width, final int height)
    {
        super(width, height);
        image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    }
    
    @Override
    public void draw(final long x, final long y, final Color color)
    {
        long r = (long) (color.r() * 255);
        long g = (long) (color.g() * 255);
        long b = (long) (color.b() * 255);
        
        r = (r > 255 ? 255 : r);
        g = (g > 255 ? 255 : g);
        b = (b > 255 ? 255 : b);
        
        r = (r < 0 ? 0 : r);
        g = (g < 0 ? 0 : g);
        b = (b < 0 ? 0 : b);
        
        final int intColor = 0xff000000
                | (int) (r << 16)
                | (int) (g << 8)
                | (int) b;
        
        image.getRaster().setDataElements((int) x, (int) y,
                image.getColorModel().getDataElements(intColor, null));
    }
    
    public BufferedImage getBufferedImage()
    {
        return image;
    }
}
