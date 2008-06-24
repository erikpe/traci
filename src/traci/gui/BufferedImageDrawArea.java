package traci.gui;

import java.awt.image.BufferedImage;

import traci.model.material.Color;

public class BufferedImageDrawArea extends AbstractDrawArea implements DrawArea
{
    private final BufferedImage image;
    
    public BufferedImageDrawArea(final int width, final int height)
    {
        super(width, height);
        image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
    }
    
    @Override
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
        
        image.getRaster().setDataElements(x, y,
                image.getColorModel().getDataElements(intColor, null));
    }
    
    public BufferedImage getBufferedImage()
    {
        return image;
    }
}
