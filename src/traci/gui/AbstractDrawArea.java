package traci.gui;

public abstract class AbstractDrawArea implements DrawArea
{
    private final int width;
    
    private final int height;
    
    public AbstractDrawArea(final int width, final int height)
    {
        this.width = width;
        this.height = height;
    }
    
    @Override
    public int width()
    {
        return width;
    }
    
    @Override
    public int height()
    {
        return height;
    }
    
    @Override
    public void start()
    {
    }
    
    @Override
    public void finish()
    {
    }
}
