package traci.model.material;

public class Material
{
    private Texture texture;
    
    private Material() { }
    
    public static Material newDefault()
    {
        final Material mat = new Material();
        
        mat.texture = new Texture();
        mat.getTexture().pigment = new Pigment();
        mat.getTexture().finish = new Finish();
        
        mat.setColor(Color.WHITE);
        
        mat.getFinish().setDiffCoeff(0.3);
        mat.getFinish().setSpecCoeff(0.3);
        mat.getFinish().setShininess(50);
        mat.getFinish().setReflectivness(0.3);
        
        return mat;
    }
    
    public Texture getTexture()
    {
        return texture;
    }
    
    public Finish getFinish()
    {
        return getTexture().getFinish();
    }
    
    public Pigment getPigment()
    {
        return getTexture().getPigment();
    }
    
    public Color getColor()
    {
        return getPigment().getColor();
    }
    
    public void setColor(final Color color)
    {
        getPigment().setColor(color);
    }
    
    @Override
    public Object clone()
    {
        try
        {
            final Material res = (Material) super.clone();
            res.texture = (Texture) texture.clone();
            
            return res;
        }
        catch (CloneNotSupportedException e)
        {
            e.printStackTrace();
        }
        
        return null;
    }
}
