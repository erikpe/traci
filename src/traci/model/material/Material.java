package traci.model.material;

import traci.model.material.pigment.Pigment;
import traci.model.material.pigment.Solid;

public class Material
{
    private Texture texture;
    
    private Material() { }
    
    public static Material newDefault()
    {
        final Material mat = new Material();
        
        mat.texture = new Texture();
        mat.getTexture().finish = new Finish();
        mat.setPigment(new Solid(Color.WHITE));
        
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
    
    public void setPigment(final Pigment pigment)
    {
        getTexture().setPigment(pigment);
    }
    
//    public Color getColor()
//    {
//        return getPigment().getColor();
//    }
    
    public void setColor(final Color color)
    {
        setPigment(new Solid(color));
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
