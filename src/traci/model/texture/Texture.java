package traci.model.texture;

public class Texture implements Cloneable
{
    private Pigment pigment;
    private Finish finish;
    
    private Texture() { }
    
    public static Texture newDefault()
    {
        final Texture defTexture = new Texture();
        
        defTexture.pigment = new Pigment();
        defTexture.finish = new Finish();
        
        defTexture.getPigment().setColor(Color.WHITE);
        defTexture.getFinish().setCAmb(Color.make(0.1, 0.1, 0.1));
        defTexture.getFinish().setDiffCoeff(0.5);
        defTexture.getFinish().setSpecCoeff(0.5);
        defTexture.getFinish().setShininess(50);
        
        return defTexture;
    }
    
    public Pigment getPigment()
    {
        return pigment;
    }
    
    public Finish getFinish()
    {
        return finish;
    }
    
    @Override
    public Object clone()
    {
        Texture res = null;
        
        try
        {
            res = (Texture) super.clone();
            
            res.pigment = (Pigment) res.pigment.clone();
            res.finish = (Finish) res.finish.clone();
        }
        catch (CloneNotSupportedException e)
        {
            e.printStackTrace();
        }
        
        return res;
    }
}
