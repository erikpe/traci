package traci.model.material;

public class Texture implements Cloneable
{
    protected Pigment pigment;
    protected Finish finish;
    
    protected Texture() { }
    
    public Pigment getPigment()
    {
        return pigment;
    }
    
    public Finish getFinish()
    {
        return finish;
    }
    
    @Override
    protected Object clone() throws CloneNotSupportedException
    {
        Texture res = (Texture) super.clone();
        
        res.pigment = (Pigment) pigment.clone();
        res.finish = (Finish) finish.clone();
        
        return res;
        
//        Texture res = null;
//        
//        try
//        {
//            res = (Texture) super.clone();
//            
//            res.pigment = (Pigment) res.pigment.clone();
//            res.finish = (Finish) res.finish.clone();
//        }
//        catch (final CloneNotSupportedException e)
//        {
//            e.printStackTrace();
//        }
//        
//        return res;
    }
}
