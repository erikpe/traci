package traci.model.material;

import traci.model.material.pigment.Pigment;


public class Texture implements Cloneable
{
    protected Pigment pigment;
    protected Finish finish;
    
    public Pigment getPigment()
    {
        return pigment;
    }
    
    public Finish getFinish()
    {
        return finish;
    }
    
    public void setPigment(final Pigment pigment)
    {
        this.pigment = pigment;
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
