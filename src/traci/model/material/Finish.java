package traci.model.material;

public class Finish implements Cloneable
{
    protected double specCoeff;
    protected double diffCoeff;
    protected double shininess;
    protected double reflectiveness;
    
    public double getSpecCoeff()
    {
        return specCoeff;
    }
    
    public void setSpecCoeff(final double specCoeff)
    {
        this.specCoeff = specCoeff;
    }
    
    public double getDiffCoeff()
    {
        return diffCoeff;
    }
    
    public void setDiffCoeff(final double diffCoeff)
    {
        this.diffCoeff = diffCoeff;
    }
    
    public double getShininess()
    {
        return shininess;
    }
    
    public void setShininess(final double shininess)
    {
        this.shininess = shininess;
    }
    
    public double getReflectivness()
    {
        return reflectiveness;
    }
    
    public void setReflectivness(final double reflectivness)
    {
        this.reflectiveness = reflectivness;
    }
    
    @Override
    protected Object clone() throws CloneNotSupportedException
    {
        final Finish res = (Finish) super.clone();
        
        res.specCoeff = specCoeff;
        res.diffCoeff = diffCoeff;
        res.shininess = shininess;
        
        return res;
    }
}
