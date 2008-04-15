package traci.model.texture;

public class Finish
{
    public final Color cAmb;
    public final double specCoeff;
    public final double diffCoeff;
    public final double shininess;
    
    public Finish(final Color cAmb, final double specCoeff,
                  final double diffCoeff, final double shininess)
    {
        this.cAmb = cAmb;
        this.specCoeff = specCoeff;
        this.diffCoeff = diffCoeff;
        this.shininess = shininess;
    }
}
