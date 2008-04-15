package traci.model.texture;

public class Texture
{
    public static final Texture DEFAULT;
    
    static
    {
        DEFAULT = new Texture();
        
        DEFAULT.color = Color.WHITE;
        DEFAULT.specularCoeff = 0.5;
        DEFAULT.diffuseCoeff = 0.5;
        DEFAULT.ambientCoeff = 0.1;
        DEFAULT.shininess = 100;
    }
    
    public Color color;
    
    public double specularCoeff;
    public double diffuseCoeff;
    public double ambientCoeff;
    public double shininess;
}
