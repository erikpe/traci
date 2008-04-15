package traci.parser;

public class Token
{
    public static enum Type { LPAR, RPAR, WORD, EOF };
    
    public final Type type;
    
    public final String str;
    
    public Token(final Type type, final String str)
    {
        this.type = type;
        this.str = str;
    }
}
