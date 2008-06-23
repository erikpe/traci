package traci.parser;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

public class Tokenizer implements Iterable<Token>
{
    private final String str;
    
    private final Collection<Token> tokens;
    
    public Tokenizer(final String str)
    {
        this.str = str;
        this.tokens = new ArrayList<Token>();
        
        tokenize();
    }
    
    private void tokenize()
    {
        int idx = 0;
        
        while (idx < str.length())
        {
            final char c = str.charAt(idx);
            
            switch (c)
            {
            case ' ': case '\t': case '\n':
                ++idx;
                break;
                
            case '(':
                tokens.add(new Token(Token.Type.LPAR, "("));
                ++idx;
                break;
                
            case ')':
                tokens.add(new Token(Token.Type.RPAR, ")"));
                ++idx;
                break;
                
            case ';':
                idx = str.indexOf('\n', idx);
                if (idx < 0) idx = str.length();
                break;
                
            default:
                final int newIdx = indexOfAny(idx, " \t\n();");
                tokens.add(new Token(Token.Type.WORD, str.substring(idx, newIdx)));
                idx = newIdx;
                break;
            }
        }
        
        tokens.add(new Token(Token.Type.EOF, ""));
    }
    
    private int indexOfAny(int idx, final String searchStr)
    {
        while (idx < str.length())
        {
            if (searchStr.indexOf(str.charAt(idx)) > 0)
            {
                break;
            }
            
            ++idx;
        }
        
        return idx;
    }
    
    public Iterator<Token> iterator()
    {
        return tokens.iterator();
    }
}
