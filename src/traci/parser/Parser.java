package traci.parser;

import java.util.Iterator;

public class Parser
{
    private final Iterator<Token> tokens;
    
    private Token sym = null;
    private Token prevSym = null;
    
    private Parser(final Tokenizer tokenizer)
    {
        this.tokens = tokenizer.iterator();
    }
    
    public static Node parse(final Tokenizer tokenizer) throws ParseError
    {
        return new Parser(tokenizer).run();
    }
    
    private Node run() throws ParseError
    {
        final Node node;
        
        getSym();
        if (accept(Token.Type.LPAR) || accept(Token.Type.WORD))
        {
            node = node();
            expect(Token.Type.EOF);
            return node;
        }
        
        throw new ParseError("Error in parsing.");
    }
    
    private void getSym()
    {
        prevSym = sym;
        sym = tokens.next();
    }
    
    private boolean accept(final Token.Type type)
    {
        if (type == sym.type)
        {
            getSym();
            return true;
        }
        
        return false;
    }
    
    private void expect(final Token.Type type) throws ParseError
    {
        if (accept(type))
        {
            return;
        }
        
        throw new ParseError("Excepted: " + type + ", got: " + sym.type);
    }
    
    private Node node() throws ParseError
    {
        if (prevSym.type == Token.Type.LPAR)
        {
            expect(Token.Type.WORD);
            final Node node = new Node(prevSym.str, false);
            
            while (accept(Token.Type.WORD) || accept(Token.Type.LPAR))
            {
                //node.addChild(node());
            }
            
            expect(Token.Type.RPAR);
            
            return node;
        }
        else if (prevSym.type == Token.Type.RPAR)
        {
            expect(Token.Type.WORD);
            return new Node(prevSym.str, true);
        }
        
        throw new ParseError("Error in parsing.");
    }
}
