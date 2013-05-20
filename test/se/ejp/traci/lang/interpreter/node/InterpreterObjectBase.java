package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;

import org.antlr.runtime.RecognitionException;

import se.ejp.traci.lang.interpreter.InterpreterBase;
import se.ejp.traci.lang.interpreter.TraciValue.Type;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArguments;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;

public class InterpreterObjectBase extends InterpreterBase
{
    private final Type type;

    protected InterpreterObjectBase(final Type type)
    {
        this.type = type;
    }

    protected static List<String> getSnippets(final String id, final String args, final String modifiers)
    {
        final List<String> snippets = new ArrayList<String>();

        if (args == null && modifiers == null)
        {
            snippets.add("return " + id + ";");
            snippets.add("return " + id + "();");
            snippets.add("return " + id + " { };");
            snippets.add("return " + id + "() { };");
            snippets.add("val = " + id + "; return val;");
            snippets.add("val = " + id + "(); return val;");
            snippets.add("val = " + id + " { }; return val;");
            snippets.add("val = " + id + "() { }; return val;");
            snippets.add("val = " + id + "; return val { };");
            snippets.add("val = " + id + "(); return val { };");
            snippets.add("val = " + id + " { }; return val { };");
            snippets.add("val = " + id + "() { }; return val { };");
        }
        else if (args != null && modifiers == null)
        {
            snippets.add("return " + id + "(" + args + ");");
            snippets.add("return " + id + "(" + args + ") { };");
            snippets.add("val = " + id + "(" + args + "); return val;");
            snippets.add("val = " + id + "(" + args + ") { }; return val;");
            snippets.add("val = " + id + "(" + args + "); return val { };");
            snippets.add("val = " + id + "(" + args + ") { }; return val { };");
        }
        else if (args == null && modifiers != null)
        {
            snippets.add("return " + id + " { " + modifiers + " };");
            snippets.add("return " + id + "() { " + modifiers + " };");
            snippets.add("val = " + id + " { " + modifiers + " }; return val;");
            snippets.add("val = " + id + "() { " + modifiers + " }; return val;");
            snippets.add("val = " + id + " { " + modifiers + " }; return val { };");
            snippets.add("val = " + id + "() { " + modifiers + " }; return val { };");
            snippets.add("val = " + id + "; return val { " + modifiers + " };");
            snippets.add("val = " + id + "(); return val { " + modifiers + " };");
        }
        else if (args != null && modifiers != null)
        {
            snippets.add("return " + id + "(" + args + ") { " + modifiers + " };");
            snippets.add("val = " + id + "(" + args + ") { " + modifiers + " }; return val;");
            snippets.add("val = " + id + "(" + args + ") { " + modifiers + " }; return val { };");
            snippets.add("val = " + id + "(" + args + "); return val { " + modifiers + " };");
        }

        return snippets;
    }

    @SuppressWarnings("unchecked")
    private <P> P run(final String code, final Class<P> clazz) throws RecognitionException, InterpreterRuntimeException
    {
        runInterpreter(code);
        assertEquals(type, value.getType());
        assertEquals(clazz, value.getObject().getClass());
        return (P) value.getObject();
    }

    private void runFail(final String code, final String id, final Class<? extends InterpreterRuntimeException> eClass)
            throws RecognitionException
    {
        try
        {
            runInterpreter(code);
            fail("Missing exception");
        }
        catch (final InterpreterIllegalArguments e)
        {
            assertEquals(eClass, e.getClass());
            assertEquals(id, e.function);
        }
        catch (final InterpreterRuntimeException e)
        {
            assertEquals(eClass, e.getClass());
        }
    }

    protected <P> List<P> runTests(final String id, final Class<P> clazz, final String args, final String modifiers)
            throws RecognitionException, InterpreterRuntimeException
    {
        final List<P> res = new ArrayList<P>();
        for (final String code : getSnippets(id, args, modifiers))
        {
            res.add(run(code, clazz));
        }
        return res;
    }

    protected void runTestsFail(final Class<? extends InterpreterRuntimeException> eClass, final String id,
            final String args, final String modifiers) throws RecognitionException
    {
        for (final String code : getSnippets(id, args, modifiers))
        {
            runFail(code, id, eClass);
        }
    }
}
