package se.ejp.traci.lang.interpreter.node;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import se.ejp.traci.lang.interpreter.Context;
import se.ejp.traci.lang.interpreter.TraciValue;
import se.ejp.traci.lang.interpreter.exceptions.FunctionReturnException;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterIllegalArgumentType;
import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.lang.interpreter.functions.FunctionSet;
import se.ejp.traci.lang.parser.IncludeLocation.FileLocation;
import se.ejp.traci.lang.parser.TraciToken;
import se.ejp.traci.model.Scene;

public class ForNodeTest
{
    private Context context;
    private TraciToken dummyToken;

    @Before
    public void setUp()
    {
        final Scene scene = new Scene();
        context = Context.newRootContext(scene);
        
        // Create a dummy token for testing
        final FileLocation fileLocation = new FileLocation("test.traci", 1, 1);
        final List<FileLocation> emptyIncludeStack = Collections.emptyList();
        dummyToken = new TraciToken(null, 0, 0, 0, 0, fileLocation, emptyIncludeStack);
    }

    @Test
    public void testBasicForLoop_PositiveRange() throws Exception
    {
        // Setup: for (i in 1 .. 3) { ... }
        final TraciNode startNode = new ConstNode(new TraciValue(1.0));
        final TraciNode endNode = new ConstNode(new TraciValue(3.0));
        
        final List<Double> collectedValues = new ArrayList<Double>();
        final BlockNode block = new BlockNode(new FunctionSet(null));
        block.addStatement(new TraciNode()
        {
            @Override
            public TraciValue eval(Context ctx) throws FunctionReturnException, InterpreterRuntimeException
            {
                try
                {
                    final TraciValue counterValue = ctx.getValue("i");
                    collectedValues.add(counterValue.getNumber());
                }
                catch (CloneNotSupportedException e)
                {
                    throw new RuntimeException(e);
                }
                return null;
            }
        });

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        // Execute
        final TraciValue result = forNode.eval(context);
        
        // Verify
        assertNull("ForNode should return null", result);
        assertEquals("Should iterate 3 times", 3, collectedValues.size());
        assertEquals(1.0, collectedValues.get(0), 0.001);
        assertEquals(2.0, collectedValues.get(1), 0.001);
        assertEquals(3.0, collectedValues.get(2), 0.001);
    }

    @Test
    public void testForLoop_SingleIteration() throws Exception
    {
        // Setup: for (i in 5 .. 5) { ... }
        final TraciNode startNode = new ConstNode(new TraciValue(5.0));
        final TraciNode endNode = new ConstNode(new TraciValue(5.0));
        
        final List<Double> collectedValues = new ArrayList<Double>();
        final BlockNode block = new BlockNode(new FunctionSet(null));
        block.addStatement(new TraciNode()
        {
            @Override
            public TraciValue eval(Context ctx) throws FunctionReturnException, InterpreterRuntimeException
            {
                try
                {
                    final TraciValue counterValue = ctx.getValue("i");
                    collectedValues.add(counterValue.getNumber());
                }
                catch (CloneNotSupportedException e)
                {
                    throw new RuntimeException(e);
                }
                return null;
            }
        });

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        // Execute
        forNode.eval(context);
        
        // Verify
        assertEquals("Should iterate once", 1, collectedValues.size());
        assertEquals(5.0, collectedValues.get(0), 0.001);
    }

    @Test
    public void testForLoop_NoIterations() throws Exception
    {
        // Setup: for (i in 10 .. 5) { ... }
        final TraciNode startNode = new ConstNode(new TraciValue(10.0));
        final TraciNode endNode = new ConstNode(new TraciValue(5.0));
        
        final List<Double> collectedValues = new ArrayList<Double>();
        final BlockNode block = new BlockNode(new FunctionSet(null));
        block.addStatement(new TraciNode()
        {
            @Override
            public TraciValue eval(Context ctx) throws FunctionReturnException, InterpreterRuntimeException
            {
                try
                {
                    final TraciValue counterValue = ctx.getValue("i");
                    collectedValues.add(counterValue.getNumber());
                }
                catch (CloneNotSupportedException e)
                {
                    throw new RuntimeException(e);
                }
                return null;
            }
        });

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        // Execute
        forNode.eval(context);
        
        // Verify
        assertEquals("Should not iterate when start > end", 0, collectedValues.size());
    }

    @Test
    public void testForLoop_ZeroRange() throws Exception
    {
        // Setup: for (i in 0 .. 0) { ... }
        final TraciNode startNode = new ConstNode(new TraciValue(0.0));
        final TraciNode endNode = new ConstNode(new TraciValue(0.0));
        
        final List<Double> collectedValues = new ArrayList<Double>();
        final BlockNode block = new BlockNode(new FunctionSet(null));
        block.addStatement(new TraciNode()
        {
            @Override
            public TraciValue eval(Context ctx) throws FunctionReturnException, InterpreterRuntimeException
            {
                try
                {
                    final TraciValue counterValue = ctx.getValue("i");
                    collectedValues.add(counterValue.getNumber());
                }
                catch (CloneNotSupportedException e)
                {
                    throw new RuntimeException(e);
                }
                return null;
            }
        });

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        // Execute
        forNode.eval(context);
        
        // Verify
        assertEquals("Should iterate once with zero", 1, collectedValues.size());
        assertEquals(0.0, collectedValues.get(0), 0.001);
    }

    @Test
    public void testForLoop_NegativeRange() throws Exception
    {
        // Setup: for (i in -3 .. -1) { ... }
        final TraciNode startNode = new ConstNode(new TraciValue(-3.0));
        final TraciNode endNode = new ConstNode(new TraciValue(-1.0));
        
        final List<Double> collectedValues = new ArrayList<Double>();
        final BlockNode block = new BlockNode(new FunctionSet(null));
        block.addStatement(new TraciNode()
        {
            @Override
            public TraciValue eval(Context ctx) throws FunctionReturnException, InterpreterRuntimeException
            {
                try
                {
                    final TraciValue counterValue = ctx.getValue("i");
                    collectedValues.add(counterValue.getNumber());
                }
                catch (CloneNotSupportedException e)
                {
                    throw new RuntimeException(e);
                }
                return null;
            }
        });

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        // Execute
        forNode.eval(context);
        
        // Verify
        assertEquals("Should iterate 3 times", 3, collectedValues.size());
        assertEquals(-3.0, collectedValues.get(0), 0.001);
        assertEquals(-2.0, collectedValues.get(1), 0.001);
        assertEquals(-1.0, collectedValues.get(2), 0.001);
    }

    @Test
    public void testForLoop_CounterVariableAccessible() throws Exception
    {
        // Setup: for (counter in 1 .. 2) { ... }
        final TraciNode startNode = new ConstNode(new TraciValue(1.0));
        final TraciNode endNode = new ConstNode(new TraciValue(2.0));
        
        final BlockNode block = new BlockNode(new FunctionSet(null));
        block.addStatement(new TraciNode()
        {
            @Override
            public TraciValue eval(Context ctx) throws FunctionReturnException, InterpreterRuntimeException
            {
                try
                {
                    final TraciValue counterValue = ctx.getValue("counter");
                    if (counterValue == null)
                    {
                        fail("Counter variable should be accessible in block");
                    }
                    assertEquals("Counter should be a number", TraciValue.Type.NUMBER, counterValue.getType());
                }
                catch (CloneNotSupportedException e)
                {
                    throw new RuntimeException(e);
                }
                return null;
            }
        });

        final ForNode forNode = new ForNode("counter", startNode, endNode, block, dummyToken);
        
        // Execute
        forNode.eval(context);
    }

    @Test
    public void testForLoop_EmptyBlock() throws Exception
    {
        // Setup: for (i in 1 .. 3) { }
        final TraciNode startNode = new ConstNode(new TraciValue(1.0));
        final TraciNode endNode = new ConstNode(new TraciValue(3.0));
        final BlockNode block = new BlockNode(new FunctionSet(null));

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        // Execute - should not throw
        final TraciValue result = forNode.eval(context);
        
        // Verify
        assertNull("ForNode should return null even with empty block", result);
    }

    @Test
    public void testForLoop_NonNumericStartValue_ThrowsException()
    {
        // Setup: for (i in "string" .. 3) { ... }
        final TraciNode startNode = new ConstNode(new TraciValue("not a number"));
        final TraciNode endNode = new ConstNode(new TraciValue(3.0));
        final BlockNode block = new BlockNode(new FunctionSet(null));

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        try
        {
            forNode.eval(context);
            fail("Should throw InterpreterIllegalArgumentType for non-numeric start value");
        }
        catch (InterpreterIllegalArgumentType e)
        {
            // Expected
            assertEquals("for-statement", e.function);
            assertEquals(TraciValue.Type.NUMBER, e.expectedArgType.iterator().next());
            assertEquals(TraciValue.Type.STRING, e.gotArgType);
            assertEquals(1, e.argIndex);
        }
        catch (Exception e)
        {
            fail("Should throw InterpreterIllegalArgumentType, not " + e.getClass().getName());
        }
    }

    @Test
    public void testForLoop_NonNumericEndValue_ThrowsException()
    {
        // Setup: for (i in 1 .. "string") { ... }
        final TraciNode startNode = new ConstNode(new TraciValue(1.0));
        final TraciNode endNode = new ConstNode(new TraciValue("not a number"));
        final BlockNode block = new BlockNode(new FunctionSet(null));

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        try
        {
            forNode.eval(context);
            fail("Should throw InterpreterIllegalArgumentType for non-numeric end value");
        }
        catch (InterpreterIllegalArgumentType e)
        {
            // Expected
            assertEquals("for-statement", e.function);
            assertEquals(TraciValue.Type.NUMBER, e.expectedArgType.iterator().next());
            assertEquals(TraciValue.Type.STRING, e.gotArgType);
            assertEquals(2, e.argIndex);
        }
        catch (Exception e)
        {
            fail("Should throw InterpreterIllegalArgumentType, not " + e.getClass().getName());
        }
    }

    @Test
    public void testForLoop_BooleanStartValue_ThrowsException()
    {
        // Setup: for (i in true .. 3) { ... }
        final TraciNode startNode = new ConstNode(new TraciValue(true));
        final TraciNode endNode = new ConstNode(new TraciValue(3.0));
        final BlockNode block = new BlockNode(new FunctionSet(null));

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        try
        {
            forNode.eval(context);
            fail("Should throw InterpreterIllegalArgumentType for boolean start value");
        }
        catch (InterpreterIllegalArgumentType e)
        {
            // Expected
            assertEquals("for-statement", e.function);
            assertEquals(TraciValue.Type.NUMBER, e.expectedArgType.iterator().next());
            assertEquals(TraciValue.Type.BOOLEAN, e.gotArgType);
            assertEquals(1, e.argIndex);
        }
        catch (Exception e)
        {
            fail("Should throw InterpreterIllegalArgumentType, not " + e.getClass().getName());
        }
    }

    @Test
    public void testForLoop_BooleanEndValue_ThrowsException()
    {
        // Setup: for (i in 1 .. false) { ... }
        final TraciNode startNode = new ConstNode(new TraciValue(1.0));
        final TraciNode endNode = new ConstNode(new TraciValue(false));
        final BlockNode block = new BlockNode(new FunctionSet(null));

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        try
        {
            forNode.eval(context);
            fail("Should throw InterpreterIllegalArgumentType for boolean end value");
        }
        catch (InterpreterIllegalArgumentType e)
        {
            // Expected
            assertEquals("for-statement", e.function);
            assertEquals(TraciValue.Type.NUMBER, e.expectedArgType.iterator().next());
            assertEquals(TraciValue.Type.BOOLEAN, e.gotArgType);
            assertEquals(2, e.argIndex);
        }
        catch (Exception e)
        {
            fail("Should throw InterpreterIllegalArgumentType, not " + e.getClass().getName());
        }
    }

    @Test
    public void testForLoop_LargeRange() throws Exception
    {
        // Setup: for (i in 1 .. 100) { ... }
        final TraciNode startNode = new ConstNode(new TraciValue(1.0));
        final TraciNode endNode = new ConstNode(new TraciValue(100.0));
        
        final List<Double> collectedValues = new ArrayList<Double>();
        final BlockNode block = new BlockNode(new FunctionSet(null));
        block.addStatement(new TraciNode()
        {
            @Override
            public TraciValue eval(Context ctx) throws FunctionReturnException, InterpreterRuntimeException
            {
                try
                {
                    final TraciValue counterValue = ctx.getValue("i");
                    collectedValues.add(counterValue.getNumber());
                }
                catch (CloneNotSupportedException e)
                {
                    throw new RuntimeException(e);
                }
                return null;
            }
        });

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        // Execute
        forNode.eval(context);
        
        // Verify
        assertEquals("Should iterate 100 times", 100, collectedValues.size());
        assertEquals(1.0, collectedValues.get(0), 0.001);
        assertEquals(50.0, collectedValues.get(49), 0.001);
        assertEquals(100.0, collectedValues.get(99), 0.001);
    }

    @Test
    public void testForLoop_FractionalBounds() throws Exception
    {
        // Setup: for (i in 1.5 .. 3.7) { ... }
        // Should iterate for i = 1.5, 2.5, 3.5 (stops at 3.5 since 4.5 > 3.7)
        final TraciNode startNode = new ConstNode(new TraciValue(1.5));
        final TraciNode endNode = new ConstNode(new TraciValue(3.7));
        
        final List<Double> collectedValues = new ArrayList<Double>();
        final BlockNode block = new BlockNode(new FunctionSet(null));
        block.addStatement(new TraciNode()
        {
            @Override
            public TraciValue eval(Context ctx) throws FunctionReturnException, InterpreterRuntimeException
            {
                try
                {
                    final TraciValue counterValue = ctx.getValue("i");
                    collectedValues.add(counterValue.getNumber());
                }
                catch (CloneNotSupportedException e)
                {
                    throw new RuntimeException(e);
                }
                return null;
            }
        });

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        // Execute
        forNode.eval(context);
        
        // Verify
        assertEquals("Should iterate 3 times", 3, collectedValues.size());
        assertEquals(1.5, collectedValues.get(0), 0.001);
        assertEquals(2.5, collectedValues.get(1), 0.001);
        assertEquals(3.5, collectedValues.get(2), 0.001);
    }

    @Test
    public void testForLoop_CounterAccessibleAfterLoop() throws Exception
    {
        // Setup: for (i in 1 .. 2) { ... }
        // In Traci, loop counter remains in local scope after the loop
        final TraciNode startNode = new ConstNode(new TraciValue(1.0));
        final TraciNode endNode = new ConstNode(new TraciValue(2.0));
        final BlockNode block = new BlockNode(new FunctionSet(null));

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        // Execute
        forNode.eval(context);
        
        // Verify counter is accessible after the loop and has the final value
        try
        {
            final TraciValue counterAfterLoop = context.getValue("i");
            assertEquals("Counter variable should be accessible after loop ends with final iteration value", 
                         2.0, counterAfterLoop.getNumber(), 0.001);
        }
        catch (CloneNotSupportedException e)
        {
            fail("Should not throw exception when checking counter variable");
        }
    }

    @Test
    public void testForLoop_NestedVariableAccess() throws Exception
    {
        // Setup: Set a global variable, access it inside the loop
        context.putGlobalValue("globalVar", new TraciValue(42.0));
        
        final TraciNode startNode = new ConstNode(new TraciValue(1.0));
        final TraciNode endNode = new ConstNode(new TraciValue(1.0));
        
        final List<Double> globalValues = new ArrayList<Double>();
        final BlockNode block = new BlockNode(new FunctionSet(null));
        block.addStatement(new TraciNode()
        {
            @Override
            public TraciValue eval(Context ctx) throws FunctionReturnException, InterpreterRuntimeException
            {
                try
                {
                    final TraciValue globalValue = ctx.getValue("globalVar");
                    globalValues.add(globalValue.getNumber());
                }
                catch (CloneNotSupportedException e)
                {
                    throw new RuntimeException(e);
                }
                return null;
            }
        });

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        // Execute
        forNode.eval(context);
        
        // Verify
        assertEquals("Should access global variable", 1, globalValues.size());
        assertEquals(42.0, globalValues.get(0), 0.001);
    }

    @Test
    public void testForLoop_PropagatesFunctionReturn() throws Exception
    {
        // Setup: for (i in 1 .. 3) { return 99; }
        final TraciNode startNode = new ConstNode(new TraciValue(1.0));
        final TraciNode endNode = new ConstNode(new TraciValue(3.0));
        
        final BlockNode block = new BlockNode(new FunctionSet(null));
        block.addStatement(new TraciNode()
        {
            @Override
            public TraciValue eval(Context ctx) throws FunctionReturnException, InterpreterRuntimeException
            {
                throw new FunctionReturnException(new TraciValue(99.0));
            }
        });

        final ForNode forNode = new ForNode("i", startNode, endNode, block, dummyToken);
        
        try
        {
            forNode.eval(context);
            fail("Should propagate FunctionReturnException");
        }
        catch (FunctionReturnException e)
        {
            // Expected
            assertEquals(99.0, e.value.getNumber(), 0.001);
        }
        catch (Exception e)
        {
            fail("Should propagate FunctionReturnException, not " + e.getClass().getName());
        }
    }
}
