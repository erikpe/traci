package traci.lang;

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeNodeStream;
import org.antlr.runtime.tree.Tree;

import traci.gui.DynamicJPanelDrawArea;
import traci.gui.MainWindow;
import traci.gui.MultiDrawArea;
import traci.gui.PngDrawArea;
import traci.lang.interpreter.Context;
import traci.lang.interpreter.Entities;
import traci.lang.interpreter.Entities.Entity;
import traci.lang.interpreter.node.BlockNode;
import traci.lang.parser.TraciLexer;
import traci.lang.parser.TraciParser;
import traci.lang.parser.TraciTreeWalker;
import traci.math.Vector;
import traci.model.Camera;
import traci.model.Scene;
import traci.model.light.PointLight;
import traci.model.material.Color;
import traci.model.shape.csg.Union;
import traci.render.Renderer;
import traci.render.Settings;

public class TestParser
{
    private static void prettyPrint(final Tree tree, final int indent, final StringBuilder sb)
    {
        sb.append('\n');
        for (int i = 0; i < indent; ++i)
        {
            sb.append("    ");
        }

        if (tree.getChildCount() > 0)
        {
            sb.append('(');
            sb.append(tree.getText());
            for (int i = 0; i < tree.getChildCount(); ++i)
            {
                prettyPrint(tree.getChild(i), indent + 1, sb);
            }
            sb.append(')');
        }
        else
        {
            sb.append(tree.getText());
        }
    }

    public static void main(final String[] args) throws Exception
    {
        final int width = 1600;
        final int height = 1200;
        final String filename = "out.png";

        final DynamicJPanelDrawArea visibleDrawArea = new DynamicJPanelDrawArea(width, height);
        final MainWindow window = new MainWindow(visibleDrawArea);
        window.setVisible(true);

        final PngDrawArea pngDrawArea = new PngDrawArea(width, height, filename);
        final MultiDrawArea multiDrawArea = new MultiDrawArea(pngDrawArea);
        multiDrawArea.add(visibleDrawArea);

        double start = System.currentTimeMillis();
        final ANTLRFileStream input = new ANTLRFileStream("src/traci/lang/input.txt");
        final TraciLexer lexer = new TraciLexer(input);
        final CommonTokenStream tokens = new CommonTokenStream(lexer);
        final TraciParser parser = new TraciParser(tokens);
        final CommonTree tree = (CommonTree) parser.scene().getTree();
        double stop = System.currentTimeMillis();

        System.out.println("Parsing: " + (stop - start) + " ms.");

        System.out.println(tree.toStringTree());
        final StringBuilder sb = new StringBuilder();
        //prettyPrint(tree, 0, sb);
        //System.out.println(sb.toString());

        start = System.currentTimeMillis();
        final CommonTreeNodeStream nodes = new CommonTreeNodeStream(tree);
        final TraciTreeWalker walker = new TraciTreeWalker(nodes);
        final BlockNode bn = walker.block();
        stop = System.currentTimeMillis();
        System.out.println("TreeWalker: " + (stop - start) + " ms.");

        start = System.currentTimeMillis();
        final Union rootUnion = new Union();
        final Entity entity = Entities.makeEntity(rootUnion);
        bn.eval(Context.newRootContext(entity));
        stop = System.currentTimeMillis();
        System.out.println("Eval: " + (stop - start) + " ms.");

        final PointLight light = new PointLight(Vector.make(2, 15, 30), Color.WHITE.mul(30*50));
        final PointLight light2 = new PointLight(Vector.make(-10, 10, 10), Color.WHITE.mul(150));

        final Vector camLocation = Vector.make(-10, 15, 15);
        final Vector camLookAt = Vector.make(8, 2, 0);
        final Camera cam = new Camera(camLocation, camLookAt, Vector.UNIT_Y);
        final Scene scene = new Scene(rootUnion, cam);
        scene.addLight(light);
        scene.addLight(light2);

        Renderer.renderScene(scene, new Settings(), multiDrawArea, 8);

    }
}
