package traci.parser;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.helpers.DefaultHandler;

import traci.model.Scene;

public class XmlParser extends DefaultHandler
{
    private XmlParser()
    {
    }
    
    public static void main(final String[] args)
    {
        try
        {
            final File file = new File("scenes\\test2.xml");
            build(new BufferedInputStream(new FileInputStream(file)));
        }
        catch (final Exception e)
        {
            e.printStackTrace();
        }
    }
    
    public static Scene build(final InputStream is)
    {
        final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        
        try
        {
            final DocumentBuilder docBuilder = dbf.newDocumentBuilder();
            final Document doc = docBuilder.parse(is);
            return scene(doc.getDocumentElement());
        }
        catch (final Exception e)
        {
            e.printStackTrace();
            System.exit(-1);
        }
        
        return null;
    }
    
    private static Scene scene(final Element node)
    {
        assert "scene".equals(node.getNodeName());
        
        final Scene scene = new Scene();
        return null;//node.get
    }
}
