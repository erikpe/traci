package traci.parser;

public class BuildScene
{
//    public static Scene run(final Node node) throws ParseError
//    {
//        assert node.id.equals("scene");
//        assert !node.isLeaf;
//        
//        Camera camera = null;
//        final Union rootUnion = new Union();
//        final Collection<PointLight> lights = new ArrayList<PointLight>();
//        
//        for (final Node child : node)
//        {
//            if (Shape.isShape(child.id))
//            {
//                //rootUnion.add(readShape(child, (Texture) rootUnion.texture.clone()));
//            }
//            else if (Camera.isCamera(child.id))
//            {
//                assert camera == null; // Only one camera per scene
//                //camera = readCamera(child);
//            }
//            else if (PointLight.isLight(child.id))
//            {
//                //lights.add(readLight(child));
//            }
//            else
//            {
//                throw new ParseError("kuken!");
//            }
//        }
//        
//        assert camera != null;
//        
//        final Scene scene = new Scene(rootUnion, camera);
//        
//        for (final PointLight light : lights)
//        {
//            scene.addLight(light);
//        }
//        
//        return scene;
//    }
//    
//    private static Shape readShape(final Node node, final Texture parentMaterial)
//    {
//        final Shape shape;
//        
//        final Texture material = readMaterial(node, parentMaterial);
//        
//        if (Csg.isCsg(node.id))
//        {
//            return readCsg(node, material);
//        }
//        else
//        {
//            assert Primitive.isPrimitive(node.id);
//            return readPrimitive(node, material);
//        }
//        
//        
//    }
//    
//    private static Shape readCsg(final Node node, final Texture parentMaterial)
//    {
//        final Csg csg;
//        
//        final Texture material = readMaterial(node, parentMaterial);
//        
//        if (node.id.equals("union"))
//        {
//            csg = new Union(material);
//        }
//        else if (node.id.equals("intersection"))
//        {
//            csg = new Intersection(material);
//        }
//        else
//        {
//            assert node.id.equals("difference");
//            csg = new Difference(parentMaterial);
//        }
//        
//        for (final Node child : node)
//        {
//            if (Shape.isShape(child.id))
//            {
//                csg.add(readShape(child, material));
//            }
//        }
//        
//        return csg;
//    }
//    
//    private static Primitive readPrimitive(final Node node, final Texture parentMaterial)
//    {
//        final Primitive primitive;
//        
//        final Texture material = readMaterial(node, parentMaterial);
//        
//        if (node.id.equals("box"))
//        {
//            primitive = new Box(parentMaterial);
//        }
//        else if (node.id.equals("cylinder"))
//        {
//            primitive = new Cylinder(parentMaterial);
//        }
//        else if (node.id.equals("plane"))
//        {
//            primitive = new Plane(parentMaterial);
//        }
//        else if (node.id.equals("sphere"))
//        {
//            primitive = new Sphere();
//        }
//        else
//        {
//            assert node.id.equals("torus");
//            primitive = new Torus(Double.parseDouble(node.get(0).id));
//        }
//        return null;
//        //primitive.
//    }
//    
//    private static Texture readMaterial(final Node node, final Texture parentMaterial)
//    {
//        final Texture newMaterial = (Texture) parentMaterial;//.clone();
//        
//        if (node.id.equals("color"))
//        {
//            newMaterial.getPigment().setColor(readColor(node));
//        }
//        
//        return newMaterial;
//    }
//    
//    private static Color readColor(final Node node)
//    {
//        assert node.id.equals("color");
//        assert !node.isLeaf;
//        assert node.size() == 3;
//        
//        double r = Double.parseDouble(node.get(0).id);
//        double g = Double.parseDouble(node.get(0).id);
//        double b = Double.parseDouble(node.get(0).id);
//        
//        return Color.make(r, g, b);
//    }
}
