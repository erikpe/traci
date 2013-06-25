package se.ejp.traci.model.shape.primitive;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.smurn.jply.Element;
import org.smurn.jply.ElementReader;
import org.smurn.jply.PlyReader;
import org.smurn.jply.PlyReaderFile;
import org.smurn.jply.Property;
import org.smurn.jply.util.NormalMode;
import org.smurn.jply.util.NormalizingPlyReader;
import org.smurn.jply.util.TesselationMode;
import org.smurn.jply.util.TextureMode;

import se.ejp.traci.util.Log;

public class MeshReader
{
    public static class MeshData
    {
        public static final int VERTEX_SIZE = 6;
        public static final int TRIANGLE_SIZE = 3;
        public static final int BSP_NODE_SIZE = 2;
        public static final int BBOX_SIZE = 6;

        public int numVertices = 0;
        public int numTriangles = 0;
        public int numBspNode = 0;

        public double[] vertices = null;
        public int[] triangles = null;
        public int[] bspTree = null;
        public double[] bboxes = null;
    }

    private class BSPNode
    {
      private double xLow, xHigh;
      private double yLow, yHigh;
      private double zLow, zHigh;

      private int a, b;
    }

    private static final int MAX_TRIANGLES_IN_LEAF = 10;
    private static final Map<String, MeshData> cache = new HashMap<String, MeshData>();

    private final String filename;

    private List<BSPNode> bspNodes = null;
    private MeshData meshData;

    public MeshReader(final String filename)
    {
        this.filename = filename;
    }

    private void bspNodesToMesh()
    {
        meshData.bspTree = new int[MeshData.BSP_NODE_SIZE * bspNodes.size()];
        meshData.bboxes = new double[MeshData.BBOX_SIZE * bspNodes.size()];
        meshData.numBspNode = bspNodes.size();

        int treeIdx = -1;
        int bboxIdx = -1;

        for (final BSPNode node : bspNodes)
        {
            meshData.bspTree[++treeIdx] = node.a;
            meshData.bspTree[++treeIdx] = node.b;

            meshData.bboxes[++bboxIdx] = node.xLow;
            meshData.bboxes[++bboxIdx] = node.xHigh;
            meshData.bboxes[++bboxIdx] = node.yLow;
            meshData.bboxes[++bboxIdx] = node.yHigh;
            meshData.bboxes[++bboxIdx] = node.zLow;
            meshData.bboxes[++bboxIdx] = node.zHigh;
        }
    }

    private void readVertices(final ElementReader elementReader) throws IOException
    {
        final List<Double> vertices = new ArrayList<Double>();
        Element element;

        while ((element = elementReader.readElement()) != null)
        {
            vertices.add(element.getDouble("x"));
            vertices.add(element.getDouble("y"));
            vertices.add(element.getDouble("z"));
            vertices.add(element.getDouble("nx"));
            vertices.add(element.getDouble("ny"));
            vertices.add(element.getDouble("nz"));
        }

        meshData.vertices = new double[vertices.size()];
        meshData.numVertices = vertices.size() / MeshData.VERTEX_SIZE;

        for (int i = 0; i < vertices.size(); ++i)
        {
            meshData.vertices[i] = vertices.get(i);
        }
    }

    private void readTriangles(final ElementReader elementReader) throws IOException
    {
        final List<Property> properties = elementReader.getElementType().getProperties();
        String id = "vertex_indices";
        for (final Property property : properties)
        {
            if ("vertex_index".equals(property.getName()))
            {
                id = property.getName();
                break;
            }
        }

        final List<Integer> triangles = new ArrayList<Integer>();
        Element element;

        while ((element = elementReader.readElement()) != null)
        {
            final int[] indices = element.getIntList(id);
            triangles.add(indices[0]);
            triangles.add(indices[1]);
            triangles.add(indices[2]);
        }

        meshData.triangles = new int[triangles.size()];
        meshData.numTriangles = triangles.size() / MeshData.TRIANGLE_SIZE;

        for (int i = 0; i < triangles.size(); ++i)
        {
            meshData.triangles[i] = triangles.get(i);
        }
    }

    public MeshData read() throws IOException
    {
        if ((meshData = cache.get(filename)) != null)
        {
            return meshData;
        }

        Log.INFO("Loading mesh data from '" + filename + "'");

        meshData = new MeshData();
        cache.put(filename, meshData);

        PlyReader plyReader = new PlyReaderFile(filename);
        plyReader = new NormalizingPlyReader(plyReader, TesselationMode.TRIANGLES, NormalMode.ADD_NORMALS_CCW,
                TextureMode.PASS_THROUGH);

        ElementReader elementReader;
        while ((elementReader = plyReader.nextElementReader()) != null)
        {
            if ("vertex".equals(elementReader.getElementType().getName()))
            {
                readVertices(elementReader);
            }
            else if ("face".equals(elementReader.getElementType().getName()))
            {
                readTriangles(elementReader);
            }

            elementReader.close();
        }

        bspNodes = new ArrayList<BSPNode>();
        makeBspNode(0, meshData.numTriangles);
        bspNodesToMesh();
        bspNodes = null;

        return meshData;
    }

    private boolean rightOfPlane(final int triIdx, final int axis, final double splitPlane)
    {
        double val0, val1, val2;

        int idx = MeshData.TRIANGLE_SIZE * triIdx;
        val0 = meshData.vertices[MeshData.VERTEX_SIZE * meshData.triangles[idx]   + axis];
        val1 = meshData.vertices[MeshData.VERTEX_SIZE * meshData.triangles[++idx] + axis];
        val2 = meshData.vertices[MeshData.VERTEX_SIZE * meshData.triangles[++idx] + axis];

        val0 = val0 - splitPlane;
        val1 = val1 - splitPlane;
        val2 = val2 - splitPlane;

        final double valMin = min(val0, min(val1, val2));
        final double valMax = max(val0, max(val1, val2));

        return valMax > -valMin;
    }

    private int split(final int begin, final int end, final int axis, final double splitPlane)
    {
        int left = begin;
        int right = end - 1;

        final int size = MeshData.TRIANGLE_SIZE;
        final int[] tmpTriangle = new int[size];

        while (left < right)
        {
            if (!rightOfPlane(left, axis, splitPlane))
            {
                ++left;
            }
            else if (rightOfPlane(right, axis, splitPlane))
            {
                --right;
            }
            else
            {
                System.arraycopy(meshData.triangles, size * left,  tmpTriangle,        0,            size);
                System.arraycopy(meshData.triangles, size * right, meshData.triangles, size * left,  size);
                System.arraycopy(tmpTriangle,        0,            meshData.triangles, size * right, size);
            }
        }

        return left;
    }

    private int makeBspNode(final int begin, final int end)
    {
        final BSPNode node = new BSPNode();
        bspNodes.add(node);
        final int nodeIndex = bspNodes.size() - 1;

        makeBBox(node, begin, end);

        if (end - begin <= MAX_TRIANGLES_IN_LEAF)
        {
            node.a = begin;
            node.b = end;
            return nodeIndex;
        }

        final double xSize = node.xHigh - node.xLow;
        final double ySize = node.yHigh - node.yLow;
        final double zSize = node.zHigh - node.zLow;

        int splitIdx;

        if (xSize > ySize && xSize > zSize)
        {
            splitIdx = split(begin, end, 0, 0.5 * (node.xHigh + node.xLow));
        }
        else if (ySize > zSize)
        {
            splitIdx = split(begin, end, 1, 0.5 * (node.yHigh + node.yLow));
        }
        else
        {
            splitIdx = split(begin, end, 2, 0.5 * (node.zHigh + node.zLow));
        }

        node.a = -makeBspNode(begin, splitIdx);
        node.b = -makeBspNode(splitIdx, end);

        return nodeIndex;
    }

    private double min(final double d0, final double d1)
    {
        return d0 < d1 ? d0 : d1;
    }

    private double max(final double d0, final double d1)
    {
        return d0 > d1 ? d0 : d1;
    }

    private void makeBBox(final BSPNode node, final int begin, final int end)
    {
        node.xLow = Double.POSITIVE_INFINITY;
        node.xHigh = Double.NEGATIVE_INFINITY;
        node.yLow = Double.POSITIVE_INFINITY;
        node.yHigh = Double.NEGATIVE_INFINITY;
        node.zLow = Double.POSITIVE_INFINITY;
        node.zHigh = Double.NEGATIVE_INFINITY;

        for (int i = begin; i < end; ++i)
        {
            int idx = MeshData.TRIANGLE_SIZE * i;
            final int v0 = meshData.triangles[idx];
            final int v1 = meshData.triangles[++idx];
            final int v2 = meshData.triangles[++idx];

            idx = MeshData.VERTEX_SIZE * v0;
            final double v0_x = meshData.vertices[idx];
            final double v0_y = meshData.vertices[++idx];
            final double v0_z = meshData.vertices[++idx];

            idx = MeshData.VERTEX_SIZE * v1;
            final double v1_x = meshData.vertices[idx];
            final double v1_y = meshData.vertices[++idx];
            final double v1_z = meshData.vertices[++idx];

            idx = MeshData.VERTEX_SIZE * v2;
            final double v2_x = meshData.vertices[idx];
            final double v2_y = meshData.vertices[++idx];
            final double v2_z = meshData.vertices[++idx];

            node.xLow = min(node.xLow, min(v0_x, min(v1_x, v2_x)));
            node.xHigh = max(node.xHigh, max(v0_x, max(v1_x, v2_x)));
            node.yLow = min(node.yLow, min(v0_y, min(v1_y, v2_y)));
            node.yHigh = max(node.yHigh, max(v0_y, max(v1_y, v2_y)));
            node.zLow = min(node.zLow, min(v0_z, min(v1_z, v2_z)));
            node.zHigh = max(node.zHigh, max(v0_z, max(v1_z, v2_z)));
        }
    }
}
