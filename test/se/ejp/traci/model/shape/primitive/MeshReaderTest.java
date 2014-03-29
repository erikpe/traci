package se.ejp.traci.model.shape.primitive;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;

import org.junit.Test;

import se.ejp.traci.model.shape.primitive.MeshReader.MeshData;

public class MeshReaderTest
{
    private void assertBboxWithinBbox(final MeshData data, final int parentNode,
            final int childNode)
    {
        assertNotEquals(parentNode, childNode);

        int idx = MeshData.BBOX_SIZE * parentNode;
        final double parentXLow = data.bboxes[idx];
        final double parentXHigh = data.bboxes[++idx];
        final double parentYLow = data.bboxes[++idx];
        final double parentYHigh = data.bboxes[++idx];
        final double parentZLow = data.bboxes[++idx];
        final double parentZHigh = data.bboxes[++idx];

        idx = MeshData.BBOX_SIZE * childNode;
        final double childXLow = data.bboxes[idx];
        final double childXHigh = data.bboxes[++idx];
        final double childYLow = data.bboxes[++idx];
        final double childYHigh = data.bboxes[++idx];
        final double childZLow = data.bboxes[++idx];
        final double childZHigh = data.bboxes[++idx];

        assertTrue(parentXLow <= childXLow);
        assertTrue(parentXHigh >= childXHigh);
        assertTrue(parentYLow <= childYLow);
        assertTrue(parentYHigh >= childYHigh);
        assertTrue(parentZLow <= childZLow);
        assertTrue(parentZHigh >= childZHigh);
    }

    private void assertTriangleWithinBbox(final MeshData data, final int node,
            final int triangle)
    {
        int idx = MeshData.BBOX_SIZE * node;
        final double bboxXLow = data.bboxes[idx];
        final double bboxXHigh = data.bboxes[++idx];
        final double bboxYLow = data.bboxes[++idx];
        final double bboxYHigh = data.bboxes[++idx];
        final double bboxZLow = data.bboxes[++idx];
        final double bboxZHigh = data.bboxes[++idx];

        for (int i = 0; i < 3; ++i)
        {
            final int vec = data.triangles[i + MeshData.TRIANGLE_SIZE * triangle];
            assertTrue(vec < data.numVertices);

            idx = MeshData.VERTEX_SIZE * vec;
            final double x = data.vertices[idx];
            final double y = data.vertices[++idx];
            final double z = data.vertices[++idx];

            assertTrue(bboxXLow <= x);
            assertTrue(bboxXHigh >= x);
            assertTrue(bboxYLow <= y);
            assertTrue(bboxYHigh >= y);
            assertTrue(bboxZLow <= z);
            assertTrue(bboxZHigh >= z);
        }
    }

    private void assertMeshDataNode(final MeshData data, final int node,
            final int[] triangleUsage, final int[] bspNodeUsage)
    {
        bspNodeUsage[node]++;

        int idx = MeshData.BSP_NODE_SIZE * node;
        final int a = data.bspTree[idx];
        final int b = data.bspTree[++idx];

        if (a < 0)
        {
            assertTrue(b < 0);
            assertBboxWithinBbox(data, node, -a);
            assertBboxWithinBbox(data, node, -b);
            assertMeshDataNode(data, -a, triangleUsage, bspNodeUsage);
            assertMeshDataNode(data, -b, triangleUsage, bspNodeUsage);
        }
        else
        {
            assertTrue(b >= 0);
            for (int i = a; i < b; ++i)
            {
                triangleUsage[i]++;
                assertTriangleWithinBbox(data, node, i);
            }
        }
    }

    private void assertMeshData(final MeshData data)
    {
        assertEquals(MeshData.VERTEX_SIZE * data.numVertices, data.vertices.length);
        assertEquals(MeshData.TRIANGLE_SIZE * data.numTriangles, data.triangles.length);
        assertEquals(MeshData.BSP_NODE_SIZE * data.numBspNode, data.bspTree.length);
        assertEquals(MeshData.BBOX_SIZE * data.numBspNode, data.bboxes.length);

        final int[] triangleUsage = new int[data.numTriangles];
        final int[] bspNodeUsage = new int[data.numBspNode];

        assertMeshDataNode(data, 0, triangleUsage, bspNodeUsage);

        for (int i = 0; i < data.numTriangles; ++i)
        {
            assertEquals(1, triangleUsage[i]);
        }

        for (int i = 0; i < data.numBspNode; ++i)
        {
            assertEquals(1, bspNodeUsage[i]);
        }
    }

    @Test
    public void testBunny() throws IOException
    {
        final MeshData data = MeshReader.readFile("testcode/bun_zipper_res4.ply");
        assertEquals(453, data.numVertices);
        assertEquals(948, data.numTriangles);
        assertMeshData(data);
    }

    @Test
    public void testIcosahedron() throws IOException
    {
        final MeshData data = MeshReader.readFile("testcode/icosahedron.ply");
        assertEquals(12, data.numVertices);
        assertEquals(20, data.numTriangles);
        assertMeshData(data);
    }

    @Test
    public void testTetrahedron() throws IOException
    {
        final MeshData data = MeshReader.readFile("testcode/tetrahedron.ply");
        assertEquals(4, data.numVertices);
        assertEquals(4, data.numTriangles);
        assertMeshData(data);
    }
}
