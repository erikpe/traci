package se.ejp.traci.model.shape.primitive;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import se.ejp.traci.lang.interpreter.exceptions.InterpreterIOException;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.shape.primitive.MeshReader.MeshData;
import se.ejp.traci.render.Ray;
import se.ejp.traci.util.ComparablePair;

public class Mesh extends Primitive
{
    private final MeshData meshData;

    private Mesh(final MeshData meshData)
    {
        this.meshData = meshData;
    }

    public static Mesh make(final String filename) throws InterpreterIOException
    {
        try
        {
            return new Mesh(new MeshReader(filename).read());
        }
        catch (final IOException e)
        {
            throw new InterpreterIOException(null, null, "Unable to read file: '" + filename + "'", e);
        }
    }

    @Override
    protected Vector primitiveGetNormalAt(final Vector p)
    {
        throw new UnsupportedOperationException("Plane object should always have precalculated normal");
    }

    @Override
    protected Ray primitiveShootRay(final Vector p, final Vector dir)
    {
        final List<ComparablePair<Double, Vector>> points = new ArrayList<ComparablePair<Double, Vector>>(2);

        shoot(p, dir, 0, points);
        Collections.sort(points);

        return null;
    }

    private void shoot(final Vector p, final Vector dir, final int node, final List<ComparablePair<Double, Vector>> points)
    {
        if (!bboxHit(p, dir, node))
        {
            return;
        }

        int idx = MeshData.BSP_NODE_SIZE * node;
        final int a = meshData.bspTree[idx];
        final int b = meshData.bspTree[++idx];

        if (a < 0)
        {
            shoot(p, dir, -a, points);
            shoot(p, dir, -b, points);
        }
        else
        {
            intersect(p, dir, a, b, points);
        }
    }

    private boolean bboxHit(final Vector p, final Vector dir, final int node)
    {
        int idx = MeshData.BBOX_SIZE * node;

        final double xLow = meshData.bboxes[idx];
        final double xHigh = meshData.bboxes[++idx];
        final double px = p.x();
        final double dirx = dir.x();

        final double x0 = (xLow - px) / dirx;
        final double x1 = (xHigh - px) / dirx;

        double near = min(x0, x1);
        double far = max(x0, x1);

        if (far < 0.0)
        {
            return false;
        }

        final double yLow = meshData.bboxes[++idx];
        final double yHigh = meshData.bboxes[++idx];
        final double py = p.y();
        final double diry = dir.y();

        final double y0 = (yLow - py) / diry;
        final double y1 = (yHigh - py) / diry;

        near = max(near, min(y0, y1));
        far = min(far, max(y0, y1));

        if (far < 0.0 || far < near)
        {
            return false;
        }

        final double zLow = meshData.bboxes[++idx];
        final double zHigh = meshData.bboxes[++idx];
        final double pz = p.z();
        final double dirz = dir.z();

        final double z0 = (zLow - pz) / dirz;
        final double z1 = (zHigh - pz) / dirz;

        near = max(near, min(z0, z1));
        far = min(far, max(z0, z1));

        if (far < 0.0 || far < near)
        {
            return false;
        }

        return true;
    }

    private void intersect(final Vector O, final Vector D, final int begin, final int end, final List<ComparablePair<Double, Vector>> points)
    {
        for (int i = begin; i < end; ++i)
        {
            int idx = MeshData.TRIANGLE_SIZE * i;
            final int v0_idx = meshData.triangles[idx];
            final int v1_idx = meshData.triangles[++idx];
            final int v2_idx = meshData.triangles[++idx];

            idx = MeshData.VERTEX_SIZE * v0_idx;
            double x = meshData.vertices[idx];
            double y = meshData.vertices[++idx];
            double z = meshData.vertices[++idx];
            final Vector V0 = Vector.make(x, y, z);

            idx = MeshData.VERTEX_SIZE * v1_idx;
            x = meshData.vertices[idx];
            y = meshData.vertices[++idx];
            z = meshData.vertices[++idx];
            final Vector V1 = Vector.make(x, y, z);

            idx = MeshData.VERTEX_SIZE * v2_idx;
            x = meshData.vertices[idx];
            y = meshData.vertices[++idx];
            z = meshData.vertices[++idx];
            final Vector V2 = Vector.make(x, y, z);

            // Find vectors for two edges sharing V0
            final Vector e1 = V1.sub(V0);
            final Vector e2 = V2.sub(V0);

            // Begin calculating determinant - also used to calculate u parameter
            final Vector P = D.cross(e2);
            final double det = e1.dot(P);
            final double inv_det = 1.0 / det;

            // Calculate distance from V0 to ray origin
            final Vector T = O.sub(V0);

            // Calculate u parameter and test bound
            final double u = T.dot(P) * inv_det;

            // The intersection lies outside of the triangle
            if (u < 0.0 || u > 1.0)
            {
                continue;
            }

            // Prepare to test v parameter
            final Vector Q = T.cross(e1);

            // Calculate V parameter and test bound
            final double v = D.dot(Q) * inv_det;

            // The intersection lies outside of the triangle
            if (v < 0.0 || u + v > 1.0)
            {
                continue;
            }

            final double t = e2.dot(Q) * inv_det;

            points.add(ComparablePair.make(t, Vector.UNIT_Y));
        }
    }
}
