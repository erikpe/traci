package se.ejp.traci.model;

import se.ejp.traci.lang.interpreter.exceptions.InterpreterRuntimeException;
import se.ejp.traci.math.Projection2D;
import se.ejp.traci.math.Transformable;
import se.ejp.traci.math.Transformation;
import se.ejp.traci.math.Transformations;
import se.ejp.traci.math.Vector;
import se.ejp.traci.model.material.pigment.FileImage;
import se.ejp.traci.model.material.pigment.Pigment;
import se.ejp.traci.model.material.pigment.RepeatPolicy;

public class Skybox implements Transformable
{
    private Transformation transformation;
    private Pigment forward, right, back, left, up, down;

    private Skybox(final Pigment forward, final Pigment right, final Pigment back, final Pigment left,
            final Pigment up, final Pigment down)
    {
        this.transformation = Transformations.identity();

        this.forward = forward;
        this.right = right;
        this.back = back;
        this.left = left;
        this.up = up;
        this.down = down;
    }

    public static Skybox make(final Pigment forward, final Pigment right, final Pigment back, final Pigment left,
            final Pigment up, final Pigment down)
    {
        return new Skybox(forward, right, back, left, up, down);
    }

    public static Skybox make(final String forwardImg, final String rightImg, final String backImg,
            final String leftImg, final String upImg, final String downImg) throws InterpreterRuntimeException
    {
        Pigment forward = FileImage.make(forwardImg, RepeatPolicy.STRETCH.id, Projection2D.XY_PLANE.id);
        Pigment right = FileImage.make(rightImg, RepeatPolicy.STRETCH.id, Projection2D.ZY_PLANE.id);
        Pigment back = FileImage.make(backImg, RepeatPolicy.STRETCH.id, Projection2D.XY_PLANE.id);
        Pigment left = FileImage.make(leftImg, RepeatPolicy.STRETCH.id, Projection2D.ZY_PLANE.id);
        Pigment up = FileImage.make(upImg, RepeatPolicy.STRETCH.id, Projection2D.XZ_PLANE.id);
        Pigment down = FileImage.make(downImg, RepeatPolicy.STRETCH.id, Projection2D.XZ_PLANE.id);

        forward = forward.transform(Transformations.translate(-0.5, -0.5, 0.0));
        right = right.transform(Transformations.translate(0.0, -0.5, -0.5));
        back = back.transform(Transformations.translate(-0.5, -0.5, 0.0));
        back = back.transform(Transformations.scalex(-1.0));
        left = left.transform(Transformations.translate(0.0, -0.5, -0.5));
        left = left.transform(Transformations.scalez(-1.0));
        up = up.transform(Transformations.translate(-0.5, 0.0, -0.5));
        down = down.transform(Transformations.translate(-0.5, 0.0, -0.5));
        down = down.transform(Transformations.scalez(-1.0));

        return make(forward, right, back, left, up, down);
    }

    @Override
    public void transform(final Transformation tr)
    {
        transformation = transformation.compose(tr);

        forward = forward.transform(tr);
        right = right.transform(tr);
        back = back.transform(tr);
        left = left.transform(tr);
        up = up.transform(tr);
        down = down.transform(tr);
    }

    public Color sample(Vector dir)
    {
        dir = transformation.dirInv(dir);

        final double absX = Math.abs(dir.x());
        final double absY = Math.abs(dir.y());
        final double absZ = Math.abs(dir.z());

        final Pigment pigment;
        final Vector hit;

        if (absX >= absY && absX >= absZ)
        {
            pigment = (dir.x() > 0.0 ? right : left);
            hit = dir.div(absX * 2.0);
        }
        else if (absY >= absX && absY >= absZ)
        {
            pigment = (dir.y() > 0.0 ? up : down);
            hit = dir.div(absY * 2.0);
        }
        else
        {
            pigment = (dir.z() > 0.0 ? back : forward);
            hit = dir.div(absZ * 2.0);
        }

        return pigment.getColor(hit);
    }
}
