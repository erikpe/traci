package se.ejp.traci.model.shape.primitive;

import java.io.IOException;

import org.junit.Test;

public class MeshReaderTest
{
     @Test
     public void testReadBunny() throws IOException
     {
         MeshReader.readFile("scenes/ply/bun_zipper_res4.ply");
     }
}
