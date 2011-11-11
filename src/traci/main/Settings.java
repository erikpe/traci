package traci.main;

public class Settings
{
    public boolean debug = false;

    public String inputFilename = null;
    public String outputFilename = null;
    public boolean display = false;

    public int numThreads = Runtime.getRuntime().availableProcessors();

    public int width = 800;
    public int height = 600;

    public boolean focalBlurEnabled = false;
    public int focalBlurSamples = 1;

    public boolean antialiasEnabled = false;
    public int aaLevel = 0;

    public int fov = 40;

    public int workBlockWidth = 32;
    public int workBlockHeight = 32;
}
