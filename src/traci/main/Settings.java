package traci.main;

public class Settings
{
    public boolean debug;

    public String inputFilename;
    public String outputFilename;
    public boolean display;

    public int numThreads;

    public int width;
    public int height;

    public boolean focalBlurEnabled;
    public int focalBlurSamples;

    public boolean antialiasEnabled;
    public int aaLevel;

    public int fov;

    public int workBlockWidth;
    public int workBlockHeight;

    private Settings() { }

    public static Settings getDefault()
    {
        final Settings settings = new Settings();

        settings.debug = false;

        settings.inputFilename = null;
        settings.outputFilename = null;
        settings.display = false;

        settings.numThreads = Runtime.getRuntime().availableProcessors();

        settings.width = 800;
        settings.height = 600;

        settings.focalBlurEnabled = false;
        settings.focalBlurSamples = 1;

        settings.antialiasEnabled = false;
        settings.aaLevel = 0;

        settings.fov = 40;

        settings.workBlockWidth = 16;
        settings.workBlockHeight = 16;

        return settings;
    }
}
