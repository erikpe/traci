package traci.main.options;

import java.util.List;

public class Settings
{
    Boolean debug = null;

    String inputFilename = null;
    String outputFilename = null;
    Boolean display = null;

    Integer numThreads = null;

    Integer width = null;
    Integer height = null;

    Boolean focalBlurEnabled = null;
    Integer focalBlurSamples = null;

    Boolean aaEnabled = null;
    Integer aaLevel = null;

    Integer fov = null;

    Integer workBlockWidth = null;
    Integer workBlockHeight = null;

    List<String> preprocessorMacros = null;
    List<String> includeDirs = null;

    String preprocessorOutput = null;

    Settings() { }

    public int getWorkBlockWidth()
    {
        return workBlockWidth;
    }

    public int getWorkBlockHeight()
    {
        return workBlockHeight;
    }

    public int getNumThreads()
    {
        return numThreads;
    }

    public boolean getDebug()
    {
        return debug;
    }

    public int getAaLevel()
    {
        return aaLevel;
    }

    public int getFocalBlurSamples()
    {
        return focalBlurSamples;
    }

    public boolean getFocalBlurEnabled()
    {
        return focalBlurEnabled;
    }

    public boolean getDisplay()
    {
        return display;
    }

    public String getOutputFilename()
    {
        return outputFilename;
    }

    public int getWidth()
    {
        return width;
    }

    public int getHeight()
    {
        return height;
    }

    public String getInputFilename()
    {
        return inputFilename;
    }

    public long getFov()
    {
        return fov;
    }

    public List<String> getPreprocessorMacros()
    {
        return preprocessorMacros;
    }

    public String getPreprocessorOutput()
    {
        return preprocessorOutput;
    }

    public List<String> getIncludeDirs()
    {
        return includeDirs;
    }
}
