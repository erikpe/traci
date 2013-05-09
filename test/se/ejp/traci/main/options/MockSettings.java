package se.ejp.traci.main.options;

import java.util.ArrayList;

public class MockSettings extends Settings
{
    public MockSettings()
    {
        this.includeDirs = new ArrayList<String>();
        this.preprocessorMacros = new ArrayList<String>();
    }

    public void setInputFilename(final String inputFilename)
    {
        this.inputFilename = inputFilename;
    }
}
