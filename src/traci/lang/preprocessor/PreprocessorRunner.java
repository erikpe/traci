package traci.lang.preprocessor;

import java.io.File;
import java.io.IOException;
import java.io.Reader;

import org.anarres.cpp.CppReader;
import org.anarres.cpp.Feature;
import org.anarres.cpp.Preprocessor;

import traci.main.Result;
import traci.main.options.Settings;
import traci.util.Log;
import traci.util.Utilities;

public class PreprocessorRunner
{
    private final Settings settings;
    private StringBuilder sb = null;

    public PreprocessorRunner(final Settings settings)
    {
        this.settings = settings;
    }

    public String getProcessedCode()
    {
        return sb.toString();
    }

    public Result run()
    {
        final String inputFilename = settings.getInputFilename();

        Log.INFO("Preprocessing input file: '" + inputFilename + "'");
        final long start = System.currentTimeMillis();

        sb = new StringBuilder();

        Preprocessor pp = null;
        final File inputFile = new File(inputFilename);

        try
        {
            pp = new Preprocessor(inputFile);
            pp.addFeature(Feature.LINEMARKERS);
            pp.addFeature(Feature.KEEPCOMMENTS);
        }
        catch (final IOException e)
        {
            Log.ERROR("Unable to open input file: '" + inputFilename + "':\n" + e.getMessage());
            return Result.IO_ERROR;
        }

        final Reader reader = new CppReader(pp);
        try
        {
            int c;
            while ((c = reader.read()) >= 0)
            {
                sb.append((char) c);
            }
            reader.close();
            pp.close();
        }
        catch (final IOException e)
        {
            Log.ERROR("Error while reading preprocessed file:\n" + e.getMessage());
            return Result.IO_ERROR;
        }

        final long stop = System.currentTimeMillis();
        Log.INFO("Preprocessing finished in " + Utilities.millisecondsToString(stop - start));

        return Result.SUCCESS;
    }
}
