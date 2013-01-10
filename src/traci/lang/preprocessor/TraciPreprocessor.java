package traci.lang.preprocessor;

import java.io.File;
import java.io.IOException;
import java.io.Reader;

import org.anarres.cpp.CppReader;
import org.anarres.cpp.Feature;
import org.anarres.cpp.Preprocessor;

import traci.main.Settings;
import traci.util.Log;
import traci.util.Utilities;

public class TraciPreprocessor
{
    final Settings settings;

    public TraciPreprocessor(final Settings settings)
    {
        this.settings = settings;
    }

    public String run()
    {
        Log.INFO("Preprocessing input file: '" + settings.inputFilename + "'");
        final long start = System.currentTimeMillis();

        Preprocessor pp = null;
        final File inputFile = new File(settings.inputFilename);

        try
        {
            pp = new Preprocessor(inputFile);
            pp.addFeature(Feature.LINEMARKERS);
            pp.addFeature(Feature.KEEPCOMMENTS);
        }
        catch (final IOException e)
        {
            Log.ERROR("Unable to open input file: '" + settings.inputFilename + "':");
            Log.ERROR(e.getMessage());
            System.exit(-1);
        }

        final Reader reader = new CppReader(pp);
        final StringBuilder sb = new StringBuilder();

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
            Log.ERROR("Error while reading preprocessed file:");
            Log.ERROR(e.getMessage());
            System.exit(-1);
        }

        final long stop = System.currentTimeMillis();
        Log.INFO("Preprocessing finished in " + Utilities.millisecondsToString(stop - start));
        //System.out.println(sb.toString());
        return sb.toString();
    }
}
