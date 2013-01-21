package traci.lang.preprocessor;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;

import org.anarres.cpp.CppReader;
import org.anarres.cpp.Feature;
import org.anarres.cpp.LexerException;
import org.anarres.cpp.Preprocessor;

import traci.main.Result;
import traci.main.options.Settings;
import traci.util.Log;
import traci.util.Utilities;

public class PreprocessorRunner
{
    private final Settings settings;
    private String code = null;

    public PreprocessorRunner(final Settings settings)
    {
        this.settings = settings;
    }

    public String getProcessedCode()
    {
        return code;
    }

    private Result addMacros(final Preprocessor pp)
    {
        for (final String macroStr : settings.getPreprocessorMacros())
        {
            final int separatorIdx = macroStr.indexOf('=');
            final String name;
            final String value;

            if (separatorIdx >= 0)
            {
                name = macroStr.substring(0, separatorIdx);
                value = macroStr.substring(separatorIdx + 1);
            }
            else
            {
                name = macroStr;
                value = "1";
            }

            try
            {
                pp.addMacro(name, value);
            }
            catch (final LexerException e)
            {
                Log.ERROR("Preprocessor error:\n" + e.getMessage());
                return Result.PREPROCESSOR_ERROR;
            }
        }

        return Result.SUCCESS;
    }

    private Result runPreprocessor(final Preprocessor pp)
    {
        final StringBuilder sb = new StringBuilder();
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
            Log.ERROR("Preprocessor error:\n" + e.getMessage());
            return Result.IO_ERROR;
        }

        if (pp.getListener().getErrors() > 0)
        {
            return Result.PREPROCESSOR_ERROR;
        }

        code = sb.toString();

        return Result.SUCCESS;
    }

    private Result saveOutput()
    {
        try
        {
            final FileWriter writer = new FileWriter(settings.getPreprocessorOutput());
            writer.write(code);
            writer.close();
            Log.INFO("Saved preprocessed code to: '" + settings.getPreprocessorOutput() + "'");
        }
        catch (final IOException e)
        {
            Log.ERROR("Unable to save preprocessor output to: '" + settings.getPreprocessorOutput() + "'");
            return Result.IO_ERROR;
        }

        return Result.SUCCESS;
    }

    public Result run()
    {
        final String inputFilename = settings.getInputFilename();

        Log.INFO("Preprocessing input file: '" + inputFilename + "'");
        final long start = System.currentTimeMillis();

        final Preprocessor pp = new Preprocessor();
        final ErrorHandler errorHandler = new ErrorHandler();

        pp.setListener(errorHandler);
        pp.addFeature(Feature.LINEMARKERS);
        pp.addFeature(Feature.KEEPCOMMENTS);

        for (final String includePath : settings.getIncludeDirs())
        {
            pp.getQuoteIncludePath().add(includePath);
        }

        Result result = addMacros(pp);
        if (result != Result.SUCCESS)
        {
            return result;
        }

        try
        {
            pp.addInput(new File(inputFilename));
        }
        catch (final IOException e)
        {
            Log.ERROR("Unable to open input file: '" + inputFilename + "':\n" + e.getMessage());
            return Result.IO_ERROR;
        }

        result = runPreprocessor(pp);
        if (result != Result.SUCCESS)
        {
            return result;
        }

        final long stop = System.currentTimeMillis();
        Log.INFO("Preprocessing finished in " + Utilities.millisecondsToString(stop - start));

        if (settings.getPreprocessorOutput() != null)
        {
            result = saveOutput();
            if (result != Result.SUCCESS)
            {
                return result;
            }
        }

        return Result.SUCCESS;
    }
}
