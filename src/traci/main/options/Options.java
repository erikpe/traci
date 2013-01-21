package traci.main.options;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.MissingArgumentException;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;
import org.apache.commons.cli.UnrecognizedOptionException;

import traci.main.Result;
import traci.util.Log;

public class Options
{
    private static final String PROGNAME = "traci";
    private final org.apache.commons.cli.Options allOptions;
    private Settings settings;

    public Options()
    {
        this.allOptions = new org.apache.commons.cli.Options();
        initialize();
    }

    public Settings getSettings()
    {
        return settings;
    }

    public Result parse(final String[] argv)
    {
        settings = new Settings();

        final CommandLineParser parser = new PosixParser();
        final CommandLine cmd;

        try
        {
            cmd = parser.parse(allOptions, argv);
        }
        catch (final MissingArgumentException e)
        {
            final TraciOption option = (TraciOption) e.getOption();
            Log.ERROR("Missing argument for option '" + option.getBothNames() + "'\n" + getHelp());
            return Result.INVALID_ARGUMENT_ERROR;
        }
        catch (final UnrecognizedOptionException e)
        {
            Log.ERROR("Unrecognized option '" + e.getOption() + "'\n" + getHelp());
            return Result.INVALID_ARGUMENT_ERROR;
        }
        catch (final ParseException e)
        {
            Log.ERROR("Option error: " + e.getMessage() + "\n" + getHelp());
            return Result.INVALID_ARGUMENT_ERROR;
        }

        return readCmdLine(cmd, settings);
    }

    private Result readCmdLine(final CommandLine cmd, final Settings settings)
    {
        for (final Object o : allOptions.getOptions())
        {
            final TraciOption option = (TraciOption) o;
            final Result result = option.checkOption(cmd);

            if (result != Result.SUCCESS)
            {
                return result;
            }
        }

        final String[] args = cmd.getArgs();

        if (args.length == 0)
        {
            Log.ERROR("No input file given\n" + getHelp());
            return Result.INVALID_ARGUMENT_ERROR;
        }
        else if (args.length > 1)
        {
            Log.ERROR("More than one input file specified\n" + getHelp());
            return Result.INVALID_ARGUMENT_ERROR;
        }

        settings.inputFilename = args[0];

        return Result.SUCCESS;
    }

    private int sortingIdx = 0;
    private void addOption(final TraciOption option)
    {
        option.sortingIdx = sortingIdx++;
        allOptions.addOption(option);
    }

    @SuppressWarnings("serial")
    private void initialize()
    {
        addOption(new FlagOption(null, "help", "show help")
        {
            @Override
            public Result handleOption(final boolean flagSet)
            {
                if (flagSet)
                {
                    final HelpFormatter formatter = new HelpFormatter();
                    formatter.setOptionComparator(TraciOption.COMPARATOR);
                    formatter.printHelp(120, "traci [options] <input file>", "Options:", allOptions, null, false);
                    return Result.ABORT;
                }

                return Result.SUCCESS;
            }
        });

        addOption(new IntOption('w', "width", "image width in pixels", "SIZE", 800)
        {
            @Override
            public Result handleOption(final int value, final boolean userSupplied)
            {
                settings.width = value;
                return Result.SUCCESS;
            }
        });

        addOption(new IntOption('h', "height", "image height in pixels", "SIZE", 600)
        {
            @Override
            public Result handleOption(final int value, final boolean userSupplied)
            {
                settings.height = value;
                return Result.SUCCESS;
            }
        });

        addOption(new IntOption('a', "aa-level", "level of antialiasing (0: no antialiasing, 3: a lot)", "LEVEL", 0)
        {
            @Override
            public Result handleOption(final int value, final boolean userSupplied)
            {
                settings.aaLevel = value;
                settings.aaEnabled = (value > 0);
                return Result.SUCCESS;
            }
        });

        addOption(new StringOption('o', "output", "output file", "FILE", null)
        {
            @Override
            public Result handleOption(final String value, final boolean userSupplied)
            {
                settings.outputFilename = value;
                return Result.SUCCESS;
            }
        });

        addOption(new FlagOption('d', "display", "display image during rendering")
        {
            @Override
            public Result handleOption(final boolean flagSet)
            {
                settings.display = flagSet;
                return Result.SUCCESS;
            }
        });

        addOption(new FloatOption(null, "fov", "field of view in degrees", "DEGREES", 40.0)
        {
            @Override
            public Result handleOption(final double value, final boolean userSupplied)
            {
                settings.fov = (int) value;
                return Result.SUCCESS;
            }
        });

        addOption(new IntOption(null, "focal-blur-samples", "number of samples per pixel for focal blur", "NUM", 0)
        {
            @Override
            public Result handleOption(final int value, final boolean userSupplied)
            {
                settings.focalBlurSamples = value;
                settings.focalBlurEnabled = (value > 0);
                return Result.SUCCESS;
            }
        });

        addOption(new IntOption(null, "workblock-width", "workblock width in pixels", "SIZE", 16)
        {
            @Override
            public Result handleOption(final int value, final boolean userSupplied)
            {
                settings.workBlockWidth = value;
                return Result.SUCCESS;
            }
        });

        addOption(new IntOption(null, "workblock-height", "workblock height in pixels", "SIZE", 16)
        {
            @Override
            public Result handleOption(final int value, final boolean userSupplied)
            {
                settings.workBlockHeight = value;
                return Result.SUCCESS;
            }
        });

        addOption(new FlagOption(null, "debug", "enable debug messages")
        {
            @Override
            public Result handleOption(final boolean flagSet)
            {
                settings.debug = flagSet;
                return Result.SUCCESS;
            }
        });

        addOption(new IntOption(null, "threads", "number of worker threads", "NUM", 0)
        {
            @Override
            public Result handleOption(final int value, final boolean userSupplied)
            {
                settings.numThreads = (value > 0 ? value : Runtime.getRuntime().availableProcessors());
                return Result.SUCCESS;
            }
        });

        addOption(new MultipleStringOption('D', null, "define macro for preprocessor", "NAME=VALUE", null)
        {
            @Override
            public Result handleOption(final String[] values, final boolean userSupplied)
            {
                settings.preprocessorMacros = new ArrayList<String>(Arrays.asList(values));
                settings.preprocessorMacros = Collections.unmodifiableList(settings.preprocessorMacros);
                return Result.SUCCESS;
            }
        });

        addOption(new MultipleStringOption('I', null, "directory to search in for includes", "DIR", null)
        {
            @Override
            public Result handleOption(final String[] values, final boolean userSupplied)
            {
                settings.includeDirs = new ArrayList<String>(Arrays.asList(values));
                settings.includeDirs = Collections.unmodifiableList(settings.includeDirs);
                return Result.SUCCESS;
            }
        });

        addOption(new StringOption(null, "preprocessor-output", "save preprocessed code to file", "FILE", null)
        {
            @Override
            public Result handleOption(final String value, final boolean userSupplied)
            {
                settings.preprocessorOutput = value;
                return Result.SUCCESS;
            }
        });
    }

    static String getHelp()
    {
        return "run '" + PROGNAME + " --help' for help";
    }
}
