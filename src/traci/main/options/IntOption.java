package traci.main.options;

import org.apache.commons.cli.CommandLine;

import traci.main.Result;
import traci.util.Log;

@SuppressWarnings("serial")
public abstract class IntOption extends TraciOption
{
    public IntOption(final Character shortOpt, final String longOpt, final String desc, final String argName)
    {
        super(shortOpt, longOpt, desc, true, argName);
    }

    @Override
    public Result handleOption(final CommandLine cmd)
    {
        final String strValue = cmd.getOptionValue(optName);

        final long value;
        try
        {
            value = Long.parseLong(strValue);
        }
        catch (final NumberFormatException e)
        {
            Log.ERROR("Argument to option '" + getBothNames() + "' must be integer\n" + Options.getHelp());
            return Result.INVALID_ARGUMENT_ERROR;
        }

        return handleIntOption(value);
    }

    public abstract Result handleIntOption(final long value);
}
