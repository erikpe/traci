package traci.main.options;

import org.apache.commons.cli.CommandLine;

import traci.main.Result;
import traci.util.Log;

@SuppressWarnings("serial")
public abstract class FloatOption extends TraciOption
{
    public FloatOption(final Character shortOpt, final String longOpt, final String desc, final String argName)
    {
        super(shortOpt, longOpt, desc, true, argName);
    }

    @Override
    public Result handleOption(final CommandLine cmd)
    {
        final String strValue = cmd.getOptionValue(optName);

        final double value;
        try
        {
            value = Double.parseDouble(strValue);
        }
        catch (final NumberFormatException e)
        {
            Log.ERROR("Argument to option '" + getBothNames() + "' must be number\n" + Options.getHelp());
            return Result.INVALID_ARGUMENT_ERROR;
        }

        return handleFloatOption(value);
    }

    public abstract Result handleFloatOption(final double value);
}
