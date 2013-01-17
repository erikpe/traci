package traci.main.options;

import org.apache.commons.cli.CommandLine;

import traci.main.Result;

@SuppressWarnings("serial")
public abstract class FlagOption extends TraciOption
{
    public FlagOption(final Character shortOpt, final String longOpt, final String desc)
    {
        super(shortOpt, longOpt, desc, false, null);
    }

    @Override
    public Result handleOption(final CommandLine cmd)
    {
        return handleFlagOption();
    }

    public abstract Result handleFlagOption();
}
