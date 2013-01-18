package traci.main.options;

import org.apache.commons.cli.CommandLine;

import traci.main.Result;

@SuppressWarnings("serial")
public abstract class FlagOption extends TraciOption
{
    public FlagOption(final Character shortOpt, final String longOpt, final String desc)
    {
        super(shortOpt, longOpt, desc, null);
    }

    @Override
    public Result checkOption(final CommandLine cmd)
    {
        if (cmd.hasOption(optName))
        {
            return handleOption();
        }

        return Result.SUCCESS;
    }

    public abstract Result handleOption();
}
