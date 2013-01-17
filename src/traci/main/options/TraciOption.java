package traci.main.options;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.Option;

import traci.main.Result;

@SuppressWarnings("serial")
public abstract class TraciOption extends Option
{
    final protected String optName;

    public String getBothNames()
    {
        if (getOpt() == null)
        {
            return "--" + getLongOpt();
        }
        else if (getLongOpt() == null)
        {
            return "-" + getOpt();
        }
        return "-" + getOpt() + "/--" + getLongOpt();
    }

    public TraciOption(final Character shortOpt, final String longOpt, final String desc, final String argName)
    {
        super(shortOpt == null ? null : String.valueOf(shortOpt), longOpt, argName != null, desc);

        if (argName != null)
        {
            setArgName(argName);
        }

        if (getOpt() == null)
        {
            this.optName = getLongOpt();
        }
        else
        {
            this.optName = getOpt();
        }
    }

    public Result checkOption(final CommandLine cmd)
    {
        if (cmd.hasOption(optName))
        {
            return handleOption(cmd);
        }

        return Result.SUCCESS;
    }

    public abstract Result handleOption(final CommandLine cmd);
}
