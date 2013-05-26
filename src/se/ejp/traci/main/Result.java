package se.ejp.traci.main;

public enum Result
{
    SUCCESS(0),
    ABORT(0),
    IO_ERROR(-1),
    PARSE_ERROR(-2),
    RUNTIME_ERROR(-3),
    INVALID_ARGUMENT_ERROR(-4),
    PREPROCESSOR_ERROR(-5),
    INTERNAL_ERROR(-6);

    public final int code;

    private Result(final int code)
    {
        this.code = code;
    }
}
