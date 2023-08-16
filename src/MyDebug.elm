module MyDebug exposing (log, logWrap, todo, warn)


log : String -> a -> a
log msg value =
    let
        _ =
            Debug.log msg value

        _ =
            ()
    in
    value


todo : String -> a -> a
todo msg default =
    let
        _ =
            Debug.todo msg

        -- _ =
        --     log ("TODO: " ++ msg) ()
    in
    default


warn : String -> a -> a
warn msg default =
    let
        _ =
            log ("WARN: " ++ msg) ()

        _ =
            ()
    in
    default


logIndent : Int -> String -> a -> a
logIndent indent msg value =
    --  log (String.repeat indent "  " ++ msg) value
    value


logWrap : Int -> String -> (Int -> a) -> a
logWrap indent name f =
    let
        _ =
            logIndent indent name ()
    in
    let
        result =
            f (indent + 1)
    in
    let
        _ =
            logIndent indent (name ++ " done") ()
    in
    result
