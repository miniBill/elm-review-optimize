module MyDebug exposing (log, logWrap, todo, warn)


log : String -> a -> a
log msg value =
    -- value
    Debug.log msg value


todo : String -> a -> a
todo msg default =
    -- warn msg default
    Debug.todo msg


warn : String -> a -> a
warn msg default =
    let
        _ =
            log ("WARN: " ++ msg) ()
    in
    default


logIndent : Int -> String -> a -> a
logIndent indent msg =
    log (String.repeat indent "  " ++ msg)


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
