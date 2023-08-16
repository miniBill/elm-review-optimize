module MyDebug exposing (log, todo, warn)


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
