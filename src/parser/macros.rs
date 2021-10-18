macro_rules! parse_if_matches {
    ($tokens:expr, $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
        match $tokens.token_type() {
            Result::Ok(x) => match x {
                $( $pattern )|+ $( if $guard )? => {
                    $tokens.next_token().map(Option::Some)
                },
                _ => Result::Ok(Option::None)
            }
            Result::Err(x) => Result::Err(x),
        }
    };

    ($tokens:expr, $ignore:expr, $( $pattern:pat_param )|+ $( if $guard: expr )? $(,)?) => {
        match $tokens.token_type() {
            Result::Ok(x) => match x {
                $( $pattern )|+ $( if $guard )? => {
                    $tokens.next_tok($ignore).map(Option::Some)
                },
                _ => Result::Ok(Option::None)
            }
            Result::Err(x) => Result::Err(x),
        }
    };
}
