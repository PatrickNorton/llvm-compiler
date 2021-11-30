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

macro_rules! expect_matches {
    ($tokens:expr, $pattern:pat, $message:expr) => {
        match $tokens.token_type() {
            Result::Ok(x) => match x {
                $pattern => $tokens.next_token(),
                _ => Result::Err($tokens.error_expected($message)),
            },
            Result::Err(x) => Result::Err(x),
        }
    };
}

macro_rules! line_matches {
    ($tokens:expr, $pattern:pat) => {
        $tokens.line_contains(|x| matches!(x.token_type(), $pattern))
    };
}

pub(crate) use {expect_matches, line_matches, parse_if_matches};
