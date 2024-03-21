use std::borrow::Cow;
use unicode_categories::UnicodeCategories;
use winnow::ascii::{digit0, digit1, not_line_ending};
use winnow::combinator::{alt, eof, opt, peek, repeat, terminated};
use winnow::error::{ErrMode, ErrorKind, InputError, ParserError};
use winnow::token::{any, literal, one_of, tag_no_case, take, take_until, take_while};
use winnow::Parser;
use winnow::{stream::AsChar, PResult};

pub(crate) fn tokenize(mut input: &str, named_placeholders: bool) -> Vec<Token<'_>> {
    let mut tokens: Vec<Token> = Vec::new();

    let mut last_reserved_token = None;

    // Keep processing the string until it is empty
    while let Ok(result) = get_next_token(
        &mut input,
        tokens.last().cloned(),
        last_reserved_token.clone(),
        named_placeholders,
    ) {
        if result.kind == TokenKind::Reserved {
            last_reserved_token = Some(result.clone());
        }
        // input = result.0;

        tokens.push(result);
    }
    tokens
}

#[derive(Debug, Clone)]
pub(crate) struct Token<'a> {
    pub kind: TokenKind,
    pub value: &'a str,
    // Only used for placeholder--there is a reason this isn't on the enum
    pub key: Option<PlaceholderKind<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TokenKind {
    Whitespace,
    String,
    Reserved,
    ReservedTopLevel,
    ReservedTopLevelNoIndent,
    ReservedNewline,
    Operator,
    OpenParen,
    CloseParen,
    LineComment,
    BlockComment,
    Number,
    Placeholder,
    Word,
}

#[derive(Debug, Clone)]
pub(crate) enum PlaceholderKind<'a> {
    Named(Cow<'a, str>),
    ZeroIndexed(usize),
    OneIndexed(usize),
}

impl<'a> PlaceholderKind<'a> {
    pub fn named(&'a self) -> &'a str {
        match self {
            PlaceholderKind::Named(val) => val.as_ref(),
            _ => "",
        }
    }

    pub fn indexed(&self) -> Option<usize> {
        match self {
            PlaceholderKind::ZeroIndexed(val) => Some(*val),
            PlaceholderKind::OneIndexed(val) => Some(*val - 1),
            _ => None,
        }
    }
}

fn get_next_token<'a>(
    input: &mut &'a str,
    previous_token: Option<Token<'a>>,
    last_reserved_token: Option<Token<'a>>,
    named_placeholders: bool,
) -> PResult<Token<'a>> {
    get_whitespace_token(input)
        .or_else(|_| get_comment_token(input))
        .or_else(|_| get_string_token(input))
        .or_else(|_| get_open_paren_token(input))
        .or_else(|_| get_close_paren_token(input))
        .or_else(|_| get_number_token(input))
        .or_else(|_| get_reserved_word_token(input, previous_token, last_reserved_token))
        .or_else(|_| get_placeholder_token(input, named_placeholders))
        .or_else(|_| get_word_token(input))
        .or_else(|_| get_operator_token(input))
}

fn get_whitespace_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    take_while(1.., char::is_whitespace)
        .map(|token| Token {
            kind: TokenKind::Whitespace,
            value: token,
            key: None,
        })
        .parse_next(input)
}

fn get_comment_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    get_line_comment_token(input).or_else(|_| get_block_comment_token(input))
}

fn get_line_comment_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    Parser::recognize((alt((literal("#"), literal("--"))), not_line_ending))
        .map(|token| Token {
            kind: TokenKind::LineComment,
            value: token,
            key: None,
        })
        .parse_next(input)
}

fn get_block_comment_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    Parser::recognize((
        literal("/*"),
        alt((
            take_until(0.., "*/"),
            Parser::<_, String, _>::recognize(repeat(0.., any)),
        )),
        opt(take(2usize)),
    ))
    .map(|token| Token {
        kind: TokenKind::BlockComment,
        value: token,
        key: None,
    })
    .parse_next(input)
}

pub fn take_till_escaping<'a, Error: ParserError<&'a str>>(
    desired: char,
    escapes: &'static [char],
) -> impl Fn(&mut &'a str) -> PResult<&'a str, Error> {
    move |input: &mut &'a str| {
        let mut chars = input.chars().enumerate().peekable();
        loop {
            let item = chars.next();
            let next = chars.peek().map(|item| item.1);
            match item {
                Some(item) => {
                    // escape?
                    if escapes.contains(&item.1) && next.map(|n| n == desired).unwrap_or(false) {
                        // consume this and next char
                        chars.next();
                        continue;
                    }

                    if item.1 == desired {
                        let byte_pos = input.chars().take(item.0).map(|c| c.len()).sum::<usize>();
                        return Ok(&input[..byte_pos]);
                    }
                }
                None => {
                    return Ok(input);
                }
            }
        }
    }
}

// This enables the following string patterns:
// 1. backtick quoted string using `` to escape
// 2. square bracket quoted string (SQL Server) using ]] to escape
// 3. double quoted string using "" or \" to escape
// 4. single quoted string using '' or \' to escape
// 5. national character quoted string using N'' or N\' to escape
fn get_string_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    alt((
        Parser::recognize((one_of('`'), take_till_escaping('`', &['`']), take(1usize))),
        Parser::recognize((one_of('['), take_till_escaping(']', &[']']), take(1usize))),
        Parser::recognize((
            one_of('"'),
            take_till_escaping('"', &['"', '\\']),
            take(1usize),
        )),
        Parser::recognize((
            one_of('\''),
            take_till_escaping('\'', &['\'', '\\']),
            take(1usize),
        )),
        Parser::recognize((
            literal("N'"),
            take_till_escaping('\'', &['\'', '\\']),
            take(1usize),
        )),
    ))
    .map(|token| Token {
        kind: TokenKind::String,
        value: token,
        key: None,
    })
    .parse_next(input)
}

// Like above but it doesn't replace double quotes
fn get_placeholder_string_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    alt((
        Parser::recognize((one_of('`'), take_till_escaping('`', &['`']), take(1usize))),
        Parser::recognize((one_of('['), take_till_escaping(']', &[']']), take(1usize))),
        Parser::recognize((one_of('"'), take_till_escaping('"', &['\\']), take(1usize))),
        Parser::recognize((
            one_of('\''),
            take_till_escaping('\'', &['\\']),
            take(1usize),
        )),
        Parser::recognize((
            literal("N'"),
            take_till_escaping('\'', &['\\']),
            take(1usize),
        )),
    ))
    .map(|token| Token {
        kind: TokenKind::String,
        value: token,
        key: None,
    })
    .parse_next(input)
}

fn get_open_paren_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    alt((literal("("), terminated(tag_no_case("CASE"), end_of_word)))
        .map(|token| Token {
            kind: TokenKind::OpenParen,
            value: token,
            key: None,
        })
        .parse_next(input)
}

fn get_close_paren_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    alt((literal(")"), terminated(tag_no_case("END"), end_of_word)))
        .map(|token| Token {
            kind: TokenKind::CloseParen,
            value: token,
            key: None,
        })
        .parse_next(input)
}

fn get_placeholder_token<'s>(input: &mut &'s str, named_placeholders: bool) -> PResult<Token<'s>> {
    // The precedence changes based on 'named_placeholders' but not the exhaustiveness.
    // This is to ensure the formatting is the same even if parameters aren't used.

    if named_placeholders {
        alt((
            get_ident_named_placeholder_token,
            get_string_named_placeholder_token,
            get_indexed_placeholder_token,
        ))
        .parse_next(input)
    } else {
        alt((
            get_indexed_placeholder_token,
            get_ident_named_placeholder_token,
            get_string_named_placeholder_token,
        ))
        .parse_next(input)
    }
}

fn get_indexed_placeholder_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    alt((
        Parser::recognize((alt((one_of('?'), one_of('$'))), digit1)),
        Parser::recognize(one_of('?')),
    ))
    .map(|token| Token {
        kind: TokenKind::Placeholder,
        value: token,
        key: if token.len() > 1 {
            if let Ok(index) = token[1..].parse::<usize>() {
                Some(if token.starts_with('$') {
                    PlaceholderKind::OneIndexed(index)
                } else {
                    PlaceholderKind::ZeroIndexed(index)
                })
            } else {
                None
            }
        } else {
            None
        },
    })
    .parse_next(input)
}

fn get_ident_named_placeholder_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    Parser::recognize((
        alt((one_of('@'), one_of(':'), one_of('$'))),
        take_while(1.., |item: char| {
            item.is_alphanumeric() || item == '.' || item == '_' || item == '$'
        }),
    ))
    .map(|token: &str| {
        let index = Cow::Borrowed(&token[1..]);
        Token {
            kind: TokenKind::Placeholder,
            value: token,
            key: Some(PlaceholderKind::Named(index)),
        }
    })
    .parse_next(input)
}

fn get_string_named_placeholder_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    Parser::recognize((
        alt((one_of('@'), one_of(':'))),
        get_placeholder_string_token,
    ))
    .map(|token| {
        let index =
            get_escaped_placeholder_key(&token[2..token.len() - 1], &token[token.len() - 1..]);

        Token {
            kind: TokenKind::Placeholder,
            value: token,
            key: Some(PlaceholderKind::Named(index)),
        }
    })
    .parse_next(input)
}

fn get_escaped_placeholder_key<'a>(key: &'a str, quote_char: &str) -> Cow<'a, str> {
    Cow::Owned(key.replace(&format!("\\{}", quote_char), quote_char))
}

fn get_number_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    Parser::recognize((
        opt(literal("-")),
        alt((scientific_notation, decimal_number, digit1)),
    ))
    .map(|token| Token {
        kind: TokenKind::Number,
        value: token,
        key: None,
    })
    .parse_next(input)
}

fn decimal_number<'s>(input: &mut &'s str) -> PResult<&'s str> {
    Parser::recognize((digit1, literal("."), digit0)).parse_next(input)
}

fn scientific_notation<'s>(input: &mut &'s str) -> PResult<&'s str> {
    Parser::recognize((
        alt((decimal_number, digit1)),
        literal("e"),
        alt((literal("-"), literal("+"), literal(""))),
        digit1,
    ))
    .parse_next(input)
}

fn get_reserved_word_token<'a>(
    input: &mut &'a str,
    previous_token: Option<Token<'a>>,
    last_reserved_token: Option<Token<'a>>,
) -> PResult<Token<'a>, InputError<&'a str>> {
    // A reserved word cannot be preceded by a "."
    // this makes it so in "my_table.from", "from" is not considered a reserved word
    if let Some(token) = previous_token {
        if token.value == "." {
            return Err(ErrMode::Backtrack(InputError::new(input, ErrorKind::Not)));
        }
    }

    alt((
        get_top_level_reserved_token,
        get_newline_reserved_token(last_reserved_token),
        get_top_level_reserved_token_no_indent,
        get_plain_reserved_token,
    ))
    .parse_next(input)
}

// We have to be a bit creative here for performance reasons
fn get_uc_words(input: &str, words: usize) -> String {
    input
        .split_whitespace()
        .take(words)
        .collect::<Vec<&str>>()
        .join(" ")
        .to_ascii_uppercase()
}

fn get_top_level_reserved_token<'s>(
    input: &mut &'s str,
) -> PResult<Token<'s>, InputError<&'s str>> {
    let uc_input = get_uc_words(input, 3);
    let result: PResult<&str> = alt((
        terminated(literal("ADD"), end_of_word),
        terminated(literal("AFTER"), end_of_word),
        terminated(literal("ALTER COLUMN"), end_of_word),
        terminated(literal("ALTER TABLE"), end_of_word),
        terminated(literal("DELETE FROM"), end_of_word),
        terminated(literal("EXCEPT"), end_of_word),
        terminated(literal("FETCH FIRST"), end_of_word),
        terminated(literal("FROM"), end_of_word),
        terminated(literal("GROUP BY"), end_of_word),
        terminated(literal("GO"), end_of_word),
        terminated(literal("HAVING"), end_of_word),
        terminated(literal("INSERT INTO"), end_of_word),
        terminated(literal("INSERT"), end_of_word),
        terminated(literal("LIMIT"), end_of_word),
        terminated(literal("MODIFY"), end_of_word),
        terminated(literal("ORDER BY"), end_of_word),
        terminated(literal("SELECT"), end_of_word),
        terminated(literal("SET CURRENT SCHEMA"), end_of_word),
        terminated(literal("SET SCHEMA"), end_of_word),
        terminated(literal("SET"), end_of_word),
        alt((
            terminated(literal("UPDATE"), end_of_word),
            terminated(literal("VALUES"), end_of_word),
            terminated(literal("WHERE"), end_of_word),
        )),
    ))
    .parse_next(&mut uc_input.as_str());
    if let Ok(token) = result {
        let final_word = token.split(' ').last().unwrap();
        let input_end_pos = input.to_ascii_uppercase().find(final_word).unwrap() + final_word.len();
        let (token, returned_input) = input.split_at(input_end_pos);
        *input = returned_input;
        Ok(Token {
            kind: TokenKind::ReservedTopLevel,
            value: token,
            key: None,
        })
    } else {
        Err(ErrMode::Backtrack(InputError::new(input, ErrorKind::Alt)))
    }
}

fn get_newline_reserved_token<'a>(
    last_reserved_token: Option<Token<'a>>,
) -> impl FnMut(&mut &'a str) -> PResult<Token<'a>, InputError<&'a str>> {
    move |input: &mut &'a str| {
        let uc_input = get_uc_words(input, 3);
        let result = alt((
            terminated(literal("AND"), end_of_word),
            terminated(literal("CROSS APPLY"), end_of_word),
            terminated(literal("CROSS JOIN"), end_of_word),
            terminated(literal("ELSE"), end_of_word),
            terminated(literal("INNER JOIN"), end_of_word),
            terminated(literal("JOIN"), end_of_word),
            terminated(literal("LEFT JOIN"), end_of_word),
            terminated(literal("LEFT OUTER JOIN"), end_of_word),
            terminated(literal("OR"), end_of_word),
            terminated(literal("OUTER APPLY"), end_of_word),
            terminated(literal("OUTER JOIN"), end_of_word),
            terminated(literal("RIGHT JOIN"), end_of_word),
            terminated(literal("RIGHT OUTER JOIN"), end_of_word),
            terminated(literal("WHEN"), end_of_word),
            terminated(literal("XOR"), end_of_word),
        ))
        .parse_next(&mut uc_input.as_str());
        if let Ok(token) = result {
            let final_word = token.split(' ').last().unwrap();
            let input_end_pos =
                input.to_ascii_uppercase().find(final_word).unwrap() + final_word.len();
            let (token, returned_input) = input.split_at(input_end_pos);
            *input = returned_input;

            let kind = if token == "AND"
                && last_reserved_token.is_some()
                && last_reserved_token.as_ref().unwrap().value == "BETWEEN"
            {
                // If the "AND" is part of a "BETWEEN" clause, we want to handle it as one clause by not adding a new line.
                TokenKind::Reserved
            } else {
                TokenKind::ReservedNewline
            };
            Ok(Token {
                kind,
                value: token,
                key: None,
            })
        } else {
            Err(ErrMode::Backtrack(InputError::new(input, ErrorKind::Alt)))
        }
    }
}

fn get_top_level_reserved_token_no_indent<'s>(
    input: &mut &'s str,
) -> PResult<Token<'s>, InputError<&'s str>> {
    let uc_input = get_uc_words(input, 2);
    let result = alt((
        terminated(literal("BEGIN"), end_of_word),
        terminated(literal("DECLARE"), end_of_word),
        terminated(literal("INTERSECT"), end_of_word),
        terminated(literal("INTERSECT ALL"), end_of_word),
        terminated(literal("MINUS"), end_of_word),
        terminated(literal("UNION"), end_of_word),
        terminated(literal("UNION ALL"), end_of_word),
        terminated(literal("$$"), end_of_word),
    ))
    .parse_next(&mut uc_input.as_str());
    if let Ok(token) = result {
        let final_word = token.split(' ').last().unwrap();
        let input_end_pos = input.to_ascii_uppercase().find(final_word).unwrap() + final_word.len();
        let (token, returned_input) = input.split_at(input_end_pos);
        *input = returned_input;
        Ok(Token {
            kind: TokenKind::ReservedTopLevelNoIndent,
            value: token,
            key: None,
        })
    } else {
        Err(ErrMode::Backtrack(InputError::new(input, ErrorKind::Alt)))
    }
}

fn get_plain_reserved_token<'s>(input: &mut &'s str) -> PResult<Token<'s>, InputError<&'s str>> {
    let uc_input = get_uc_words(input, 1);
    let result = alt((
        terminated(literal("ACCESSIBLE"), end_of_word),
        terminated(literal("ACTION"), end_of_word),
        terminated(literal("AGAINST"), end_of_word),
        terminated(literal("AGGREGATE"), end_of_word),
        terminated(literal("ALGORITHM"), end_of_word),
        terminated(literal("ALL"), end_of_word),
        terminated(literal("ALTER"), end_of_word),
        terminated(literal("ANALYSE"), end_of_word),
        terminated(literal("ANALYZE"), end_of_word),
        terminated(literal("AS"), end_of_word),
        terminated(literal("ASC"), end_of_word),
        terminated(literal("AUTOCOMMIT"), end_of_word),
        terminated(literal("AUTO_INCREMENT"), end_of_word),
        terminated(literal("BACKUP"), end_of_word),
        terminated(literal("BETWEEN"), end_of_word),
        terminated(literal("BINLOG"), end_of_word),
        terminated(literal("BOTH"), end_of_word),
        terminated(literal("CASCADE"), end_of_word),
        terminated(literal("CASE"), end_of_word),
        alt((
            terminated(literal("CHANGE"), end_of_word),
            terminated(literal("CHANGED"), end_of_word),
            terminated(literal("CHARACTER SET"), end_of_word),
            terminated(literal("CHARSET"), end_of_word),
            terminated(literal("CHECK"), end_of_word),
            terminated(literal("CHECKSUM"), end_of_word),
            terminated(literal("COLLATE"), end_of_word),
            terminated(literal("COLLATION"), end_of_word),
            terminated(literal("COLUMN"), end_of_word),
            terminated(literal("COLUMNS"), end_of_word),
            terminated(literal("COMMENT"), end_of_word),
            terminated(literal("COMMIT"), end_of_word),
            terminated(literal("COMMITTED"), end_of_word),
            terminated(literal("COMPRESSED"), end_of_word),
            terminated(literal("CONCURRENT"), end_of_word),
            terminated(literal("CONSTRAINT"), end_of_word),
            terminated(literal("CONTAINS"), end_of_word),
            terminated(literal("CONVERT"), end_of_word),
            terminated(literal("CREATE"), end_of_word),
            terminated(literal("CROSS"), end_of_word),
            alt((
                terminated(literal("CURRENT_TIMESTAMP"), end_of_word),
                terminated(literal("DATABASE"), end_of_word),
                terminated(literal("DATABASES"), end_of_word),
                terminated(literal("DAY"), end_of_word),
                terminated(literal("DAY_HOUR"), end_of_word),
                terminated(literal("DAY_MINUTE"), end_of_word),
                terminated(literal("DAY_SECOND"), end_of_word),
                terminated(literal("DEFAULT"), end_of_word),
                terminated(literal("DEFINER"), end_of_word),
                terminated(literal("DELAYED"), end_of_word),
                terminated(literal("DELETE"), end_of_word),
                terminated(literal("DESC"), end_of_word),
                terminated(literal("DESCRIBE"), end_of_word),
                terminated(literal("DETERMINISTIC"), end_of_word),
                terminated(literal("DISTINCT"), end_of_word),
                terminated(literal("DISTINCTROW"), end_of_word),
                terminated(literal("DIV"), end_of_word),
                terminated(literal("DO"), end_of_word),
                terminated(literal("DROP"), end_of_word),
                terminated(literal("DUMPFILE"), end_of_word),
                alt((
                    terminated(literal("DUPLICATE"), end_of_word),
                    terminated(literal("DYNAMIC"), end_of_word),
                    terminated(literal("ELSE"), end_of_word),
                    terminated(literal("ENCLOSED"), end_of_word),
                    terminated(literal("END"), end_of_word),
                    terminated(literal("ENGINE"), end_of_word),
                    terminated(literal("ENGINES"), end_of_word),
                    terminated(literal("ENGINE_TYPE"), end_of_word),
                    terminated(literal("ESCAPE"), end_of_word),
                    terminated(literal("ESCAPED"), end_of_word),
                    terminated(literal("EVENTS"), end_of_word),
                    terminated(literal("EXEC"), end_of_word),
                    terminated(literal("EXECUTE"), end_of_word),
                    terminated(literal("EXISTS"), end_of_word),
                    terminated(literal("EXPLAIN"), end_of_word),
                    terminated(literal("EXTENDED"), end_of_word),
                    terminated(literal("FAST"), end_of_word),
                    terminated(literal("FETCH"), end_of_word),
                    terminated(literal("FIELDS"), end_of_word),
                    alt((
                        terminated(literal("FILE"), end_of_word),
                        terminated(literal("FIRST"), end_of_word),
                        terminated(literal("FIXED"), end_of_word),
                        terminated(literal("FLUSH"), end_of_word),
                        terminated(literal("FOR"), end_of_word),
                        terminated(literal("FORCE"), end_of_word),
                        terminated(literal("FOREIGN"), end_of_word),
                        terminated(literal("FULL"), end_of_word),
                        terminated(literal("FULLTEXT"), end_of_word),
                        terminated(literal("FUNCTION"), end_of_word),
                        terminated(literal("GLOBAL"), end_of_word),
                        terminated(literal("GRANT"), end_of_word),
                        terminated(literal("GRANTS"), end_of_word),
                        terminated(literal("GROUP_CONCAT"), end_of_word),
                        terminated(literal("HEAP"), end_of_word),
                        terminated(literal("HIGH_PRIORITY"), end_of_word),
                        terminated(literal("HOSTS"), end_of_word),
                        terminated(literal("HOUR"), end_of_word),
                        terminated(literal("HOUR_MINUTE"), end_of_word),
                        terminated(literal("HOUR_SECOND"), end_of_word),
                        alt((
                            terminated(literal("IDENTIFIED"), end_of_word),
                            terminated(literal("IF"), end_of_word),
                            terminated(literal("IFNULL"), end_of_word),
                            terminated(literal("IGNORE"), end_of_word),
                            terminated(literal("IN"), end_of_word),
                            terminated(literal("INDEX"), end_of_word),
                            terminated(literal("INDEXES"), end_of_word),
                            terminated(literal("INFILE"), end_of_word),
                            terminated(literal("INSERT"), end_of_word),
                            terminated(literal("INSERT_ID"), end_of_word),
                            terminated(literal("INSERT_METHOD"), end_of_word),
                            terminated(literal("INTERVAL"), end_of_word),
                            terminated(literal("INTO"), end_of_word),
                            terminated(literal("INVOKER"), end_of_word),
                            terminated(literal("IS"), end_of_word),
                            terminated(literal("ISOLATION"), end_of_word),
                            terminated(literal("KEY"), end_of_word),
                            terminated(literal("KEYS"), end_of_word),
                            terminated(literal("KILL"), end_of_word),
                            terminated(literal("LAST_INSERT_ID"), end_of_word),
                            alt((
                                terminated(literal("LEADING"), end_of_word),
                                terminated(literal("LEVEL"), end_of_word),
                                terminated(literal("LIKE"), end_of_word),
                                terminated(literal("LINEAR"), end_of_word),
                                terminated(literal("LINES"), end_of_word),
                                terminated(literal("LOAD"), end_of_word),
                                terminated(literal("LOCAL"), end_of_word),
                                terminated(literal("LOCK"), end_of_word),
                                terminated(literal("LOCKS"), end_of_word),
                                terminated(literal("LOGS"), end_of_word),
                                terminated(literal("LOW_PRIORITY"), end_of_word),
                                terminated(literal("MARIA"), end_of_word),
                                terminated(literal("MASTER"), end_of_word),
                                terminated(literal("MASTER_CONNECT_RETRY"), end_of_word),
                                terminated(literal("MASTER_HOST"), end_of_word),
                                terminated(literal("MASTER_LOG_FILE"), end_of_word),
                                terminated(literal("MATCH"), end_of_word),
                                terminated(literal("MAX_CONNECTIONS_PER_HOUR"), end_of_word),
                                terminated(literal("MAX_QUERIES_PER_HOUR"), end_of_word),
                                terminated(literal("MAX_ROWS"), end_of_word),
                                alt((
                                    terminated(literal("MAX_UPDATES_PER_HOUR"), end_of_word),
                                    terminated(literal("MAX_USER_CONNECTIONS"), end_of_word),
                                    terminated(literal("MEDIUM"), end_of_word),
                                    terminated(literal("MERGE"), end_of_word),
                                    terminated(literal("MINUTE"), end_of_word),
                                    terminated(literal("MINUTE_SECOND"), end_of_word),
                                    terminated(literal("MIN_ROWS"), end_of_word),
                                    terminated(literal("MODE"), end_of_word),
                                    terminated(literal("MODIFY"), end_of_word),
                                    terminated(literal("MONTH"), end_of_word),
                                    terminated(literal("MRG_MYISAM"), end_of_word),
                                    terminated(literal("MYISAM"), end_of_word),
                                    terminated(literal("NAMES"), end_of_word),
                                    terminated(literal("NATURAL"), end_of_word),
                                    terminated(literal("NOT"), end_of_word),
                                    terminated(literal("NOW()"), end_of_word),
                                    terminated(literal("NULL"), end_of_word),
                                    terminated(literal("OFFSET"), end_of_word),
                                    terminated(literal("ON DELETE"), end_of_word),
                                    terminated(literal("ON UPDATE"), end_of_word),
                                    alt((
                                        terminated(literal("ON"), end_of_word),
                                        terminated(literal("ONLY"), end_of_word),
                                        terminated(literal("OPEN"), end_of_word),
                                        terminated(literal("OPTIMIZE"), end_of_word),
                                        terminated(literal("OPTION"), end_of_word),
                                        terminated(literal("OPTIONALLY"), end_of_word),
                                        terminated(literal("OUTFILE"), end_of_word),
                                        terminated(literal("PACK_KEYS"), end_of_word),
                                        terminated(literal("PAGE"), end_of_word),
                                        terminated(literal("PARTIAL"), end_of_word),
                                        terminated(literal("PARTITION"), end_of_word),
                                        terminated(literal("PARTITIONS"), end_of_word),
                                        terminated(literal("PASSWORD"), end_of_word),
                                        terminated(literal("PRIMARY"), end_of_word),
                                        terminated(literal("PRIVILEGES"), end_of_word),
                                        terminated(literal("PROCEDURE"), end_of_word),
                                        terminated(literal("PROCESS"), end_of_word),
                                        terminated(literal("PROCESSLIST"), end_of_word),
                                        terminated(literal("PURGE"), end_of_word),
                                        terminated(literal("QUICK"), end_of_word),
                                        alt((
                                            terminated(literal("RAID0"), end_of_word),
                                            terminated(literal("RAID_CHUNKS"), end_of_word),
                                            terminated(literal("RAID_CHUNKSIZE"), end_of_word),
                                            terminated(literal("RAID_TYPE"), end_of_word),
                                            terminated(literal("RANGE"), end_of_word),
                                            terminated(literal("READ"), end_of_word),
                                            terminated(literal("READ_ONLY"), end_of_word),
                                            terminated(literal("READ_WRITE"), end_of_word),
                                            terminated(literal("REFERENCES"), end_of_word),
                                            terminated(literal("REGEXP"), end_of_word),
                                            terminated(literal("RELOAD"), end_of_word),
                                            terminated(literal("RENAME"), end_of_word),
                                            terminated(literal("REPAIR"), end_of_word),
                                            terminated(literal("REPEATABLE"), end_of_word),
                                            terminated(literal("REPLACE"), end_of_word),
                                            terminated(literal("REPLICATION"), end_of_word),
                                            terminated(literal("RESET"), end_of_word),
                                            terminated(literal("RESTORE"), end_of_word),
                                            terminated(literal("RESTRICT"), end_of_word),
                                            terminated(literal("RETURN"), end_of_word),
                                            alt((
                                                terminated(literal("RETURNS"), end_of_word),
                                                terminated(literal("REVOKE"), end_of_word),
                                                terminated(literal("RLIKE"), end_of_word),
                                                terminated(literal("ROLLBACK"), end_of_word),
                                                terminated(literal("ROW"), end_of_word),
                                                terminated(literal("ROWS"), end_of_word),
                                                terminated(literal("ROW_FORMAT"), end_of_word),
                                                terminated(literal("SECOND"), end_of_word),
                                                terminated(literal("SECURITY"), end_of_word),
                                                terminated(literal("SEPARATOR"), end_of_word),
                                                terminated(literal("SERIALIZABLE"), end_of_word),
                                                terminated(literal("SESSION"), end_of_word),
                                                terminated(literal("SHARE"), end_of_word),
                                                terminated(literal("SHOW"), end_of_word),
                                                terminated(literal("SHUTDOWN"), end_of_word),
                                                terminated(literal("SLAVE"), end_of_word),
                                                terminated(literal("SONAME"), end_of_word),
                                                terminated(literal("SOUNDS"), end_of_word),
                                                terminated(literal("SQL"), end_of_word),
                                                terminated(
                                                    literal("SQL_AUTO_IS_NULL"),
                                                    end_of_word,
                                                ),
                                                alt((
                                                    terminated(
                                                        literal("SQL_BIG_RESULT"),
                                                        end_of_word,
                                                    ),
                                                    terminated(
                                                        literal("SQL_BIG_SELECTS"),
                                                        end_of_word,
                                                    ),
                                                    terminated(
                                                        literal("SQL_BIG_TABLES"),
                                                        end_of_word,
                                                    ),
                                                    terminated(
                                                        literal("SQL_BUFFER_RESULT"),
                                                        end_of_word,
                                                    ),
                                                    terminated(literal("SQL_CACHE"), end_of_word),
                                                    terminated(
                                                        literal("SQL_CALC_FOUND_ROWS"),
                                                        end_of_word,
                                                    ),
                                                    terminated(literal("SQL_LOG_BIN"), end_of_word),
                                                    terminated(literal("SQL_LOG_OFF"), end_of_word),
                                                    terminated(
                                                        literal("SQL_LOG_UPDATE"),
                                                        end_of_word,
                                                    ),
                                                    terminated(
                                                        literal("SQL_LOW_PRIORITY_UPDATES"),
                                                        end_of_word,
                                                    ),
                                                    terminated(
                                                        literal("SQL_MAX_JOIN_SIZE"),
                                                        end_of_word,
                                                    ),
                                                    terminated(
                                                        literal("SQL_NO_CACHE"),
                                                        end_of_word,
                                                    ),
                                                    terminated(
                                                        literal("SQL_QUOTE_SHOW_CREATE"),
                                                        end_of_word,
                                                    ),
                                                    terminated(
                                                        literal("SQL_SAFE_UPDATES"),
                                                        end_of_word,
                                                    ),
                                                    terminated(
                                                        literal("SQL_SELECT_LIMIT"),
                                                        end_of_word,
                                                    ),
                                                    terminated(
                                                        literal("SQL_SLAVE_SKIP_COUNTER"),
                                                        end_of_word,
                                                    ),
                                                    terminated(
                                                        literal("SQL_SMALL_RESULT"),
                                                        end_of_word,
                                                    ),
                                                    terminated(
                                                        literal("SQL_WARNINGS"),
                                                        end_of_word,
                                                    ),
                                                    terminated(literal("START"), end_of_word),
                                                    terminated(literal("STARTING"), end_of_word),
                                                    alt((
                                                        terminated(literal("STATUS"), end_of_word),
                                                        terminated(literal("STOP"), end_of_word),
                                                        terminated(literal("STORAGE"), end_of_word),
                                                        terminated(
                                                            literal("STRAIGHT_JOIN"),
                                                            end_of_word,
                                                        ),
                                                        terminated(literal("STRING"), end_of_word),
                                                        terminated(literal("STRIPED"), end_of_word),
                                                        terminated(literal("SUPER"), end_of_word),
                                                        terminated(literal("TABLE"), end_of_word),
                                                        terminated(literal("TABLES"), end_of_word),
                                                        terminated(
                                                            literal("TEMPORARY"),
                                                            end_of_word,
                                                        ),
                                                        terminated(
                                                            literal("TERMINATED"),
                                                            end_of_word,
                                                        ),
                                                        terminated(literal("THEN"), end_of_word),
                                                        terminated(literal("TO"), end_of_word),
                                                        terminated(
                                                            literal("TRAILING"),
                                                            end_of_word,
                                                        ),
                                                        terminated(
                                                            literal("TRANSACTIONAL"),
                                                            end_of_word,
                                                        ),
                                                        terminated(literal("TRUE"), end_of_word),
                                                        terminated(
                                                            literal("TRUNCATE"),
                                                            end_of_word,
                                                        ),
                                                        terminated(literal("TYPE"), end_of_word),
                                                        terminated(literal("TYPES"), end_of_word),
                                                        terminated(
                                                            literal("UNCOMMITTED"),
                                                            end_of_word,
                                                        ),
                                                        alt((
                                                            terminated(
                                                                literal("UNIQUE"),
                                                                end_of_word,
                                                            ),
                                                            terminated(
                                                                literal("UNLOCK"),
                                                                end_of_word,
                                                            ),
                                                            terminated(
                                                                literal("UNSIGNED"),
                                                                end_of_word,
                                                            ),
                                                            terminated(
                                                                literal("USAGE"),
                                                                end_of_word,
                                                            ),
                                                            terminated(literal("USE"), end_of_word),
                                                            terminated(
                                                                literal("USING"),
                                                                end_of_word,
                                                            ),
                                                            terminated(
                                                                literal("VARIABLES"),
                                                                end_of_word,
                                                            ),
                                                            terminated(
                                                                literal("VIEW"),
                                                                end_of_word,
                                                            ),
                                                            terminated(
                                                                literal("WHEN"),
                                                                end_of_word,
                                                            ),
                                                            terminated(
                                                                literal("WITH"),
                                                                end_of_word,
                                                            ),
                                                            terminated(
                                                                literal("WORK"),
                                                                end_of_word,
                                                            ),
                                                            terminated(
                                                                literal("WRITE"),
                                                                end_of_word,
                                                            ),
                                                            terminated(
                                                                literal("YEAR_MONTH"),
                                                                end_of_word,
                                                            ),
                                                        )),
                                                    )),
                                                )),
                                            )),
                                        )),
                                    )),
                                )),
                            )),
                        )),
                    )),
                )),
            )),
        )),
    ))
    .parse_next(&mut uc_input.as_str());
    if let Ok(token) = result {
        let input_end_pos = token.len();
        let (token, returned_input) = input.split_at(input_end_pos);
        *input = returned_input;

        println!("input: {input}");
        println!("returned input: {returned_input}");
        println!("token: {token}");
        Ok(Token {
            kind: TokenKind::Reserved,
            value: token,
            key: None,
        })
    } else {
        Err(ErrMode::Backtrack(InputError::new(input, ErrorKind::Alt)))
    }
}

fn get_word_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    take_while(1.., is_word_character)
        .map(|token| Token {
            kind: TokenKind::Word,
            value: token,
            key: None,
        })
        .parse_next(input)
}

fn get_operator_token<'s>(input: &mut &'s str) -> PResult<Token<'s>> {
    alt((
        literal("!="),
        literal("<>"),
        literal("=="),
        literal("<="),
        literal(">="),
        literal("!<"),
        literal("!>"),
        literal("||"),
        literal("::"),
        literal("->>"),
        literal("->"),
        literal("~~*"),
        literal("~~"),
        literal("!~~*"),
        literal("!~~"),
        literal("~*"),
        literal("!~*"),
        literal("!~"),
        literal(":="),
        Parser::recognize(Parser::verify(take(1usize), |token: &str| {
            token != "\n" && token != "\r"
        })),
    ))
    .map(|token| Token {
        kind: TokenKind::Operator,
        value: token,
        key: None,
    })
    .parse_next(input)
}

fn end_of_word<'s>(input: &mut &'s str) -> PResult<&'s str> {
    peek(alt((
        eof,
        Parser::verify(take(1usize), |val: &str| {
            !is_word_character(val.chars().next().unwrap())
        }),
    )))
    .parse_next(input)
}

fn is_word_character(item: char) -> bool {
    item.is_alphanumeric() || item.is_mark() || item.is_punctuation_connector()
}
