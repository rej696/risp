use std::fmt;
use std::io;
use std::rc::Rc;
use std::collections::HashMap;
use std::num::ParseFloatError;

#[derive(Clone)]
enum RispExp {
    Bool(bool),
    Symbol(String),
    Number(f64),
    List(Vec<RispExp>),
    Func(fn(&[RispExp]) -> Result<RispExp, RispErr>),
    Lambda(RispLambda)
}

#[derive(Clone)]
struct RispLambda {
    params_exp: Rc<RispExp>,
    body_exp: Rc<RispExp>,
}

impl fmt::Display for RispExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let str = match self {
            RispExp::Bool(b) => b.to_string(),
            RispExp::Symbol(s) => s.clone(),
            RispExp::Number(n) => n.to_string(),
            RispExp::List(list) => {
                let xs: Vec<String> = list
                    .iter()
                    .map(|x| x.to_string())
                    .collect();
                format!("({})", xs.join(","))
            },
            RispExp::Func(_) => "Function {}".to_string(),
            RispExp::Lambda(_) => "Lambda {}".to_string(),
        };

        write!(f, "{}", str)
    }
}

#[derive(Debug)]
enum RispErr {
    InvalidToken,
    InvalidArgs,
    InvalidNumber,
    // SyntaxErr(u32, u32),
    UnbalancedParens,
    UnexpectedForm,
    EmptyList,
    FormNotFunction,
    UnexpectedSymbol(String),
    ExpectedForm,
    ExpectedCondForm,
    InvalidCondForm,
    InvalidTestForm,
    ExpectedTestForm,
    UnexpectedTestForm,
    InvalidDefForm,
    InvalidLambdaForm,
    ExpectedArgsList,
    InvalidArgsList,
    InvalidArguments(usize, usize)
}

impl fmt::Display for RispErr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RispErr::InvalidToken => write!(f, "Unable to parse token"),
            RispErr::InvalidArgs => write!(f, "Function arguments are invalid"),
            RispErr::InvalidNumber => write!(f, "The token is not a valid number"),
            // RispErr::SyntaxErr(l,c) => write!(f, "Syntax Error at line {}, col {}", l, c),
            RispErr::UnbalancedParens => write!(f, "Unbalanced Parentheses"),
            RispErr::UnexpectedForm => write!(f, "Unexpected Form"),
            RispErr::EmptyList => write!(f, "Expected a non-empty list"),
            RispErr::FormNotFunction => write!(f, "First form in a list must be a function"),
            RispErr::UnexpectedSymbol(k) => write!(f, "Unknown symbol '{}'", k),
            RispErr::ExpectedForm => write!(f, "Expected a form"),
            RispErr::ExpectedCondForm => write!(f, "Expected a Cond form"),
            RispErr::InvalidCondForm => write!(f, "Invalid Cond form"),
            RispErr::InvalidTestForm => write!(f, "Invalid Test form"),
            RispErr::ExpectedTestForm => write!(f, "Expected a test form"),
            RispErr::UnexpectedTestForm => write!(f, "Unexpected test form"),
            RispErr::InvalidDefForm => write!(f, "Invalid define form"),
            RispErr::InvalidLambdaForm => write!(f, "Invalid lambda form"),
            RispErr::ExpectedArgsList => write!(f, "Expected a list of arguments"),
            RispErr::InvalidArgsList => write!(f, "Arguments lits is invalid"),
            RispErr::InvalidArguments(e, g) => write!(f, "Expected {} args, found {}", e, g)
        }
    }
}

#[derive(Clone)]
struct RispEnv<'a> {
    data: HashMap<String, RispExp>,
    outer: Option<&'a RispEnv<'a>>,
}

fn tokenise(expr: String) -> Vec<String> {
    expr.replace("(", " ( ")
        .replace(")", " ) ")
        .split_whitespace()
        .map(|x| x.to_string())
        .collect()
}

fn parse<'a>(tokens: &'a [String]) -> Result<(RispExp, &'a [String]), RispErr> {
    let (token, rest) = tokens.split_first()
        .ok_or(RispErr::InvalidToken)?;

    match &token[..] {
        "(" => read_seq(rest),
        ")" => Err(RispErr::UnbalancedParens),
        _ => Ok((parse_atom(token), rest)),
    }
}

fn read_seq<'a>(tokens: &'a [String]) -> Result<(RispExp, &'a [String]), RispErr> {
    let mut res: Vec<RispExp> = vec![];
    let mut xs = tokens;
    loop {
        let (next_token, rest) = xs
            .split_first()
            .ok_or(RispErr::UnbalancedParens)
            ?;
        if next_token == ")" {
            // skip ')', head to the next token
            return Ok((RispExp::List(res), rest))
        }
        let (exp, new_xs) = parse(&xs)?;
        res.push(exp);
        xs = new_xs;
    }
}

fn parse_atom(token: &str) -> RispExp {
    match token.as_ref() {
        "true" => RispExp::Bool(true),
        "else" => RispExp::Bool(true),
        "false" => RispExp::Bool(false),
        _ => {
            let potential_float: Result<f64, ParseFloatError> = token.parse();
            match potential_float {
                Ok(v) => RispExp::Number(v),
                Err(_) => RispExp::Symbol(token.to_string().clone())
            }
        }
    }
}

macro_rules! ensure_tonicity {
    ($check_fn:expr) => {{
        |args: &[RispExp]| -> Result<RispExp, RispErr> {
            let floats = parse_list_of_floats(args)?;
            let first = floats.first().ok_or(RispErr::InvalidArgs)?;
            let rest = &floats[1..];
            fn f (prev: &f64, xs: &[f64]) -> bool {
                match xs.first() {
                    Some(x) => $check_fn(prev, x) && f(x, &xs[1..]),
                    None => true,
                }
            }
            Ok(RispExp::Bool(f(first, rest)))
        }
    }};
}

fn default_env<'a>() -> RispEnv<'a> {
    let mut data: HashMap<String, RispExp> = HashMap::new();
    data.insert(
        "+".to_string(),
        RispExp::Func(
            |args: &[RispExp]| -> Result<RispExp, RispErr> {
                let sum = parse_list_of_floats(args)?.iter().fold(0.0, |sum, a| sum + a);

                Ok(RispExp::Number(sum))
            }
        )
    );
    data.insert(
        "-".to_string(),
        RispExp::Func(
            |args: &[RispExp]| -> Result<RispExp, RispErr> {
                let floats = parse_list_of_floats(args)?;
                let first = *floats.first().ok_or(RispErr::InvalidArgs)?;
                let sum_of_rest = floats[1..].iter().fold(0.0, |sum, a| sum + a);

                Ok(RispExp::Number(first - sum_of_rest))
            }
        )
    );
    data.insert(
        "=".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a == b))
    );
    data.insert(
        ">".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a > b))
    );
    data.insert(
        ">=".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a >= b))
    );
    data.insert(
        "<".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a < b))
    );
    data.insert(
        "<=".to_string(),
        RispExp::Func(ensure_tonicity!(|a, b| a <= b))
    );

    RispEnv {data, outer: None}
}

fn parse_list_of_floats(args: &[RispExp]) -> Result<Vec<f64>, RispErr> {
    args
        .iter()
        .map(|x| parse_single_float(x))
        .collect()
}

fn parse_single_float(exp: &RispExp) -> Result<f64, RispErr> {
    match exp {
        RispExp::Number(num) => Ok(*num),
        _ => Err(RispErr::InvalidNumber)
    }
}

fn eval_def_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let first_form = arg_forms.first().ok_or(RispErr::ExpectedForm)?;
    let first_str = match first_form {
        RispExp::Symbol(s) => Ok(s.clone()),
        _ => Err(RispErr::InvalidDefForm)
    }?;
    let second_form = arg_forms.get(1).ok_or(RispErr::ExpectedForm)?;
    if arg_forms.len() > 2 {
        return Err(RispErr::InvalidDefForm);
    }
    let second_eval = eval(second_form, env)?;
    env.data.insert(first_str, second_eval);

    Ok(first_form.clone())
}

fn eval_cond_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let first_form = arg_forms.first().ok_or(RispErr::ExpectedForm)?;
    match first_form {
        RispExp::List(list) => {
            let test_form = eval(list.first().ok_or(RispErr::InvalidCondForm)?, env)?;
            match test_form {
                RispExp::Bool(b) => {
                    if b {
                        let res_form = (&list[1..]).first().ok_or(RispErr::InvalidCondForm)?;
                        eval(res_form, env)
                    } else {
                        let next_args = &arg_forms[1..];
                        eval_cond_args(next_args, env)
                    }
                },
                _ => Err(RispErr::ExpectedTestForm)
            }
        },
        _ => Err(RispErr::ExpectedCondForm)
    }
}
    // let test_form = first_form.first().ok_or(RispErr::ExpectedCondForm)?;
    // let test_eval = eval(test_form, env)?;
    // match test_eval {
    //     RispExp::Bool(b) => {
    //         if b {
    //             let rest_form = first_form.get(1)
    //                 .ok_or(RispErr::InvalidCondForm)
    //                 ?;
    //             let res_eval = eval(rest_form, env);

    //             res_eval
    //         } else {
    //             // TODO figure out how to handle non-true cond test and fall to next branch
    //             let res_form = arg_forms.get(form_idx)
    //                 .ok_or(RispErr::InvalidTestForm)
    //                 ?;
    //             let res_eval = eval(res_form, env);

    //         }
    //     },
    //     _ => Err(RispErr::UnexpectedTestForm)
    // }
// }

fn eval_if_args(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let test_form = arg_forms.first().ok_or(RispErr::ExpectedTestForm)?;
    let test_eval = eval(test_form, env)?;
    match test_eval {
        RispExp::Bool(b) => {
            let form_idx = if b { 1 } else { 2 };
            let res_form = arg_forms.get(form_idx)
                .ok_or(RispErr::InvalidTestForm)
                ?;
            eval(res_form, env)
        },
        _ => Err(RispErr::UnexpectedTestForm)
    }
}

fn eval_lambda_args(arg_forms: &[RispExp]) -> Result<RispExp, RispErr> {
    let params_exp = arg_forms.first().ok_or(RispErr::ExpectedForm)?;
    let body_exp = arg_forms.get(1).ok_or(RispErr::ExpectedForm)?;
    if arg_forms.len() > 2 {
        return Err(RispErr::InvalidLambdaForm);
    }
    Ok(
        RispExp::Lambda(
            RispLambda {
                body_exp: Rc::new(body_exp.clone()),
                params_exp: Rc::new(params_exp.clone()),
            }
        )
    )
}

fn eval_built_in_form(
    exp: &RispExp, arg_forms: &[RispExp], env: &mut RispEnv
) -> Option<Result<RispExp, RispErr>> {
    match exp {
        RispExp::Symbol(s) =>
            match s.as_ref() {
                "cond" => Some(eval_cond_args(arg_forms, env)),
                "if" => Some(eval_if_args(arg_forms, env)),
                "def" => Some(eval_def_args(arg_forms, env)),
                "fn" => Some(eval_lambda_args(arg_forms)),
                _ => None,
            },
        _ => None,
    }
}

fn eval_forms(arg_forms: &[RispExp], env: &mut RispEnv) -> Result<Vec<RispExp>, RispErr> {
    arg_forms
        .iter()
        .map(|x| eval(x, env))
        .collect()
}

fn parse_list_of_symbol_strings(form: Rc<RispExp>) -> Result<Vec<String>, RispErr> {
    let list = match form.as_ref() {
        RispExp::List(s) => Ok(s.clone()),
        _ => Err(RispErr::ExpectedArgsList)
    }?;

    list
        .iter()
        .map(
            |x| {
                match x {
                    RispExp::Symbol(s) => Ok(s.clone()),
                    _ => Err(RispErr::InvalidArgsList)
                }
            }
        ).collect()
}


fn env_for_lambda<'a>(
    params: Rc<RispExp>,
    arg_forms: &[RispExp],
    outer_env: &'a mut RispEnv,
) -> Result<RispEnv<'a>, RispErr> {
    let ks = parse_list_of_symbol_strings(params)?;
    if ks.len() != arg_forms.len() {
        return Err(RispErr::InvalidArguments(ks.len(), arg_forms.len()));
    }
    let vs = eval_forms(arg_forms, outer_env)?;
    let mut data: HashMap<String, RispExp> = HashMap::new();
    for (k, v) in ks.iter().zip(vs.iter()) {
        data.insert(k.clone(), v.clone());
    }
    Ok(
        RispEnv {
            data,
            outer: Some(outer_env),
        }
    )
}

fn eval_list(list: &Vec<RispExp>, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let first_form = list
        .first()
        .ok_or(RispErr::EmptyList)
        ?;
    let arg_forms = &list[1..];
    match eval_built_in_form(first_form, arg_forms, env) {
        Some(res) => res,
        None => {
            let first_eval = eval(first_form, env)?;
            match first_eval {
                RispExp::Func(f) => {
                    let args_eval = arg_forms
                        .iter()
                        .map(|x| eval(x, env))
                        .collect::<Result<Vec<RispExp>, RispErr>>();
                    f(&args_eval?)
                },
                RispExp::Lambda(lambda) => {
                    let new_env = &mut env_for_lambda(lambda.params_exp, arg_forms, env)?;
                    eval(&lambda.body_exp, new_env)
                },
                _ => Err(RispErr::FormNotFunction),
            }
        }
    }
}

fn env_get(k: &str, env: &RispEnv) -> Option<RispExp> {
    match env.data.get(k) {
        Some(exp) => Some(exp.clone()),
        None => {
            match &env.outer {
                Some(outer_env) => env_get(k, &outer_env),
                None => None
            }
        }
    }
}

fn eval(exp: &RispExp, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    match exp {
        RispExp::Symbol(k) => env_get(k, env)
            .ok_or(RispErr::UnexpectedSymbol(k.to_string()))
            .map(|x| x.clone()),
        RispExp::Number(_a) => Ok(exp.clone()),
        RispExp::Bool(_a) => Ok(exp.clone()),
        RispExp::List(list) => eval_list(list, env),
        RispExp::Func(_) => Err(RispErr::UnexpectedForm),
        RispExp::Lambda(_) => Err(RispErr::UnexpectedForm)
    }
}

fn parse_eval(expr: String, env: &mut RispEnv) -> Result<RispExp, RispErr> {
    let(parsed_exp, _) = parse(&tokenise(expr))?;
    let evaled_exp = eval(&parsed_exp, env)?;

    Ok(evaled_exp)
}

fn slurp_expr() -> String {
    let mut expr = String::new();

    io::stdin().read_line(&mut expr)
        .expect("Failed to read line");

    expr
}

fn main() {
    let env = &mut default_env();
    loop {
        println!("risp >");
        let expr = slurp_expr();

        match parse_eval(expr, env) {
            Ok(res) => println!("// {}", res),
            Err(e) => println!("// {}", e),
        }
    }
}
