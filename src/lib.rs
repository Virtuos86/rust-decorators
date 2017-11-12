/* TODO: Need support inline comments inside function's signature (what's the foolish Rust feature; I will try to use a some magic with `lo`/`hi` indexes).
 
   And yes, I don't want to use any regex crate, only vanilla Rust.
 */


#![feature(proc_macro)]
#![allow(unused_assignments)]

extern crate proc_macro;
use proc_macro::{TokenStream, TokenNode, Delimiter, Span, Term};

#[derive(Debug)]
struct Item(Span, Term);

#[derive(Debug, Clone)]
struct DecoratorItem {
    span: Span,
    term: Term,
    args: Vec<String>
}

enum Where {
    Begin,
    End
}

enum RecursionLevel {
    Outer,
    Inner
}

// macro instead function for a speed
macro_rules! parse_term_pos {
    ($item:expr, $where:path) => ({
        let tag = match $where {
            Where::Begin => "lo: BytePos(",
            Where::End => "hi: BytePos(",
        };
        let span_string = format!("{:?}", $item.0);
        let term_pos_string = span_string.split_at(
            span_string.find(tag)
                       .unwrap() + tag.len())
                       .1;
        let term_pos = term_pos_string.split_at(term_pos_string.find(")")
                                                               .unwrap())
                                                               .0
                                                               .parse::<usize>()
                                                               .unwrap();
        term_pos
    })
}

macro_rules! get_group_slice {
    ($source:expr, $item:ident, $begin_pos:expr) => ({
        let begin = parse_term_pos!($item, Where::Begin) - $begin_pos + 1;
        let end = parse_term_pos!($item, Where::End) - $begin_pos - begin - 1;
        let slice = $source.split_at(begin).1.split_at(end).0;
        slice
    })
}

macro_rules! get_func_args_list_from_their_source {
    ($func_args_source:expr) => ({
        let source = $func_args_source;
        let split_by_colon = source.split(':').count() - 1;
        let split_by_double_colon = source.split("::").count() - 1;
        let capacity = if split_by_colon != split_by_double_colon { split_by_colon - 2*split_by_double_colon }
                       else {0};
        let mut args_list = Vec::with_capacity(capacity);
        let mut temp = $func_args_source;
        while temp.find(':').is_some() {
            let colon_pos = temp.find(':').unwrap();
            // Processing of qualified identifiers 
            match temp.find("::") {
                Some(pos) => {
                    if pos == colon_pos {
                        temp = temp.split_at(colon_pos + "::".len()).1;
                        continue;
                    };
                },
                None => ()
            }
            let (arg, temp_) = temp.split_at(colon_pos);
            match arg.rfind(',') {
                None => args_list.push(arg),
                Some(pos) => args_list.push(arg.split_at(pos + 1).1)
            }
            temp = temp_.split_at(1).1;
        }
        args_list
    })
}

fn recursive_parsing_decor_list(token_stream: &TokenStream, recursion_level: RecursionLevel) -> Vec<DecoratorItem> {
    let mut items: Vec<DecoratorItem> = Vec::new();
    for (i, token_tree) in token_stream.clone().into_iter().enumerate() {
        let token_item = token_tree.kind;
        match token_item {
            TokenNode::Group(Delimiter::Parenthesis, token_stream) => {
                let token_stream = token_stream.clone();
                match recursion_level {
                    RecursionLevel::Outer => {
                        for item in recursive_parsing_decor_list(&token_stream, RecursionLevel::Inner).into_iter() {
                            items.push(item);
                        }
                    },
                    RecursionLevel::Inner => {
                        // if a decorator has arguments
                        for item in recursive_parsing_decor_list(&token_stream, RecursionLevel::Inner).into_iter() {
                            if items[i - 1].args.len() > 0 {
                                items[i - 1].args.push(item.term.as_str().to_owned());
                            } else {
                                items[i - 1].args = vec![item.term.as_str().to_owned()];

                            };
                        }
                    }
                };
            },
            TokenNode::Group(delimiter, _) => panic!(format!("Error! Invalid input.\n         May be you need to use `Delimiter::Parenthesis` (\"()\") instead `Delimiter::{:?}` (\"[]\")", delimiter)),
            TokenNode::Term(term) => {
                items.push(DecoratorItem { span: token_tree.span, term: term, args: Vec::new() });
            },
            TokenNode::Op(character, _) => {
                if character == ',' { continue }
                else { panic!(format!("Error! Invalid input.\n         You need to use ',' instead {:?} as a separator", character)) }
            },
            TokenNode::Literal(_literal) => {
                match recursion_level {
                    RecursionLevel::Inner => items.push(DecoratorItem { span: token_tree.span, term: Term::intern(&*format!("{}", _literal)), args: Vec::new() }),
                    _ => panic!(format!("{}{}{}", "Error! This thing - ", _literal, " - obviously unnecessary."))
                };
            },
        };
    }
    items
}

fn recursive_parsing(token_stream: &TokenStream) -> Vec<Item> {
        let mut items = Vec::new();
        for token_tree in token_stream.clone().into_iter() {
            let token_item = token_tree.kind;
            match token_item {
                TokenNode::Group(delimiter, token_stream) => {
                    let mut endterm = None;
                    items.push(
                        Item(token_tree.span,
                             Term::intern(match delimiter {
                                 Delimiter::Parenthesis => {
                                     endterm = Some(")");
                                     "("
                                 },
                                 Delimiter::Brace => {
                                     endterm = Some("}");
                                     "{"
                                 },
                                 Delimiter::Bracket => {
                                     endterm = Some("]");
                                     "["
                                 },
                                 Delimiter::None => {
                                     endterm = Some("None");
                                     "None"
                                 },
                    })));
                    let token_stream = token_stream.clone();
                    for item in recursive_parsing(&token_stream).into_iter() {
                        items.push(item);
                    }
                    if endterm.is_some() { items.push(Item(token_tree.span, Term::intern(endterm.unwrap()))) };
                },
                TokenNode::Term(term) => {
                    let item = Item(token_tree.span, term);
                    items.push(item);
                },
                TokenNode::Op(character, _) => {
                    let mut b = [0; 1];
                    let result = character.encode_utf8(&mut b);
                    let item = Item(token_tree.span, Term::intern(result));
                    items.push(item);
                },
                TokenNode::Literal(_literal) => {
                    let item = Item(token_tree.span, Term::intern(&*format!("{}", _literal)));
                    items.push(item);
                },
            };
        }
        items
}

#[proc_macro_attribute]
pub fn decorators(
    decor_list: TokenStream, decorable: TokenStream) -> TokenStream {
    let decor_items = recursive_parsing_decor_list(&decor_list, RecursionLevel::Outer);
    if decor_items.len() == 0 { return decorable; }; // `#[decorators]` && `#[decorators()]` forms

    let fn_definition = decorable.to_string();
    
    let decorable_items = recursive_parsing(&decorable);
    let mut decorable_items_iter = decorable_items.into_iter();
    let mut fn_item = decorable_items_iter.next().unwrap(); // "fn" term

    let mut is_pub = None;
    if &fn_item.1.as_str() == &"pub" {
        is_pub = Some("pub ".len());
        fn_item = decorable_items_iter.next().unwrap();
    };

    let base_func_name_item = decorable_items_iter.next().unwrap();
    let base_func_name_term = &base_func_name_item.1;
    let base_func_name = base_func_name_term.as_str();
    let decors: Vec<String> = decor_items.iter()
                                       .map(|item| {
                                           let decor = item.term.as_str();
                                           if decor != "self" {
                                               if item.args.len() > 0 {
                                                   format!("{}({}", decor, item.args.join(", ") + ", " )
                                               } else {
                                                   format!("{}(", decor)
                                               }
                                           } else { format!("{}(", base_func_name) }
                                       })
                                       .collect();
    let generated_func_name = &*(decors.iter().map(|decor| decor.split_at(decor.find('(').unwrap_or(decor.len())).0).collect::<Vec<&str>>().join("_") + "_" + base_func_name);
    let mut final_source = fn_definition.replacen(base_func_name, generated_func_name, 1);
    if is_pub.is_some() { final_source = final_source.split_at("pub ".len()).1.into() }; // generated function should be private
    final_source.push_str("\n\n");
    

    let source_begin_pos = parse_term_pos!(&fn_item, Where::Begin) - { if is_pub.is_none() {0} else {is_pub.unwrap()} };

    let mut base_func_arguments: Option<&str> = None;
    let mut decorable_items = decorable_items_iter.collect::<Vec<Item>>();
    decorable_items.retain(|ref item| { let term = &item.1.as_str();
                                        if term == &"(" && base_func_arguments.is_none() {
                                            let source = &fn_definition;
                                            let slice = get_group_slice!(source, item, source_begin_pos);
                                            base_func_arguments = Some(slice);
                                        };
                                        term == &"{" });

    let slice_begin_pos = parse_term_pos!(&base_func_name_item, Where::End) - source_begin_pos;
    let slice_end_pos = parse_term_pos!(&decorable_items[0], Where::Begin) - source_begin_pos - slice_begin_pos;
    let base_func_signature = fn_definition.split_at(slice_begin_pos).1.split_at(slice_end_pos).0;

    let decors: &Vec<&str> = &decors.iter().map(|string| { if string == &base_func_name { generated_func_name }
                                                           else { string } }).collect();
    let generated_func_source = format!(
        "{}fn {}{}{{ {}{}({}{} }}",
        if is_pub.is_none() {""} else {"pub "},
        base_func_name,
        base_func_signature,
        decors.join(""),
        generated_func_name,
        get_func_args_list_from_their_source!(base_func_arguments.unwrap()).join(","),
        (0..decors.len() + 1).map(|_| ")").collect::<String>());
    final_source.push_str(&*generated_func_source);
    //println!("{:?}", final_source);
    final_source.parse().unwrap()
}
