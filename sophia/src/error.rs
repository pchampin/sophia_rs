//! Types for handling errors.

use pest::error::{InputLocation, LineColLocation};

error_chain! {
    errors {
        InvalidDatatype(datatype: String) {
            display("invalid datatype {}", datatype)
        }
        InvalidIri(iri: String) {
            display("invalid IRI <{}>", iri)
        }
        InvalidLanguageTag(tag: String, message: String) {
            display("invalid language tag '{}':\n{}", tag, message)
        }
        InvalidVariableName(name: String) {
            display("invalid variable name '{}'", name)
        }
        InvalidPrefix(prefix: String) { // useful for parsers dealing with PNames
            display("invalid prefix <{}>", prefix)
        }
        IriMustBeAbsolute(iri: String) {
            display("IRI must be absolute <{}>", iri)
        }
        Parsing(message: String, location: InputLocation, line_col: LineColLocation) {
            display("parse error at {}:\n{}", display_location(location, line_col), message)
        }

    }
    foreign_links {
        Io(::std::io::Error);
    }
}

/// An "error" type that can never happen.
/// 
/// NB: once the [`never`] types reaches *stable*,
/// this type will be an alias for the standard type.
/// 
/// [`never`]: https://doc.rust-lang.org/std/primitive.never.html
/// 
#[derive(Clone, Debug)]
pub enum Never {}

impl ::std::fmt::Display for Never {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "Never")
    }
}

impl ::std::error::Error for Never {}


fn display_location(il: &InputLocation, lcl: &LineColLocation) -> String {
    let line = *match lcl {
        LineColLocation::Pos((line, _)) => line,
        LineColLocation::Span((line, _), _) => line,
    };
    if line == 0 {
        match il {
            InputLocation::Pos(pos) =>
                format!("{}", pos),
            InputLocation::Span((s, e)) =>
                format!("{}-{}", s, e),
        }
    } else {
        match lcl {
            LineColLocation::Pos((l, c)) =>
                format!("{}:{}", l, c),
            LineColLocation::Span((l1, c1), (l2, c2)) =>
                format!("{}:{}-{}:{}", l1, c1, l2, c2),
        }
    }
}



#[cfg(test)]
mod test {
    // Nothing really worth testing here
}