//! Types for handling errors.

use pest::error::{InputLocation, LineColLocation};

error_chain! {
    errors {
        /// Raised by the methods of the [`Graph`](../graph/trait.Graph.html) trait.
        GraphError(message: String) {
            display("error while querying Graph: {}", message)
        }
        /// Raised by the methods of the [`MutableGraph`](../graph/trait.MutableGraph.html) trait.
        GraphMutationError(msg: String) {
            display("error while modifying Graph: {}", msg)
        }
        /// Raised whenever a literal is built with an invalid datatype.
        InvalidDatatype(datatype: String) {
            display("invalid datatype {}", datatype)
        }
        /// Raised whenever an invalid IRI is used as a term.
        InvalidIri(iri: String) {
            display("invalid IRI <{}>", iri)
        }
        /// Raised whenever a literal is built with an invalid language tag.
        InvalidLanguageTag(tag: String, message: String) {
            display("invalid language tag '{}':\n{}", tag, message)
        }
        /// Raised whenever a variable is built with an invalid name.
        InvalidVariableName(name: String) {
            display("invalid variable name '{}'", name)
        }
        /// Raised whenever an invalid prefix is used in a PName.
        InvalidPrefix(prefix: String) {
            display("invalid prefix <{}>", prefix)
        }
        /// Raised whenever an IRI can not be rendered absolute in a strict RDF graph.
        IriMustBeAbsolute(iri: String) {
            display("IRI must be absolute <{}>", iri)
        }
        /// Raised by parsers when they encounter a problem.
        ParserError(message: String, location: InputLocation, line_col: LineColLocation) {
            display("parse error at {}: {}", display_location(location, line_col), message)
        }
        /// Raised by serializers when they encounter a problem.
        SerializerError(message: String) {
            display("error while serializing: {}", message)
        }
    }
}

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

/// Make a Parser Error with minimal information
pub fn make_parser_error(message: String, line_offset: usize) -> ErrorKind {
    let il = InputLocation::Pos(0);
    let lcl = LineColLocation::Pos((line_offset, 0));
    ErrorKind::ParserError(message, il, lcl).into()
}

coercible_errors!();



#[cfg(test)]
mod test {
    // Nothing really worth testing here
}