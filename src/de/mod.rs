//! Deserialize JSON data to a Rust data structure

use core::{fmt, str};

#[cfg(feature = "custom-error-messages")]
use alloc::string::String;
use serde::de::{self, Visitor};

use self::enum_::{UnitVariantAccess, VariantAccess};
use self::map::MapAccess;
use self::read::Reference;
use self::read::{Read, SliceRead};
use self::seq::SeqAccess;

use alloc::vec::Vec;

mod enum_;
mod map;
mod read;
mod seq;

/// Deserialization result
pub type Result<T> = core::result::Result<T, Error>;

/// This type represents all possible errors that can occur when deserializing JSON data
#[derive(Debug, PartialEq)]
#[non_exhaustive]
pub enum Error {
    /// EOF while parsing a list.
    EofWhileParsingList,

    /// EOF while parsing an object.
    EofWhileParsingObject,

    /// EOF while parsing a string.
    EofWhileParsingString,

    /// EOF while parsing a JSON number.
    EofWhileParsingNumber,

    /// EOF while parsing a JSON value.
    EofWhileParsingValue,

    /// Expected this character to be a `':'`.
    ExpectedColon,

    /// Expected this character to be either a `','` or a `']'`.
    ExpectedListCommaOrEnd,

    /// Expected this character to be either a `','` or a `'}'`.
    ExpectedObjectCommaOrEnd,

    /// Expected to parse either a `true`, `false`, or a `null`.
    ExpectedSomeIdent,

    /// Expected this character to start a JSON value.
    ExpectedSomeValue,

    /// Invalid number.
    InvalidNumber,

    /// Invalid type
    InvalidType,

    /// Invalid unicode code point.
    InvalidUnicodeCodePoint,

    /// Object key is not a string.
    KeyMustBeAString,

    /// JSON has non-whitespace trailing characters after the value.
    TrailingCharacters,

    /// JSON has a comma after the last value in an array or map.
    TrailingComma,

    /// Error with a custom message that we had to discard.
    CustomError,

    /// Invalid escape
    InvalidEscape,

    /// Control character while parsing string
    ControlCharacterWhileParsingString,

    /// Error with a custom message that was preserved.
    #[cfg(feature = "custom-error-messages")]
    CustomErrorWithMessage(String),
}

#[cfg(feature = "std")]
impl ::std::error::Error for Error {
    fn description(&self) -> &str {
        ""
    }
}

pub(crate) struct Deserializer<'b> {
    read: SliceRead<'b>,
    scratch: Vec<u8>,
}

impl<'a> Deserializer<'a> {
    fn new(slice: &'a [u8]) -> Deserializer<'_> {
        Deserializer {
            read: SliceRead::new(slice),
            scratch: Vec::new(),
        }
    }

    fn eat_char(&mut self) {
        self.read.discard();
    }

    fn end(&mut self) -> Result<()> {
        match self.parse_whitespace() {
            Some(_) => Err(Error::TrailingCharacters),
            None => Ok(()),
        }
    }

    fn end_seq(&mut self) -> Result<()> {
        match self.parse_whitespace().ok_or(Error::EofWhileParsingList)? {
            b']' => {
                self.eat_char();
                Ok(())
            }
            b',' => {
                self.eat_char();
                match self.parse_whitespace() {
                    Some(b']') => Err(Error::TrailingComma),
                    _ => Err(Error::TrailingCharacters),
                }
            }
            _ => Err(Error::TrailingCharacters),
        }
    }

    fn end_map(&mut self) -> Result<()> {
        match self
            .parse_whitespace()
            .ok_or(Error::EofWhileParsingObject)?
        {
            b'}' => {
                self.eat_char();
                Ok(())
            }
            b',' => Err(Error::TrailingComma),
            _ => Err(Error::TrailingCharacters),
        }
    }

    fn next_char(&mut self) -> Option<u8> {
        self.read.next_byte()
    }

    fn parse_ident(&mut self, ident: &[u8]) -> Result<()> {
        for c in ident {
            if Some(*c) != self.next_char() {
                return Err(Error::ExpectedSomeIdent);
            }
        }

        Ok(())
    }

    fn parse_object_colon(&mut self) -> Result<()> {
        match self
            .parse_whitespace()
            .ok_or(Error::EofWhileParsingObject)?
        {
            b':' => {
                self.eat_char();
                Ok(())
            }
            _ => Err(Error::ExpectedColon),
        }
    }

    /// Consumes all the whitespace characters and returns a peek into the next character
    fn parse_whitespace(&mut self) -> Option<u8> {
        loop {
            match self.peek() {
                Some(b' ') | Some(b'\n') | Some(b'\t') | Some(b'\r') => {
                    self.eat_char();
                }
                other => {
                    return other;
                }
            }
        }
    }

    fn peek(&mut self) -> Option<u8> {
        self.read.peek_byte()
    }
}

// NOTE(deserialize_*signed) we avoid parsing into u64 and then casting to a smaller integer, which
// is what upstream does, to avoid pulling in 64-bit compiler intrinsics, which waste a few KBs of
// Flash, when targeting non 64-bit architectures
macro_rules! deserialize_unsigned {
    ($self:ident, $visitor:ident, $uxx:ident, $visit_uxx:ident) => {{
        let peek = $self
            .parse_whitespace()
            .ok_or(Error::EofWhileParsingValue)?;

        match peek {
            b'-' => Err(Error::InvalidNumber),
            b'0' => {
                $self.eat_char();
                $visitor.$visit_uxx(0)
            }
            b'1'..=b'9' => {
                $self.eat_char();

                let mut number = (peek - b'0') as $uxx;
                loop {
                    match $self.peek() {
                        Some(c @ b'0'..=b'9') => {
                            $self.eat_char();
                            number = number
                                .checked_mul(10)
                                .ok_or(Error::InvalidNumber)?
                                .checked_add((c - b'0') as $uxx)
                                .ok_or(Error::InvalidNumber)?;
                        }
                        _ => return $visitor.$visit_uxx(number),
                    }
                }
            }
            _ => Err(Error::InvalidType),
        }
    }};
}

macro_rules! deserialize_signed {
    ($self:ident, $visitor:ident, $ixx:ident, $visit_ixx:ident) => {{
        let signed = match $self
            .parse_whitespace()
            .ok_or(Error::EofWhileParsingValue)?
        {
            b'-' => {
                $self.eat_char();
                true
            }
            _ => false,
        };

        match $self.peek().ok_or(Error::EofWhileParsingValue)? {
            b'0' => {
                $self.eat_char();
                $visitor.$visit_ixx(0)
            }
            c @ b'1'..=b'9' => {
                $self.eat_char();

                let mut number = (c - b'0') as $ixx * if signed { -1 } else { 1 };
                loop {
                    match $self.peek() {
                        Some(c @ b'0'..=b'9') => {
                            $self.eat_char();
                            number = number
                                .checked_mul(10)
                                .ok_or(Error::InvalidNumber)?
                                .checked_add((c - b'0') as $ixx * if signed { -1 } else { 1 })
                                .ok_or(Error::InvalidNumber)?;
                        }
                        _ => return $visitor.$visit_ixx(number),
                    }
                }
            }
            _ => return Err(Error::InvalidType),
        }
    }};
}

impl<'a, 'de> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let peek = self.parse_whitespace().ok_or(Error::EofWhileParsingValue)?;
        match peek {
            b'n' => {
                self.eat_char();
                self.parse_ident(b"ull")?;
                visitor.visit_unit()
            }
            b't' => {
                self.eat_char();
                self.parse_ident(b"rue")?;
                visitor.visit_bool(true)
            }
            b'f' => {
                self.eat_char();
                self.parse_ident(b"alse")?;
                visitor.visit_bool(false)
            }
            b'"' => {
                self.eat_char();
                self.scratch.clear();
                match self.read.parse_str(&mut self.scratch)? {
                    Reference::Borrowed(s) => visitor.visit_borrowed_str(s),
                    Reference::Copied(s) => visitor.visit_str(s),
                }
            }
            b'[' => {
                self.eat_char();
                let ret = visitor.visit_seq(SeqAccess::new(self));

                match (ret, self.end_seq()) {
                    (Ok(ret), Ok(())) => Ok(ret),
                    (Err(err), _) | (_, Err(err)) => Err(err),
                }
            }
            b'{' => {
                self.eat_char();
                let ret = visitor.visit_map(MapAccess::new(self));

                match (ret, self.end_map()) {
                    (Ok(ret), Ok(())) => Ok(ret),
                    (Err(err), _) | (_, Err(err)) => Err(err),
                }
            }
            _ => Err(Error::InvalidType),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let peek = self.parse_whitespace().ok_or(Error::EofWhileParsingValue)?;

        match peek {
            b't' => {
                self.eat_char();
                self.parse_ident(b"rue")?;
                visitor.visit_bool(true)
            }
            b'f' => {
                self.eat_char();
                self.parse_ident(b"alse")?;
                visitor.visit_bool(false)
            }
            _ => Err(Error::InvalidType),
        }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        deserialize_signed!(self, visitor, i8, visit_i8)
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        deserialize_signed!(self, visitor, i16, visit_i16)
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        deserialize_signed!(self, visitor, i32, visit_i32)
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        deserialize_signed!(self, visitor, i64, visit_i64)
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        deserialize_unsigned!(self, visitor, u8, visit_u8)
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        deserialize_unsigned!(self, visitor, u16, visit_u16)
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        deserialize_unsigned!(self, visitor, u32, visit_u32)
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        deserialize_unsigned!(self, visitor, u64, visit_u64)
    }

    fn deserialize_f32<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        Err(Error::InvalidNumber)
    }

    fn deserialize_f64<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        Err(Error::InvalidNumber)
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let peek = self.parse_whitespace().ok_or(Error::EofWhileParsingValue)?;

        match peek {
            b'"' => {
                self.eat_char();
                self.scratch.clear();
                match self.read.parse_str(&mut self.scratch)? {
                    Reference::Borrowed(s) => visitor.visit_borrowed_str(s),
                    Reference::Copied(s) => visitor.visit_str(s),
                }
            }
            _ => Err(Error::InvalidType),
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let peek = self.parse_whitespace().ok_or(Error::EofWhileParsingValue)?;

        match peek {
            b'"' => {
                self.eat_char();
                self.scratch.clear();
                match self.read.parse_str_raw(&mut self.scratch)? {
                    Reference::Borrowed(b) => visitor.visit_borrowed_bytes(b),
                    Reference::Copied(b) => visitor.visit_bytes(b),
                }
            }
            b'[' => self.deserialize_seq(visitor),
            _ => Err(Error::InvalidType),
        }
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_bytes(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self.parse_whitespace().ok_or(Error::EofWhileParsingValue)? {
            b'n' => {
                self.eat_char();
                self.parse_ident(b"ull")?;
                visitor.visit_none()
            }
            _ => visitor.visit_some(self),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let peek = match self.parse_whitespace() {
            Some(b) => b,
            None => {
                return Err(Error::EofWhileParsingValue);
            }
        };

        match peek {
            b'n' => {
                self.eat_char();
                self.parse_ident(b"ull")?;
                visitor.visit_unit()
            }
            _ => Err(Error::InvalidType),
        }
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_unit(visitor)
    }

    /// Unsupported. We can’t parse newtypes because we don’t know the underlying type.
    fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self.parse_whitespace().ok_or(Error::EofWhileParsingValue)? {
            b'[' => {
                self.eat_char();
                let ret = visitor.visit_seq(SeqAccess::new(self))?;

                self.end_seq()?;

                Ok(ret)
            }
            _ => Err(Error::InvalidType),
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let peek = self.parse_whitespace().ok_or(Error::EofWhileParsingValue)?;

        if peek == b'{' {
            self.eat_char();

            let ret = visitor.visit_map(MapAccess::new(self))?;

            self.end_map()?;

            Ok(ret)
        } else {
            Err(Error::InvalidType)
        }
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self.parse_whitespace().ok_or(Error::EofWhileParsingValue)? {
            b'"' => visitor.visit_enum(UnitVariantAccess::new(self)),
            b'{' => {
                self.eat_char();
                let value = visitor.visit_enum(VariantAccess::new(self))?;
                match self.parse_whitespace().ok_or(Error::EofWhileParsingValue)? {
                    b'}' => {
                        self.eat_char();
                        Ok(value)
                    }
                    _ => Err(Error::ExpectedSomeValue),
                }
            }
            _ => Err(Error::ExpectedSomeValue),
        }
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        self.deserialize_str(visitor)
    }

    /// Used to throw out fields from JSON objects that we don’t want to
    /// keep in our structs.
    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        match self.parse_whitespace().ok_or(Error::EofWhileParsingValue)? {
            b'"' => self.deserialize_str(visitor),
            b'[' => self.deserialize_seq(visitor),
            b'{' => self.deserialize_struct("ignored", &[], visitor),
            b',' | b'}' | b']' => Err(Error::ExpectedSomeValue),
            // If it’s something else then we chomp until we get to an end delimiter.
            // This does technically allow for illegal JSON since we’re just ignoring
            // characters rather than parsing them.
            _ => loop {
                match self.peek() {
                    // The visitor is expected to be UnknownAny’s visitor, which
                    // implements visit_unit to return its unit Ok result.
                    Some(b',') | Some(b'}') | Some(b']') => break visitor.visit_unit(),
                    Some(_) => self.eat_char(),
                    None => break Err(Error::EofWhileParsingString),
                }
            },
        }
    }
}

impl de::Error for Error {
    #[cfg_attr(not(feature = "custom-error-messages"), allow(unused_variables))]
    fn custom<T>(msg: T) -> Self
    where
        T: fmt::Display,
    {
        #[cfg(not(feature = "custom-error-messages"))]
        {
            Error::CustomError
        }
        #[cfg(feature = "custom-error-messages")]
        {
            use core::fmt::Write;

            let mut string = String::new();
            write!(string, "{:.64}", msg).unwrap();
            Error::CustomErrorWithMessage(string)
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Error::EofWhileParsingList => "EOF while parsing a list.",
                Error::EofWhileParsingObject => "EOF while parsing an object.",
                Error::EofWhileParsingString => "EOF while parsing a string.",
                Error::EofWhileParsingValue => "EOF while parsing a JSON value.",
                Error::ExpectedColon => "Expected this character to be a `':'`.",
                Error::ExpectedListCommaOrEnd => {
                    "Expected this character to be either a `','` or\
                     a \
                     `']'`."
                }
                Error::ExpectedObjectCommaOrEnd => {
                    "Expected this character to be either a `','` \
                     or a \
                     `'}'`."
                }
                Error::ExpectedSomeIdent => {
                    "Expected to parse either a `true`, `false`, or a \
                     `null`."
                }
                Error::ExpectedSomeValue => "Expected this character to start a JSON value.",
                Error::InvalidNumber => "Invalid number.",
                Error::InvalidType => "Invalid type",
                Error::InvalidUnicodeCodePoint => "Invalid unicode code point.",
                Error::KeyMustBeAString => "Object key is not a string.",
                Error::TrailingCharacters => {
                    "JSON has non-whitespace trailing characters after \
                     the \
                     value."
                }
                Error::TrailingComma => "JSON has a comma after the last value in an array or map.",
                Error::CustomError => "JSON does not match deserializer’s expected format.",
                #[cfg(feature = "custom-error-messages")]
                Error::CustomErrorWithMessage(msg) => msg.as_str(),
                _ => "Invalid JSON",
            }
        )
    }
}

/// Deserializes an instance of type `T` from bytes of JSON text
/// Returns the value and the number of bytes consumed in the process
pub fn from_slice<'a, T>(v: &'a [u8]) -> Result<T>
where
    T: de::Deserialize<'a>,
{
    let mut de = Deserializer::new(v);
    let value = de::Deserialize::deserialize(&mut de)?;
    de.end()?;

    Ok(value)
}

/// Deserializes an instance of type T from a string of JSON text
pub fn from_str<'a, T>(s: &'a str) -> Result<T>
where
    T: de::Deserialize<'a>,
{
    from_slice(s.as_bytes())
}

#[cfg(test)]
mod tests {
    use alloc::borrow::ToOwned;
    use serde_derive::Deserialize;

    #[derive(Debug, Deserialize, PartialEq)]
    enum Type {
        #[serde(rename = "boolean")]
        Boolean,
        #[serde(rename = "number")]
        Number,
        #[serde(rename = "thing")]
        Thing,
    }

    #[test]
    fn array() {
        assert_eq!(crate::from_str::<[i32; 0]>("[]"), Ok([]));
        assert_eq!(crate::from_str("[0, 1, 2]"), Ok([0, 1, 2]));

        // errors
        assert!(crate::from_str::<[i32; 2]>("[0, 1,]").is_err());
    }

    #[test]
    fn bool() {
        assert_eq!(crate::from_str("true"), Ok(true));
        assert_eq!(crate::from_str(" true"), Ok(true));
        assert_eq!(crate::from_str("true "), Ok(true));

        assert_eq!(crate::from_str("false"), Ok(false));
        assert_eq!(crate::from_str(" false"), Ok(false));
        assert_eq!(crate::from_str("false "), Ok(false));

        // errors
        assert!(crate::from_str::<bool>("true false").is_err());
        assert!(crate::from_str::<bool>("tru").is_err());
    }

    #[test]
    fn floating_point() {
        assert_eq!(crate::from_str("5.0"), Ok(5.0));
        assert_eq!(crate::from_str("1"), Ok(1.0));
        assert_eq!(crate::from_str("1e5"), Ok(1e5));
        assert!(crate::from_str::<f32>("a").is_err());
        assert!(crate::from_str::<f32>(",").is_err());
    }

    #[test]
    fn integer() {
        assert_eq!(crate::from_str("5"), Ok(5));
        assert_eq!(crate::from_str("101"), Ok(101));
        assert!(crate::from_str::<u16>("1e5").is_err());
        assert!(crate::from_str::<u8>("256").is_err());
        assert!(crate::from_str::<f32>(",").is_err());
    }

    #[test]
    fn enum_clike() {
        assert_eq!(crate::from_str(r#" "boolean" "#), Ok(Type::Boolean));
        assert_eq!(crate::from_str(r#" "number" "#), Ok(Type::Number));
        assert_eq!(crate::from_str(r#" "thing" "#), Ok(Type::Thing));
    }

    #[test]
    fn str() {
        assert_eq!(crate::from_str(r#" "hello" "#), Ok("hello"));
        assert_eq!(crate::from_str(r#" "" "#), Ok(""));
        assert_eq!(crate::from_str(r#" " " "#), Ok(" "));
        assert_eq!(crate::from_str(r#" "👏" "#), Ok("👏"));

        // no unescaping is done (as documented as a known issue in lib.rs)
        assert_eq!(crate::from_str(r#" "hel\tlo" "#), Ok("hel\tlo".to_owned()));
        assert_eq!(crate::from_str(r#" "hello \\" "#), Ok("hello \\".to_owned()));

        // escaped " in the string content
        assert_eq!(crate::from_str(r#" "foo\"bar" "#), Ok("foo\"bar".to_owned()));
        assert_eq!(crate::from_str(r#" "foo\\\"bar" "#), Ok("foo\\\"bar".to_owned()));
        assert_eq!(crate::from_str(r#" "foo\"\"bar" "#), Ok("foo\"\"bar".to_owned()));
        assert_eq!(crate::from_str(r#" "\"bar" "#), Ok("\"bar".to_owned()));
        assert_eq!(crate::from_str(r#" "foo\"" "#), Ok("foo\"".to_owned()));
        assert_eq!(crate::from_str(r#" "\"" "#), Ok("\"".to_owned()));

        // non-excaped " preceded by backslashes
        assert_eq!(crate::from_str(r#" "foo bar\\" "#), Ok("foo bar\\".to_owned()));
        assert_eq!(crate::from_str(r#" "foo bar\\\\" "#), Ok("foo bar\\\\".to_owned()));
        assert_eq!(
            crate::from_str(r#" "foo bar\\\\\\" "#),
            Ok("foo bar\\\\\\".to_owned())
        );
        assert_eq!(
            crate::from_str(r#" "foo bar\\\\\\\\" "#),
            Ok("foo bar\\\\\\\\".to_owned())
        );
        assert_eq!(crate::from_str(r#" "\\" "#), Ok("\\".to_owned()));
    }

    #[test]
    fn struct_bool() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Led {
            led: bool,
        }

        assert_eq!(crate::from_str(r#"{ "led": true }"#), Ok(Led { led: true }));
        assert_eq!(
            crate::from_str(r#"{ "led": false }"#),
            Ok(Led { led: false })
        );
    }

    #[test]
    fn struct_i8() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Temperature {
            temperature: i8,
        }

        assert_eq!(
            crate::from_str(r#"{ "temperature": -17 }"#),
            Ok(Temperature { temperature: -17 })
        );

        assert_eq!(
            crate::from_str(r#"{ "temperature": -0 }"#),
            Ok(Temperature { temperature: -0 })
        );

        assert_eq!(
            crate::from_str(r#"{ "temperature": 0 }"#),
            Ok(Temperature { temperature: 0 })
        );

        // out of range
        assert!(crate::from_str::<Temperature>(r#"{ "temperature": 128 }"#).is_err());
        assert!(crate::from_str::<Temperature>(r#"{ "temperature": -129 }"#).is_err());
    }

    #[test]
    fn struct_f32() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Temperature {
            temperature: f32,
        }

        assert_eq!(
            crate::from_str(r#"{ "temperature": -17.2 }"#),
            Ok(Temperature { temperature: -17.2 })
        );

        assert_eq!(
            crate::from_str(r#"{ "temperature": -0.0 }"#),
            Ok(Temperature { temperature: -0. })
        );

        assert_eq!(
            crate::from_str(r#"{ "temperature": -2.1e-3 }"#),
            Ok(Temperature {
                temperature: -2.1e-3
            })
        );

        assert_eq!(
            crate::from_str(r#"{ "temperature": -3 }"#),
            Ok(Temperature { temperature: -3. })
        );

        use core::f32;

        assert_eq!(
            crate::from_str(r#"{ "temperature": -1e500 }"#),
            Ok(Temperature {
                temperature: f32::NEG_INFINITY
            })
        );

        // NaNs will always compare unequal.
        let r: Temperature = crate::from_str(r#"{ "temperature": null }"#).unwrap();
        assert!(r.temperature.is_nan());

        assert!(crate::from_str::<Temperature>(r#"{ "temperature": 1e1e1 }"#).is_err());
        assert!(crate::from_str::<Temperature>(r#"{ "temperature": -2-2 }"#).is_err());
        assert!(crate::from_str::<Temperature>(r#"{ "temperature": 1 1 }"#).is_err());
        assert!(crate::from_str::<Temperature>(r#"{ "temperature": 0.0. }"#).is_err());
        assert!(crate::from_str::<Temperature>(r#"{ "temperature": ä }"#).is_err());
        assert!(crate::from_str::<Temperature>(r#"{ "temperature": None }"#).is_err());
    }

    #[test]
    fn struct_option() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Property<'a> {
            #[serde(borrow)]
            description: Option<&'a str>,
        }

        assert_eq!(
            crate::from_str(r#"{ "description": "An ambient temperature sensor" }"#),
            Ok(Property {
                description: Some("An ambient temperature sensor"),
            })
        );

        assert_eq!(
            crate::from_str(r#"{ "description": null }"#),
            Ok(Property { description: None })
        );

        assert_eq!(crate::from_str(r#"{}"#), Ok(Property { description: None }));
    }

    #[test]
    fn struct_u8() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Temperature {
            temperature: u8,
        }

        assert_eq!(
            crate::from_str(r#"{ "temperature": 20 }"#),
            Ok(Temperature { temperature: 20 })
        );

        assert_eq!(
            crate::from_str(r#"{ "temperature": 0 }"#),
            Ok(Temperature { temperature: 0 })
        );

        // out of range
        assert!(crate::from_str::<Temperature>(r#"{ "temperature": 256 }"#).is_err());
        assert!(crate::from_str::<Temperature>(r#"{ "temperature": -1 }"#).is_err());
    }

    #[test]
    fn test_unit() {
        assert_eq!(crate::from_str::<()>(r#"null"#), Ok(()));
    }

    #[test]
    fn newtype_struct() {
        #[derive(Deserialize, Debug, PartialEq)]
        struct A(pub u32);

        assert_eq!(crate::from_str::<A>(r#"54"#), Ok(A(54)));
    }

    #[test]
    fn test_newtype_variant() {
        #[derive(Deserialize, Debug, PartialEq)]
        enum A {
            A(u32),
        }
        let a = A::A(54);
        let x = crate::from_str::<A>(r#"{"A":54}"#);
        assert_eq!(x, Ok(a));
    }

    #[test]
    fn test_struct_variant() {
        #[derive(Deserialize, Debug, PartialEq)]
        enum A {
            A { x: u32, y: u16 },
        }
        let a = A::A { x: 54, y: 720 };
        let x = crate::from_str::<A>(r#"{"A": {"x":54,"y":720 } }"#);
        assert_eq!(x, Ok(a));
    }

    #[test]
    #[cfg(not(feature = "custom-error-messages"))]
    fn struct_tuple() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Xy(i8, i8);

        assert_eq!(crate::from_str(r#"[10, 20]"#), Ok(Xy(10, 8)));
        assert_eq!(crate::from_str(r#"[10, -20]"#), Ok(Xy(10, 9)));

        // wrong number of args
        assert_eq!(
            crate::from_str::<Xy>(r#"[10]"#),
            Err(crate::de::Error::CustomError)
        );
        assert_eq!(
            crate::from_str::<Xy>(r#"[10, 20, 30]"#),
            Err(crate::de::Error::TrailingCharacters)
        );
    }

    #[test]
    #[cfg(feature = "custom-error-messages")]
    fn struct_tuple() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Xy(i8, i8);

        assert_eq!(crate::from_str(r#"[10, 20]"#), Ok(Xy(10, 20)));
        assert_eq!(crate::from_str(r#"[10, -20]"#), Ok(Xy(10, -20)));

        // wrong number of args
        assert_eq!(
            crate::from_str::<Xy>(r#"[10]"#),
            Err(crate::de::Error::CustomErrorWithMessage(
                "invalid length 1, expected tuple struct Xy with 2 elements".into()
            ))
        );
        assert_eq!(
            crate::from_str::<Xy>(r#"[10, 20, 30]"#),
            Err(crate::de::Error::TrailingCharacters)
        );
    }

    #[test]
    fn ignoring_extra_fields() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Temperature {
            temperature: u8,
        }

        assert_eq!(
            crate::from_str(r#"{ "temperature": 20, "high": 80, "low": -10, "updated": true }"#),
            Ok(Temperature { temperature: 20 })
        );

        assert_eq!(
            crate::from_str(
                r#"{ "temperature": 20, "conditions": "windy", "forecast": "cloudy" }"#
            ),
            Ok(Temperature { temperature: 20 })
        );

        assert_eq!(
            crate::from_str(r#"{ "temperature": 20, "hourly_conditions": ["windy", "rainy"] }"#),
            Ok(Temperature { temperature: 20 })
        );

        assert_eq!(
            crate::from_str(
                r#"{ "temperature": 20, "source": { "station": "dock", "sensors": ["front", "back"] } }"#
            ),
            Ok(Temperature { temperature: 20 })
        );

        assert_eq!(
            crate::from_str(r#"{ "temperature": 20, "invalid": this-is-ignored }"#),
            Ok(Temperature { temperature: 20 })
        );

        assert_eq!(
            crate::from_str::<Temperature>(r#"{ "temperature": 20, "broken": }"#),
            Err(crate::de::Error::ExpectedSomeValue)
        );

        assert_eq!(
            crate::from_str::<Temperature>(r#"{ "temperature": 20, "broken": [ }"#),
            Err(crate::de::Error::ExpectedSomeValue)
        );

        assert_eq!(
            crate::from_str::<Temperature>(r#"{ "temperature": 20, "broken": ] }"#),
            Err(crate::de::Error::ExpectedSomeValue)
        );
    }

    #[test]
    #[cfg(feature = "custom-error-messages")]
    fn preserve_short_error_message() {
        use serde::de::Error;
        assert_eq!(
            crate::de::Error::custom("something bad happened"),
            crate::de::Error::CustomErrorWithMessage("something bad happened".into())
        );
    }

    #[test]
    #[cfg(feature = "custom-error-messages")]
    fn truncate_error_message() {
        use serde::de::Error;
        assert_eq!(
            crate::de::Error::custom("0123456789012345678901234567890123456789012345678901234567890123 <- after here the message should be truncated"),
            crate::de::Error::CustomErrorWithMessage(
                "0123456789012345678901234567890123456789012345678901234567890123".into()
            )
        );
    }

    // See https://iot.mozilla.org/wot/#thing-resource
    #[test]
    fn wot() {
        #[derive(Debug, Deserialize, PartialEq)]
        struct Thing<'a> {
            #[serde(borrow)]
            properties: Properties<'a>,
            #[serde(rename = "type")]
            ty: Type,
        }

        #[derive(Debug, Deserialize, PartialEq)]
        struct Properties<'a> {
            #[serde(borrow)]
            temperature: Property<'a>,
            #[serde(borrow)]
            humidity: Property<'a>,
            #[serde(borrow)]
            led: Property<'a>,
        }

        #[derive(Debug, Deserialize, PartialEq)]
        struct Property<'a> {
            #[serde(rename = "type")]
            ty: Type,
            unit: Option<&'a str>,
            #[serde(borrow)]
            description: Option<&'a str>,
            href: &'a str,
        }

        assert_eq!(
            crate::from_str::<Thing<'_>>(
                r#"
                    {
                    "type": "thing",
                    "properties": {
                        "temperature": {
                        "type": "number",
                        "unit": "celsius",
                        "description": "An ambient temperature sensor",
                        "href": "/properties/temperature"
                        },
                        "humidity": {
                        "type": "number",
                        "unit": "percent",
                        "href": "/properties/humidity"
                        },
                        "led": {
                        "type": "boolean",
                        "description": "A red LED",
                        "href": "/properties/led"
                        }
                    }
                    }
                    "#
            ),
            Ok(Thing {
                properties: Properties {
                    temperature: Property {
                        ty: Type::Number,
                        unit: Some("celsius"),
                        description: Some("An ambient temperature sensor"),
                        href: "/properties/temperature",
                    },
                    humidity: Property {
                        ty: Type::Number,
                        unit: Some("percent"),
                        description: None,
                        href: "/properties/humidity",
                    },
                    led: Property {
                        ty: Type::Boolean,
                        unit: None,
                        description: Some("A red LED"),
                        href: "/properties/led",
                    },
                },
                ty: Type::Thing,
            })
        )
    }
}
