use std::borrow::Cow;
use std::fmt::Display;

#[derive(Clone, Debug)]
enum InnerValue<'a> {
    Text(Cow<'a, str>),
    Number(f32),
}

#[derive(Clone, Debug)]
pub struct Value<'a> {
    inner: Cow<'a, InnerValue<'a>>,
}

impl<'a> Value<'a> {
    pub fn from_text<T: Into<Cow<'a, str>>>(text: T) -> Value<'a> {
        Value {
            inner: Cow::Owned(InnerValue::Text(text.into())),
        }
    }

    pub fn from_number(number: f32) -> Value<'a> {
        Value {
            inner: Cow::Owned(InnerValue::Number(number))
        }
    }
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.inner.as_ref() {
            InnerValue::Text(text) => write!(f, "{}", text),
            InnerValue::Number(number) => write!(f, "{}", number),
        }
    }
}
