use std::sync::Mutex;

use lasso::{Rodeo, Spur};
use lazy_static::lazy_static;

lazy_static! {
    pub static ref RODEO: Mutex<Rodeo> = Mutex::new(Rodeo::new());
}

pub fn get_or_intern<T>(str: T) -> Spur where T: AsRef<str>  {
    RODEO.lock().unwrap().get_or_intern(str)
}

pub fn resolve(spur: Spur) -> String {
    let rodeo = RODEO.lock().unwrap();
    // the fact that this becomes an owned string
    // feels like its missing the point
    // how can i prove to the compiler that RODEO owns the string?
    rodeo.resolve(&spur).to_string()
}