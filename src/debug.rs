#[macro_export]
macro_rules! dprint {
    ($($arg:tt)*) => {
        if *crate::args::ARGS.debug {
            print!($($arg)*);
        };
    };
}

#[macro_export]
macro_rules! dprintln {
    () => {
        if crate::args::ARGS.debug.to_value() {
            println!();
        }
    };
    ($($arg:tt)*) => {
        if crate::args::ARGS.debug.to_value() {
            println!($($arg)*);
        }
    };
}
