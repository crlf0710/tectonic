#[macro_export]
macro_rules! no_mangle_extern_fn {
    ($(
        $(#[$meta:meta])*
        $v:vis fn $symbol:ident => $extern_symbol:ident($($argname:ident: $argtype:ty),*)
                                  -> $rettype:ty;
    )*) => {
        extern "C" {
            $(
                //#[no_mangle]
                $(#[$meta])*
                fn $extern_symbol($($argname: $argtype),*) -> $rettype;
            )*
        }
    };
}
#[macro_export]
macro_rules! forward_stub_fn {
    ($(
        $(#[$meta:meta])*
        $v:vis fn $symbol:ident => $extern_symbol:ident($($argname:ident: $argtype:ty),*)
                                  -> $rettype:ty;
    )*) => {
        $(
            #[allow(unused_variables)]
            $(#[$meta])*
            $v unsafe fn $symbol($($argname: $argtype),*) -> $rettype {
                $extern_symbol($($argname),*)
            }
        )*
    };
}
#[macro_export]
macro_rules! extern_and_forward_stub {
    ($(
        $(#[$meta:meta])*
        $v:vis fn $symbol:ident => $extern_symbol:ident($($argname:ident: $argtype:ty),*)
                                  -> $rettype:ty;
    )*) => {
        no_mangle_extern_fn!($(
            $(#[$meta])*
            $v fn $symbol => $extern_symbol($($argname : $argtype),*)
                                    -> $rettype;
        )*);
        forward_stub_fn!($(
            $(#[$meta])*
            $v fn $symbol => $extern_symbol($($argname : $argtype),*)
                                    -> $rettype;
        )*);
    };
}
