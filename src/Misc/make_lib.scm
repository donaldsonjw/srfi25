(module srfi25_make_lib
   (import srfi25/store
           srfi25/shape
           srfi25/apply
           srfi25/array-base
           srfi25/array
           srfi25/s8array
           srfi25/u8array
           srfi25/s16array
           srfi25/u16array
           srfi25/s32array
           srfi25/u32array
           srfi25/s64array
           srfi25/u64array
           srfi25/f32array
           srfi25/f64array)
   (eval (export-all)))