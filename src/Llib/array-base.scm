(module srfi25/array-base
   (export      
      (abstract-class %array-base
         shared::bool
         lengthv::vector
         startv::vector
         endv::vector
         scratch::vector)
      (final-class %array::%array-base
         coeff::vector
         store)
      (final-class %shared-array::%array-base
         constants::vector
         coeffs::vector
         sscratch::vector
         backing::%array-base)))
