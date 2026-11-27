#![no_std]

/// Decode an IEEE binary32 value into GHC's signed mantissa and exponent.
///
/// # Safety
///
/// Both output pointers must be aligned and valid for writes of one RTS word.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ghc_rust_decode_float_int(
    mantissa: *mut isize,
    exponent: *mut isize,
    value: f32,
) {
    let bits = value.to_bits();
    let fraction = bits & 0x007f_ffff;
    let biased_exponent = ((bits >> 23) & 0xff) as isize;

    let (unsigned_mantissa, decoded_exponent) = match (biased_exponent, fraction) {
        (0, 0) => (0, 0),
        (0, _) => {
            // Shift the leading bit of a subnormal fraction to bit 23.
            let shift = fraction.leading_zeros() - 8;
            (fraction << shift, -149 - shift as isize)
        }
        _ => (fraction | 0x0080_0000, biased_exponent - 150),
    };
    let signed_mantissa = if bits >> 31 == 0 {
        unsigned_mantissa as isize
    } else {
        -(unsigned_mantissa as isize)
    };

    unsafe {
        mantissa.write(signed_mantissa);
        exponent.write(decoded_exponent);
    }
}
