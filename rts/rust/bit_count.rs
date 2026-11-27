#![no_std]

// The subword primops accept a full machine word. Narrow the operand before
// counting so that bits above the requested width do not affect the result.
macro_rules! bit_counts {
    ($argument:ty, $width:ty, $population:ident, $leading:ident, $trailing:ident) => {
        #[unsafe(no_mangle)]
        pub extern "C" fn $population(value: $argument) -> usize {
            (value as $width).count_ones() as usize
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn $leading(value: $argument) -> usize {
            (value as $width).leading_zeros() as usize
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn $trailing(value: $argument) -> usize {
            (value as $width).trailing_zeros() as usize
        }
    };
}

bit_counts!(usize, u8, hs_popcnt8, hs_clz8, hs_ctz8);
bit_counts!(usize, u16, hs_popcnt16, hs_clz16, hs_ctz16);
bit_counts!(usize, u32, hs_popcnt32, hs_clz32, hs_ctz32);
bit_counts!(u64, u64, hs_popcnt64, hs_clz64, hs_ctz64);

#[unsafe(no_mangle)]
pub extern "C" fn hs_popcnt(value: usize) -> usize {
    value.count_ones() as usize
}
