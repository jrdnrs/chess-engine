/// Finds the largest value and places it at index 0\
///
/// *compares the value of* `i32` *in* `(i32, usize)`
pub fn selection_sort_once(evals: &mut [(i32, usize)]) {
    let mut largest_index = 0;
    let mut largest = evals[0].0;
    for (i, (value, _)) in evals.iter().enumerate() {
        if *value > largest {
            largest_index = i;
            largest = *value;
        }
    }
    evals.swap(0, largest_index);
}
