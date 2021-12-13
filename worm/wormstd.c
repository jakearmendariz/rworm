/*
* Worm standard library
* very light weight, I made this to fit my immediate needs for array and stack operations
*/

fn max(int a, int b) -> int {
    if a > b {
        return a;
    } else {
        return b;
    }
}

/* stdlib testing */
fn std_checks(int a) -> string {
    int[] arr = [|i| i; 10];
    int[] arr2 = prepend(arr, 11);
    assert(arr2[0] == 11);
    return "SUCCESS";
}

/*
fn main() -> string {
    return std_checks(0);
}
*/