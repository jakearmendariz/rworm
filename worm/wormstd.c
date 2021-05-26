/*
* Worm standard library
*/

fn max(int a, int b) -> int {
    if a > b {
        return a;
    } else {
        return b;
    }
}

fn index_or_0(int[] arr, int i) -> int {
    if i >= len(arr) | i < 0 {
        return 0;
    }
    return arr[i];
}

fn append(int[] arr, int a) -> int[] {
    int[] b = [|i| index_or_0(arr, i); len(arr)+1];
    b[len(arr)] = a;
    return b;
}

fn prepend(int[] arr, int a) -> int[] {
    int[] b = [|i| index_or_0(arr, i-1); len(arr)+1];
    b[0] = a;
    return b;
}

fn first(string s) -> char {
    return s[0];
}

fn std_checks(int a) -> int {
    
    int[] arr = [|i| i; 10];
    int[] arr2 = prepend(arr, 11);
    int[] arr3 = append(arr, 11);
    assert(arr2[0] == 11);
    assert(arr3[10] == 11);
    char c = first("hello");
    return 0;
}
/*
fn main() -> int {
    return std_checks(0);
}
*/