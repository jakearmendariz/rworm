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
    if i >= len(arr) {
        return 0;
    }
    return arr[i];
}

fn append(int[] arr, int a) -> int[] {
    int[] b = [|i| index_or_0(arr, i); len(arr)+1];
    b[len(arr)] = a;
    return b;
}

fn first(string s) -> char {
    return s[0];
}

/*
fn main() -> int[] {
    int[] arr = [|i| i; 10];
    int[] arr2 = append(arr, 11);
    print(arr2);
    char c = first("hello");
    return arr2;
}

*/