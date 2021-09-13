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

/* Integer operators */
fn int_get_val(int[] arr, int i) -> int {
    if i >= len(arr) | i < 0 {
        return -1;
    }
    return arr[i];
}

fn int_append(int[] arr, int a) -> int[] {
    int[] b = [|i| int_get_val(arr, i); len(arr)+1];
    b[len(arr)] = a;
    return b;
}

fn prepend(int[] arr, int a) -> int[] {
    int[] b = [|i| int_get_val(arr, i-1); len(arr)+1];
    b[0] = a;
    return b;
}

/* character operations */
fn char_get_val(char[] arr, int i) -> char {
    if i >= len(arr) | i < 0 {
        return ' ';
    }
    return arr[i];
}

fn push_char(char[] arr, char a) -> char[] {
    char[] b = [|i| char_get_val(arr, i); len(arr)+1];
    b[len(arr)] = a;
    return b;
}
    /* character stack operators */
fn prepend_char(char[] arr, char a) -> char[] {
    char[] b = [|i| char_get_val(arr, i-1); len(arr)+1];
    b[0] = a;
    return b;
}

fn peekchar(char[] arr) -> char {
    return arr[len(arr) -1];
}

fn popchar(char[] arr) -> char[] {
    char[] b = [|i| arr[i]; len(arr) - 1];
    return b;
}

fn pop_left(char[] arr) -> char[] {
    char[] b = [|i| arr[i+1]; len(arr) - 1];
    return b;
}

/* map operations */
fn map_get_val(map<string, int>[] arr, int i) -> map<string, int> {
    if i >= len(arr) | i < 0 {
        return {string:int};
    }
    return arr[i];
}

fn push_map(map<string, int>[] arr, map<string, int> a) -> map<string, int>[] {
    map<string, int>[] b = [|i| map_get_val(arr, i); len(arr)+1];
    b[len(arr)] = a;
    return b;
}

fn pop_map(map<string, int>[] arr) -> map<string, int>[] {
    map<string, int>[] b = [|i| arr[i]; len(arr)-1];
    return b;
}

/* string ops */
fn pop_str(string arr) -> string {
    string s = "";
    int i = 0;
    while i < len(arr)-1 {
        s = s + arr[i];
        i = i + 1;
    }
    return s;
}

/* stdlib testing */
fn std_checks(int a) -> string {
    
    int[] arr = [|i| i; 10];
    int[] arr2 = prepend(arr, 11);
    int[] arr3 = int_append(arr, 11);
    assert(arr2[0] == 11);
    assert(arr3[10] == 11);
    return "SUCCESS";
}

/*
fn main() -> string {
    return std_checks(0);
}
*/