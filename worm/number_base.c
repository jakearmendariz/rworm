

fn to_binary(int num) -> int[] {
    int[] res = [0; 32];
    int index = 31;
    while num > 0 ∧ index >= 0 {
        print(index);
        int val = 2 ^ index;
        if num > val {
            num = num - val;
            res[index] = 1;
        }
        index = index -1;
    }
    return res;
}


fn from_binary(int[] bin) -> int[] {
    
}

fn main() -> int[] {
    int[] arr = to_binary(18493);
    print(arr);
    return arr;
}