import "worm/wormstd.c";

fn to_binary(int num) -> int[] {
    int[] res = [0; 0];
    int index = 31;
    while num > 0 ∧ index >= 0 {
        int val = 2 ^ index;
        if num >= val {
            num = num - val;
            res = int_append(res, 1);
        }else {
            res = int_append(res, 0);
        }
        index = index -1;
    }
    return res;
}

fn to_bin_str(int num) -> string {
    string res = "";
    int index = 31;
    /* This is off bc we are counting up instead of down #oops*/
    while num > 0 ∧ index >= 0 {
        int val = 2 ^ index;
        if num > val {
            num = num - val;
            res = res + '1';
        } else {
            res = res + '0';
        }
        index = index -1;
    }
    return res;
}

fn from_binary(int[] bin) -> int {
    if(len(bin) != 32) {
        return 0; /* empty array */
    }
    int val = 2 ^ 31;
    int res = 0;
    int index = 31;
    while(index >= 0) {
        val = 2 ^ index;
        if(bin[31 - index] == 1) {
            res = res + val;
        }
        index = index - 1;
    }
    return res;
}
/* NOTE static anaylsis can't detect missing return statement route */


fn main() -> int[] {
    int input = 18493;
    int[] bin = to_binary(input);
    int i = 0;
    int x = 5;
    print(8*7+x*4-6);
    string res = to_bin_str(input);
    int val = from_binary(bin);
    assert(val == input);
    return bin;
}