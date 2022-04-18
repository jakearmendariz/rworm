import "worm/wormstd.c";

fn to_binary(int num) -> int[] {
    int[] res = [int];
    int index = 30;
    while num >= 0 & index >= 0 {
        int val = 2 ^ index;
        if num >= val {
            num = num - val;
            res = append(res, 1);
        }else {
            res = append(res, 0);
        }
        /*
        print(res);
        print(num);
        print(index);
        */
        index = index -1;
    }
    return res;
}

fn to_bin_str(int num) -> string {
    string res = "";
    int index = 30;
    /* This is off bc we are counting up instead of down #oops*/
    while num >= 0 & index >= 0 {
        int val = 2 ^ index;
        if num >= val {
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
    if(len(bin) != 31) {
        return 0; /* empty array */
    }
    int val = 1;
    int res = 0;
    int index = 0;
    while(index < 31) {
        val = 2 ^ index;
        if(bin[30-index] == 1) {
            res = res + val;
        }
        index = index + 1;
    }
    return res;
}
/* NOTE static anaylsis can't detect missing return statement route */


fn main() -> int {
    int i = 0;
    while (i < 10000) {
        int input = i;
        int[] bin = to_binary(input);
        int val = from_binary(bin);
        /* assert(val == input); */
        if(val != input) {
            return -1;
        }
        i = i + 1;
    }
    return 0;
}