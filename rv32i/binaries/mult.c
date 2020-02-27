int mult(int a, int b) {
    int c = 0;
    while (a > 0) {
        c += b;
        a -= 1;
    }
    return c;
}

int main() {
    int a = 5, b = 3;
    int c = mult(a, b);
}

// void __register_exitproc(void) { }
// void __libc_init_array(void) { }
