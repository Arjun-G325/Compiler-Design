int main() {
    int i = 0;
    if (i < 10) {
        i++;
    } else {
        i--;
    }

    while (i < 5) {
        i = i + 1;
    }

    for (int j = 0; j < 10; j++) {
        i += j;
    }

    do {
        i--;
    } while (i > 0);

    switch(i) {
        case 1: i = 2; break;
        case 2: i = 3; break;
        default: i = 0;
    }

    goto label;
    i = 99;
label:
    return i;
}
