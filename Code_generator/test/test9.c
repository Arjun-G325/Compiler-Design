struct Address {
    char city[20];
    int pincode;
};

struct Student {
    char name[20];
    int roll;
    struct Address node;
};

int main() {
    struct Student s1 = {"Alice", 101, {"Roorkee", 247667}};
    struct Student *ptr = &s1;
    return ptr->node->roll;
}