class Demo {
public:
    void show();
};


void Demo::show() {
    cout << "Hello from show()" << endl;
}

int main() {
    Demo d;    
    d.show();  
    return 0;  
}
