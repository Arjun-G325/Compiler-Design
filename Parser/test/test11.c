class Demo {
public:
   
    Demo() {
        cout << "Constructor called" << endl;
    }

   
    ~Demo() {
        cout << "Destructor called" << endl;
    }

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
