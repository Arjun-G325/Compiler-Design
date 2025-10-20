#ifndef TAC_H
#define TAC_H

#include <string>
#include <vector>
#include <fstream>

class TACGenerator {
private:
    std::ofstream outfile;
    int temp_count;
    int label_count;
    
public:
    TACGenerator(const std::string& filename);
    ~TACGenerator();
    
    // Temporary variable generation
    std::string newTemp();
    
    // Label generation
    std::string newLabel();
    
    // Basic TAC operations
    void emit(const std::string& result, const std::string& arg1, const std::string& op, const std::string& arg2);
    void emitAssignment(const std::string& result, const std::string& value);
    void emitUnary(const std::string& result, const std::string& op, const std::string& arg);
    
    // Control flow
    void emitLabel(const std::string& label);
    void emitGoto(const std::string& label);
    void emitIfGoto(const std::string& condition, const std::string& label);
    void emitIfFalseGoto(const std::string& condition, const std::string& label);
    
    // Function operations
    void emitParam(const std::string& param);
    void emitCall(const std::string& result, const std::string& function, int num_params);
    void emitReturn(const std::string& value);
    void emitReturnVoid();
    void emitFunctionBegin(const std::string& function_name);
    void emitFunctionEnd(const std::string& function_name);
    
    // Array operations
    void emitArrayAccess(const std::string& result, const std::string& array, const std::string& index);
    void emitArrayStore(const std::string& array, const std::string& index, const std::string& value);
    
    // Pointer operations
    void emitAddressOf(const std::string& result, const std::string& var);
    void emitDereference(const std::string& result, const std::string& ptr);
    void emitPointerStore(const std::string& ptr, const std::string& value);
    
    // Structure operations
    void emitMemberAccess(const std::string& result, const std::string& struct_var, const std::string& member);
    void emitPointerMemberAccess(const std::string& result, const std::string& struct_ptr, const std::string& member);
    
    // Type casting
    void emitCast(const std::string& result, const std::string& value, const std::string& type);
    
    // Comments for readability
    void emitComment(const std::string& comment);
    
    // Raw emit
    void emitRaw(const std::string& instruction);
};

extern TACGenerator* tac_gen;

#endif // TAC_H
