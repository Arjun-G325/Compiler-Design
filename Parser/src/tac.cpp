#include "tac.h"
#include <iostream>

TACGenerator* tac_gen = nullptr;

TACGenerator::TACGenerator(const std::string& filename) 
    : temp_count(0), label_count(0) {
    outfile.open(filename);
    if (!outfile.is_open()) {
        std::cerr << "Error: Cannot open TAC output file: " << filename << std::endl;
        exit(1);
    }
}

TACGenerator::~TACGenerator() {
    if (outfile.is_open()) {
        outfile.close();
    }
}

std::string TACGenerator::newTemp() {
    return "t" + std::to_string(temp_count++);
}

std::string TACGenerator::newLabel() {
    return "L" + std::to_string(label_count++);
}

void TACGenerator::emit(const std::string& result, const std::string& arg1, 
                        const std::string& op, const std::string& arg2) {
    outfile << result << " = " << arg1 << " " << op << " " << arg2 << std::endl;
}

void TACGenerator::emitAssignment(const std::string& result, const std::string& value) {
    outfile << result << " = " << value << std::endl;
}

void TACGenerator::emitUnary(const std::string& result, const std::string& op, 
                             const std::string& arg) {
    outfile << result << " = " << op << arg << std::endl;
}

void TACGenerator::emitLabel(const std::string& label) {
    outfile << label << ":" << std::endl;
}

void TACGenerator::emitGoto(const std::string& label) {
    outfile << "goto " << label << std::endl;
}

void TACGenerator::emitIfGoto(const std::string& condition, const std::string& label) {
    outfile << "if " << condition << " goto " << label << std::endl;
}

void TACGenerator::emitIfFalseGoto(const std::string& condition, const std::string& label) {
    outfile << "ifFalse " << condition << " goto " << label << std::endl;
}

void TACGenerator::emitParam(const std::string& param) {
    outfile << "param " << param << std::endl;
}

void TACGenerator::emitCall(const std::string& result, const std::string& function, 
                            int num_params) {
    if (result.empty()) {
        outfile << "call " << function << ", " << num_params << std::endl;
    } else {
        outfile << result << " = call " << function << ", " << num_params << std::endl;
    }
}

void TACGenerator::emitReturn(const std::string& value) {
    outfile << "return " << value << std::endl;
}

void TACGenerator::emitReturnVoid() {
    outfile << "return" << std::endl;
}

void TACGenerator::emitFunctionBegin(const std::string& function_name) {
    outfile << "\nBeginFunc " << function_name << std::endl;
}

void TACGenerator::emitFunctionEnd(const std::string& function_name) {
    outfile << "EndFunc " << function_name << "\n" << std::endl;
}

void TACGenerator::emitArrayAccess(const std::string& result, const std::string& array, 
                                   const std::string& index) {
    outfile << result << " = " << array << "[" << index << "]" << std::endl;
}

void TACGenerator::emitArrayStore(const std::string& array, const std::string& index, 
                                  const std::string& value) {
    outfile << array << "[" << index << "] = " << value << std::endl;
}

void TACGenerator::emitAddressOf(const std::string& result, const std::string& var) {
    outfile << result << " = &" << var << std::endl;
}

void TACGenerator::emitDereference(const std::string& result, const std::string& ptr) {
    outfile << result << " = *" << ptr << std::endl;
}

void TACGenerator::emitPointerStore(const std::string& ptr, const std::string& value) {
    outfile << "*" << ptr << " = " << value << std::endl;
}

void TACGenerator::emitMemberAccess(const std::string& result, const std::string& struct_var, 
                                   const std::string& member) {
    outfile << result << " = " << struct_var << "." << member << std::endl;
}

void TACGenerator::emitPointerMemberAccess(const std::string& result, 
                                          const std::string& struct_ptr, 
                                          const std::string& member) {
    outfile << result << " = " << struct_ptr << "->" << member << std::endl;
}

void TACGenerator::emitCast(const std::string& result, const std::string& value, 
                           const std::string& type) {
    outfile << result << " = (" << type << ") " << value << std::endl;
}

void TACGenerator::emitComment(const std::string& comment) {
    outfile << "// " << comment << std::endl;
}

void TACGenerator::emitRaw(const std::string& instruction) {
    outfile << instruction << std::endl;
}
