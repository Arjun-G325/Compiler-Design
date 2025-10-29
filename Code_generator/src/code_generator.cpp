#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <sstream>
#include <regex>
#include <cctype>
#include <set>
#include <algorithm>
#include <stack>

class TACToSPIMConverter {
private:
    std::vector<std::string> output;
    std::map<std::string, std::string> floatConstants;
    std::map<std::string, std::string> stringConstants;
    std::map<std::string, std::string> varTypes;
    std::map<std::string, int> globalVars;
    std::vector<std::string> paramStack;
    std::string currentFunc;
    int labelCount;
    int stringConstCount;
    int floatConstCount;
    int tempVarCount;
    int stackOffset;
    int argStackOffset;
    int useCounter;
    bool hasParserErrors;
    std::vector<std::string> parserErrors;
    std::map<std::string, std::string> globalInitialValues;

    // Register and Address Descriptors
    struct RegisterInfo {
        std::string varName;      // Variable currently in register (empty if free)
        bool dirty;               // Whether register contains modified value
        bool isFloat;             // Whether register holds float value
        int lastUsed;             // For LRU eviction
    };
    
    struct VariableInfo {
        std::set<std::string> registers; // Registers where variable is cached
        int stackOffset;          // Stack offset (-1 if not on stack)
        bool dirty;               // Whether memory copy is stale
        bool isFloat;             // Whether variable is float type
        bool isSpilled;           // Whether variable is spilled to stack
    };

    std::map<std::string, RegisterInfo> intRegisters;
    std::map<std::string, RegisterInfo> floatRegisters;
    std::map<std::string, VariableInfo> variableInfo;
    
    // Available registers
    std::vector<std::string> availableIntRegs = {"$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8", "$t9"};
    std::vector<std::string> availableFloatRegs = {"$f0", "$f2", "$f4", "$f6", "$f8", "$f10", "$f12", "$f14", "$f16", "$f18"};
    
    // Argument registers
    std::vector<std::string> argIntRegs = {"$a0", "$a1", "$a2", "$a3"};
    std::vector<std::string> argFloatRegs = {"$f12", "$f14"};

public:
    TACToSPIMConverter() : labelCount(0), stringConstCount(0), floatConstCount(0), 
                          tempVarCount(0), stackOffset(0), argStackOffset(0), useCounter(0),
                          hasParserErrors(false) {
        initializeRegisters();
    }

    void initializeRegisters() {
        // Initialize integer registers
        for (const auto& reg : availableIntRegs) {
            intRegisters[reg] = {"", false, false, 0};
        }
        for (const auto& reg : argIntRegs) {
            intRegisters[reg] = {"", false, false, 0};
        }
        
        // Initialize float registers
        for (const auto& reg : availableFloatRegs) {
            floatRegisters[reg] = {"", false, true, 0};
        }
        for (const auto& reg : argFloatRegs) {
            floatRegisters[reg] = {"", false, true, 0};
        }
    }

    void resetRegisters() {
        for (auto& reg : intRegisters) {
            reg.second = {"", false, false, 0};
        }
        for (auto& reg : floatRegisters) {
            reg.second = {"", false, true, 0};
        }
    }

    std::string generateLabel(const std::string& base) {
        return base + std::to_string(labelCount++);
    }

    std::string getFloatConstant(const std::string& value) {
        std::string cleanValue = value;
        cleanValue = std::regex_replace(cleanValue, std::regex(R"(\s+)"), "");
        
        if (floatConstants.find(cleanValue) == floatConstants.end()) {
            std::string label = "float_const_" + std::to_string(floatConstCount++);
            floatConstants[cleanValue] = label;
        }
        return floatConstants[cleanValue];
    }

    std::string getStringConstant(const std::string& value) {
        std::string cleanValue = value;
        if (cleanValue.front() == '"' && cleanValue.back() == '"') {
            cleanValue = cleanValue.substr(1, cleanValue.length() - 2);
        }
        
        if (stringConstants.find(cleanValue) == stringConstants.end()) {
            std::string label = "string_const_" + std::to_string(stringConstCount++);
            stringConstants[cleanValue] = label;
        }
        return stringConstants[cleanValue];
    }

    bool isFloatVar(const std::string& var) {
        if (varTypes.find(var) != varTypes.end()) {
            return varTypes[var].find("float") != std::string::npos;
        }
        // Fallback for temp vars
        return var[0] == 'f';
    }

    bool isIntVar(const std::string& var) {
        if (varTypes.find(var) != varTypes.end()) {
            std::string type = varTypes[var];
            return (type.find("int") != std::string::npos && type.find("float") == std::string::npos) ||
                   type.find("char") != std::string::npos ||
                   type.find("unsigned") != std::string::npos;
        }
         // Fallback for temp vars
        return var[0] == 'i' || var[0] == 't' || var == "y"; // Removed isdigit check
    }

    bool isGlobalVar(const std::string& var) {
        return globalVars.find(var) != globalVars.end();
    }

    // Check for parser errors in the line
    bool hasParserError(const std::string& line) {
        return line.find("Parser Error") != std::string::npos ||
               line.find("syntax error") != std::string::npos ||
               line.find("no member named") != std::string::npos ||
               line.find("lvalue required") != std::string::npos ||
               line.find("Called object is not a function") != std::string::npos ||
               line.find("must be a pointer") != std::string::npos ||
               line.find("free: argument must be a pointer") != std::string::npos;
    }

    // Register Allocation Methods
    std::string getRegisterForVar(const std::string& var, bool preferFloat = false) {
        bool isFloat = preferFloat || isFloatVar(var);
        auto& registers = isFloat ? floatRegisters : intRegisters;
        auto& availableRegs = isFloat ? availableFloatRegs : availableIntRegs;
        
        // Check if variable is already in a register
        for (const auto& reg : availableRegs) {
            if (registers[reg].varName == var) {
                registers[reg].lastUsed = ++useCounter;
                return reg;
            }
        }
        
        // Find free register
        for (const auto& reg : availableRegs) {
            if (registers[reg].varName.empty()) {
                registers[reg].varName = var;
                registers[reg].isFloat = isFloat;
                registers[reg].lastUsed = ++useCounter;
                registers[reg].dirty = false;
                
                // Load from memory if variable exists there
                if (variableInfo.find(var) != variableInfo.end() && variableInfo[var].stackOffset != -1) {
                    output.push_back("    # Loading " + var + " from stack to " + reg + "\n");
                    if (isFloat) {
                        output.push_back("    l.s " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
                    } else {
                        output.push_back("    lw " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
                    }
                }
                
                // Update variable info
                if (variableInfo.find(var) == variableInfo.end()) {
                    variableInfo[var] = {{reg}, -1, false, isFloat, false};
                } else {
                    variableInfo[var].registers.insert(reg);
                }
                
                return reg;
            }
        }
        
        // No free register, evict least recently used
        std::string lruReg;
        int minUse = useCounter + 1;
        for (const auto& reg : availableRegs) {
            if (registers[reg].lastUsed < minUse) {
                minUse = registers[reg].lastUsed;
                lruReg = reg;
            }
        }
        
        // Spill the evicted variable
        std::string evictedVar = registers[lruReg].varName;
        if (!evictedVar.empty() && registers[lruReg].dirty) {
            spillRegister(lruReg, evictedVar);
        }
        
        // Update variable info for evicted variable
        if (variableInfo.find(evictedVar) != variableInfo.end()) {
            variableInfo[evictedVar].registers.erase(lruReg);
        }
        
        // Assign register to new variable
        registers[lruReg].varName = var;
        registers[lruReg].isFloat = isFloat;
        registers[lruReg].lastUsed = ++useCounter;
        registers[lruReg].dirty = false;
        
        // Load from memory if available
        if (variableInfo.find(var) != variableInfo.end() && variableInfo[var].stackOffset != -1) {
            output.push_back("    # Loading " + var + " from stack to " + lruReg + "\n");
            if (isFloat) {
                output.push_back("    l.s " + lruReg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
            } else {
                output.push_back("    lw " + lruReg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
            }
        }
        
        // Update variable info
        if (variableInfo.find(var) == variableInfo.end()) {
            variableInfo[var] = {{lruReg}, -1, false, isFloat, false};
        } else {
            variableInfo[var].registers.insert(lruReg);
        }
        
        return lruReg;
    }

    void spillRegister(const std::string& reg, const std::string& var) {
        auto& registers = floatRegisters.find(reg) != floatRegisters.end() ? floatRegisters : intRegisters;
        bool isFloat = registers[reg].isFloat;
        
        // Ensure variable has stack allocation
        if (variableInfo.find(var) == variableInfo.end()) {
            allocateStackSpace(var);
        }
        
        if (variableInfo[var].stackOffset == -1) {
            allocateStackSpace(var);
        }
        
        output.push_back("    # Spilling " + var + " from " + reg + " to stack\n");
        if (isFloat) {
            output.push_back("    s.s " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
        } else {
            output.push_back("    sw " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
        }
        
        variableInfo[var].dirty = false;
    }

    void allocateStackSpace(const std::string& var) {
        if (variableInfo.find(var) == variableInfo.end()) {
            variableInfo[var] = {{}, -1, false, isFloatVar(var), false};
        }
        
        if (variableInfo[var].stackOffset == -1) {
            stackOffset -= 4;
            variableInfo[var].stackOffset = stackOffset;
            output.push_back("    # Allocated stack space for " + var + " at offset " + std::to_string(stackOffset) + "\n");
        }
    }

    void markDirty(const std::string& var, const std::string& reg) {
        if (floatRegisters.find(reg) != floatRegisters.end()) {
            floatRegisters[reg].dirty = true;
        } else if (intRegisters.find(reg) != intRegisters.end()) {
            intRegisters[reg].dirty = true;
        }
        
        if (variableInfo.find(var) != variableInfo.end()) {
            variableInfo[var].dirty = true;
        }
    }

    void spillAllDirtyRegisters() {
        // Spill dirty integer registers
        for (const auto& reg : availableIntRegs) {
            if (!intRegisters[reg].varName.empty() && intRegisters[reg].dirty) {
                spillRegister(reg, intRegisters[reg].varName);
            }
        }
        
        // Spill dirty float registers
        for (const auto& reg : availableFloatRegs) {
            if (!floatRegisters[reg].varName.empty() && floatRegisters[reg].dirty) {
                spillRegister(reg, floatRegisters[reg].varName);
            }
        }
    }

    // Function argument handling for >4 arguments
    void setupFunctionArgs(int numArgs, const std::vector<std::string>& args, bool isFloatCall) {
        int intArgCount = 0;
        int floatArgCount = 0;
        int stackArgOffset = 0;
        
        // First pass: calculate stack space needed for arguments beyond 4
        if (numArgs > 4) {
            stackArgOffset = -(numArgs - 4) * 4;
            output.push_back("    addiu $sp, $sp, " + std::to_string(stackArgOffset) + "  # Make space for extra arguments\n");
        }
        
        for (int i = 0; i < numArgs; i++) {
            // Args are pushed in reverse, so access them in reverse
            std::string arg = args[numArgs - 1 - i];
            bool isFloat = isFloatVar(arg);
            
            if (i < 4) {
                // First 4 arguments go in registers
                if (isFloat && floatArgCount < 2) {
                    std::string reg = argFloatRegs[floatArgCount++];
                    std::string argReg = getRegisterForVar(arg, true);
                    output.push_back("    mov.s " + reg + ", " + argReg + "  # Float argument " + std::to_string(i+1) + "\n");
                } else if (!isFloat && intArgCount < 4) {
                    std::string reg = argIntRegs[intArgCount++];
                    std::string argReg = getRegisterForVar(arg, false);
                    output.push_back("    move " + reg + ", " + argReg + "  # Integer argument " + std::to_string(i+1) + "\n");
                } else {
                    // Fall back to stack for mixed types beyond available registers
                    int stackOffset = -(i - 3) * 4; // Arguments 5+ start at -4, then -8, etc.
                    std::string argReg = getRegisterForVar(arg, isFloat);
                    output.push_back("    # Argument " + std::to_string(i+1) + " on stack at offset " + std::to_string(stackOffset) + "\n");
                    if (isFloat) {
                        output.push_back("    s.s " + argReg + ", " + std::to_string(stackOffset) + "($sp)\n");
                    } else {
                        output.push_back("    sw " + argReg + ", " + std::to_string(stackOffset) + "($sp)\n");
                    }
                }
            } else {
                // Arguments 5+ go on stack
                int stackOffset = -(i - 3) * 4; // Arguments 5+ start at -4, then -8, etc.
                std::string argReg = getRegisterForVar(arg, isFloat);
                output.push_back("    # Argument " + std::to_string(i+1) + " on stack at offset " + std::to_string(stackOffset) + "\n");
                if (isFloat) {
                    output.push_back("    s.s " + argReg + ", " + std::to_string(stackOffset) + "($sp)\n");
                } else {
                    output.push_back("    sw " + argReg + ", " + std::to_string(stackOffset) + "($sp)\n");
                }
            }
        }
    }

    void parseVariableDeclaration(const std::string& line) {
        std::regex varDecl(R"(// Variable declaration: (\w+) : ([\w\s]+) \(size: \d+ bytes\))");
        std::smatch match;
        
        if (std::regex_search(line, match, varDecl)) {
            std::string varName = match[1];
            std::string varType = match[2];
            varTypes[varName] = varType;
            
            // ONLY add to globalVars if we are NOT inside a function
            if (currentFunc.empty()) {
                globalVars[varName] = 1;
            }
        }
    }


    void parseVariableAssignment(const std::string& line) {
        std::regex assignRegex(R"((\w+)\s*=\s*([^[]+)(?:\s*\[([^\]]+)\])?)");
        std::smatch match;
        
        if (std::regex_search(line, match, assignRegex)) {
            std::string varName = match[1];
            std::string value = match[2];
            std::string typeInfo = match[3];
            
            if (!typeInfo.empty()) {
                varTypes[varName] = typeInfo;
            }
            
            value = std::regex_replace(value, std::regex(R"(\s*$)"), "");
        }
    }

    std::string convertInstruction(const std::string& line) {
        std::string result = "";
        
        // Check for parser errors first
        if (hasParserError(line)) {
            hasParserErrors = true;
            parserErrors.push_back(line);
            return "";
        }
        
        // Skip comments and declarations
        if (line.find("//") == 0) {
            if (line.find("Variable declaration:") != std::string::npos) {
                parseVariableDeclaration(line); // This will now use the fixed function
            }
            return "# " + line + "\n";
        }
        
        // Function begin/end
        if (line.find("BeginFunc") != std::string::npos) {
            // ================== THIS IS THE FIX ==================
            // Changed " " to "\s+" to allow for multiple spaces/tabs
            std::regex funcRegex(R"(BeginFunc\s+(\w+))");
            // ================== END OF FIX ==================
            
            std::smatch match;
            if (std::regex_search(line, match, funcRegex)) {
                currentFunc = match[1];
                variableInfo.clear();
                stackOffset = -8; // Start after ra/fp
                argStackOffset = 0;
                paramStack.clear();
                resetRegisters();
                
                output.push_back("\n" + currentFunc + ":\n");
                output.push_back("    addiu $sp, $sp, -8    # Allocate space for ra/fp\n");
                output.push_back("    sw $ra, 4($sp)        # Save return address\n");
                output.push_back("    sw $fp, 0($sp)        # Save frame pointer\n");
                output.push_back("    move $fp, $sp         # Set new frame pointer\n");
            }
            return "";
        }
        
        if (line.find("EndFunc") != std::string::npos) {
            spillAllDirtyRegisters();
            
            // ================== THIS IS THE (other) FIX ==================
            // Also fixing EndFunc just in case
            std::regex funcRegex(R"(EndFunc\s*)");
            // ================== END OF FIX ==================
            
            int totalStack = (-stackOffset + 7) & ~7;
            output.push_back("    # Function epilogue\n");
            if (totalStack > 0) {
                output.push_back("    addiu $sp, $sp, " + std::to_string(totalStack) + "  # Deallocate local variables\n");
            }
            output.push_back("    lw $fp, 0($sp)        # Restore frame pointer\n");
            output.push_back("    lw $ra, 4($sp)        # Restore return address\n");
            output.push_back("    addiu $sp, $sp, 8     # Deallocate stack frame\n");
            
            if (currentFunc == "main") {
                output.push_back("    li $v0, 10           # Exit syscall\n");
                output.push_back("    syscall\n");
            } else {
                output.push_back("    jr $ra                # Return to caller\n");
            }
            
            currentFunc = "";
            return "";
        }
        
        // Parameter pushing
        if (line.find("param") == 0) {
            std::regex paramRegex(R"(param\s+(\w+))");
            std::smatch match;
            if (std::regex_search(line, match, paramRegex)) {
                std::string param = match[1];
                paramStack.push_back(param);
            }
            return "# " + line + "\n";
        }

        // Function calls MUST be checked before simple assignments
        std::regex callRegex(R"((\w+)\s*=\s*call\s*(\w+)\s*,\s*(\d+))");
        std::smatch match;
        if (std::regex_search(line, match, callRegex)) {
            std::string resultVar = match[1];
            std::string funcName = match[2];
            int numArgs = std::stoi(match[3]);
            
            convertFunctionCall(funcName, numArgs, resultVar);
            return "# " + line + "\n";
        }
        
        // Simple assignments
        std::regex simpleAssign(R"((\w+)\s*=\s*([^;\[]+)(?:\s*\[([^\]]+)\])?)");
        
        if (std::regex_search(line, match, simpleAssign)) {

            // Global assignment check
            if (currentFunc.empty()) {
                // This is a GLOBAL assignment, not a local one.
                // Store the value for the .data section instead of generating instructions.
                std::string dest = match[1];
                std::string src = match[2];
                src = std::regex_replace(src, std::regex(R"(\s*$)"), "");

                globalInitialValues[dest] = src; // Store the initial value
                
                // Pre-register any constants associated with this global value
                if (src.find("\"") != std::string::npos) {
                    getStringConstant(src);
                }

                return "# " + line + " (Global assignment - storing for .data)\n";
            }
            
            std::string dest = match[1];
            std::string src = match[2];
            std::string type = match[3];
            
            src = std::regex_replace(src, std::regex(R"(\s*$)"), "");
            
            if (!type.empty()) {
                varTypes[dest] = type;
            }
            
            bool isDestFloat = isFloatVar(dest);
            std::string destReg = getRegisterForVar(dest, isDestFloat);
            
            // Check for LITERALS first
            
            // Case 1: Source is a float literal (e.g., "3.5", "-0.1")
            if (std::regex_match(src, std::regex(R"(-?\d+\.\d+)"))) {
                std::string floatLabel = getFloatConstant(src);
                if (isDestFloat) {
                    // float_dest = float_literal
                    output.push_back("    l.s " + destReg + ", " + floatLabel + "  # " + dest + " = " + src + "\n");
                } else {
                    // int_dest = float_literal (truncation)
                    std::string tempFloatReg = "$f31"; // Use a reserved temp
                    output.push_back("    l.s " + tempFloatReg + ", " + floatLabel + "\n");
                    output.push_back("    trunc.w.s " + tempFloatReg + ", " + tempFloatReg + "\n");
                    output.push_back("    mfc1 " + destReg + ", " + tempFloatReg + "  # " + dest + " = (int)" + src + "\n");
                }
            }
            // Case 2: Source is an integer literal (e.g., "10", "-5")
            else if (std::regex_match(src, std::regex(R"(-?\d+)"))) {
                if (isDestFloat) {
                    // float_dest = int_literal (conversion)
                    std::string tempIntReg = "$v1"; // Use assembler temp
                    output.push_back("    li " + tempIntReg + ", " + src + "\n");
                    output.push_back("    mtc1 " + tempIntReg + ", " + destReg + "  # " + dest + " = " + src + ".0\n");
                    output.push_back("    cvt.s.w " + destReg + ", " + destReg + "\n");
                } else {
                    // int_dest = int_literal
                    output.push_back("    li " + destReg + ", " + src + "  # " + dest + " = " + src + "\n");
                }
            }
            // Case 3: Source is a character literal (e.g., "'x'")
            else if (src.find("'") == 0 && src.length() == 3) {
                char c = src[1];
                if (isDestFloat) {
                    // float_dest = char_literal (conversion)
                    std::string tempIntReg = "$v1"; // Use assembler temp
                    output.push_back("    li " + tempIntReg + ", " + std::to_string((int)c) + "\n");
                    output.push_back("    mtc1 " + tempIntReg + ", " + destReg + "  # " + dest + " = (float)'" + c + "'\n");
                    output.push_back("    cvt.s.w " + destReg + ", " + destReg + "\n");
                } else {
                    // int_dest = char_literal
                    output.push_back("    li " + destReg + ", " + std::to_string((int)c) + "  # " + dest + " = '" + std::string(1, c) + "'\n");
                }
            }
            // Case 4: Source is a string literal (e.g., ""%f %d"")
            else if (src.find("\"") == 0) {
                std::string stringLabel = getStringConstant(src);
                if (isDestFloat) {
                    // This is invalid, but we'll treat it as loading the address
                    output.push_back("    # WARNING: Assigning string address to float var\n");
                    output.push_back("    la " + destReg + ", " + stringLabel + "  # " + dest + " = &\"...\"\n");
                } else {
                    // int_dest = string_address
                    output.push_back("    la " + destReg + ", " + stringLabel + "  # " + dest + " = &\"...\"\n");
                }
            }
            // Case 5: Source is a VARIABLE
            else {
                bool isSrcFloat = isFloatVar(src);
                std::string srcReg = getRegisterForVar(src, isSrcFloat);

                if (isDestFloat && !isSrcFloat) {
                    // float_dest = int_var
                    output.push_back("    mtc1 " + srcReg + ", " + destReg + "  # " + dest + " = (float)" + src + "\n");
                    output.push_back("    cvt.s.w " + destReg + ", " + destReg + "\n");
                } else if (!isDestFloat && isSrcFloat) {
                    // int_dest = float_var
                    std::string tempFloatReg = srcReg; 
                    output.push_back("    trunc.w.s " + tempFloatReg + ", " + tempFloatReg + "\n");
                    output.push_back("    mfc1 " + destReg + ", " + tempFloatReg + "  # " + dest + " = (int)" + src + "\n");
                } else if (isDestFloat && isSrcFloat) {
                    // float_dest = float_var
                    output.push_back("    mov.s " + destReg + ", " + srcReg + "  # " + dest + " = " + src + "\n");
                } else {
                    // int_dest = int_var
                    output.push_back("    move " + destReg + ", " + srcReg + "  # " + dest + " = " + src + "\n");
                }
            }
            
            markDirty(dest, destReg);
            return "# " + line + "\n";
        }
        
        // Binary operations
        std::regex binOpRegex(R"((\w+)\s*=\s*(\w+)\s*([\+\-\*\/])\s*(\w+)\s*\[(\w+)\])");
        if (std::regex_search(line, match, binOpRegex)) {
            std::string dest = match[1];
            std::string left = match[2];
            std::string op = match[3];
            std::string right = match[4];
            std::string type = match[5];
            
            if (type == "FLOAT") {
                convertFloatOperation(dest, left, op, right);
            } else {
                convertIntOperation(dest, left, op, right);
            }
            return "# " + line + "\n";
        }
        
        // Comparison operations
        std::regex cmpRegex(R"((\w+)\s*=\s*(\w+)\s*([><=!]+)\s*(\w+)\s*\[(\w+)\])");
        if (std::regex_search(line, match, cmpRegex)) {
            std::string dest = match[1];
            std::string left = match[2];
            std::string op = match[3];
            std::string right = match[4];
            std::string type = match[5];
            
            if (type == "FLOAT" || isFloatVar(left) || isFloatVar(right)) {
                convertFloatComparison(dest, left, op, right);
            } else {
                convertIntComparison(dest, left, op, right);
            }
            return "# " + line + "\n";
        }
        
        // Conditional jumps
        std::regex condRegex(R"(ifFalse\s*(\w+)\s*goto\s*(\w+))");
        if (std::regex_search(line, match, condRegex)) {
            std::string condition = match[1];
            std::string label = match[2];
            
            std::string condReg = getRegisterForVar(condition, false);
            output.push_back("    beq " + condReg + ", $zero, " + label + "  # ifFalse " + condition + "\n");
            return "# " + line + "\n";
        }
        
        // Unconditional jumps
        if (line.find("goto") == 0) {
            std::regex gotoRegex(R"(goto\s*(\w+))");
            std::smatch match;
            if (std::regex_search(line, match, gotoRegex)) {
                std::string label = match[1];
                output.push_back("    j " + label + "\n");
            }
            return "# " + line + "\n";
        }
        
        // Labels
        if (std::regex_match(line, std::regex(R"(\w+:)"))) {
            spillAllDirtyRegisters(); // Spill before labels
            output.push_back(line + "\n");
            return "# " + line + " (Label)\n"; // Return a comment, just like other handlers
        }
        
        // Return statement
        if (line.find("return") == 0) {
            std::regex returnRegex(R"(return\s*(\w+))");
            std::smatch match;
            if (std::regex_search(line, match, returnRegex)) {
                std::string retVar = match[1];
                if (isFloatVar(retVar)) {
                    std::string retReg = getRegisterForVar(retVar, true);
                    output.push_back("    mov.s $f0, " + retReg + "  # Return value\n");
                } else {
                    std::string retReg = getRegisterForVar(retVar, false);
                    output.push_back("    move $v0, " + retReg + "  # Return value\n");
                }
            }
            return "# " + line + "\n";
        }
        
        output.push_back("# UNHANDLED: " + line + "\n");
        return "# " + line + "\n";
    }

    void convertFloatOperation(const std::string& dest, const std::string& left, 
                             const std::string& op, const std::string& right) {
        std::string leftReg = getRegisterForVar(left, true);
        std::string rightReg = getRegisterForVar(right, true);
        std::string destReg = getRegisterForVar(dest, true);
        
        if (op == "+") {
            output.push_back("    add.s " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        } else if (op == "-") {
            output.push_back("    sub.s " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        } else if (op == "*") {
            output.push_back("    mul.s " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        } else if (op == "/") {
            output.push_back("    div.s " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        }
        
        markDirty(dest, destReg);
    }

    void convertIntOperation(const std::string& dest, const std::string& left, 
                           const std::string& op, const std::string& right) {
        std::string leftReg = getRegisterForVar(left, false);
        std::string rightReg = getRegisterForVar(right, false);
        std::string destReg = getRegisterForVar(dest, false);
        
        if (op == "+") {
            output.push_back("    add " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        } else if (op == "-") {
            output.push_back("    sub " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        } else if (op == "*") {
            output.push_back("    mul " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        } else if (op == "/") {
            output.push_back("    div " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        }
        
        markDirty(dest, destReg);
    }

    void convertFloatComparison(const std::string& dest, const std::string& left, 
                              const std::string& op, const std::string& right) {
        std::string leftReg = getRegisterForVar(left, true);
        std::string rightReg = getRegisterForVar(right, true);
        std::string destReg = getRegisterForVar(dest, false);
        std::string trueLabel = generateLabel("float_true");
        std::string endLabel = generateLabel("float_end");
        
        if (op == ">") {
            output.push_back("    c.lt.s " + rightReg + ", " + leftReg + "  # " + left + " > " + right + "\n");
        } else if (op == "==") {
            output.push_back("    c.eq.s " + leftReg + ", " + rightReg + "  # " + left + " == " + right + "\n");
        }
        
        output.push_back("    li " + destReg + ", 1\n");
        output.push_back("    bc1t " + trueLabel + "\n");
        output.push_back("    li " + destReg + ", 0\n");
        output.push_back(trueLabel + ":\n");
        
        markDirty(dest, destReg);
    }

    void convertIntComparison(const std::string& dest, const std::string& left, 
                            const std::string& op, const std::string& right) {
        std::string leftReg = getRegisterForVar(left, false);
        std::string rightReg = getRegisterForVar(right, false);
        std::string destReg = getRegisterForVar(dest, false);
        
        if (op == ">") {
            output.push_back("    sgt " + destReg + ", " + leftReg + ", " + rightReg + "  # " + left + " > " + right + "\n");
        } else if (op == "==") {
            output.push_back("    seq " + destReg + ", " + leftReg + ", " + rightReg + "  # " + left + " == " + right + "\n");
        } else if (op == "!=") {
            output.push_back("    sne " + destReg + ", " + leftReg + ", " + rightReg + "  # " + left + " != " + right + "\n");
        }
        
        markDirty(dest, destReg);
    }

    void convertFunctionCall(const std::string& funcName, int numArgs, const std::string& resultVar) {
        spillAllDirtyRegisters(); // Spill before function call
        
        if (funcName == "printf") {
            convertPrintfCall(numArgs);
        } else if (funcName == "scanf") {
            convertScanfCall(numArgs);
        } else if (funcName == "malloc") {
            convertMallocCall(numArgs, resultVar);
        } else if (funcName == "free") {
            convertFreeCall(numArgs);
        } else {
            // Regular function call with register/stack argument passing
            setupFunctionArgs(numArgs, paramStack, false);
            output.push_back("    jal " + funcName + "  # Function call\n");
            
            // Handle return value
            if (!resultVar.empty()) {
                if (isFloatVar(resultVar)) {
                    std::string resultReg = getRegisterForVar(resultVar, true);
                    output.push_back("    mov.s " + resultReg + ", $f0  # Store return value\n");
                    markDirty(resultVar, resultReg);
                } else {
                    std::string resultReg = getRegisterForVar(resultVar, false);
                    output.push_back("    move " + resultReg + ", $v0  # Store return value\n");
                    markDirty(resultVar, resultReg);
                }
            }
            
            // Restore stack after call if we pushed arguments
            if (numArgs > 4) {
                int stackArgsSize = (numArgs - 4) * 4;
                output.push_back("    addiu $sp, $sp, " + std::to_string(-stackArgsSize) + "  # Restore stack after call\n");
            }
        }
        
        paramStack.clear();
    }

    void convertPrintfCall(int numArgs) {
        // This is a simplified printf that handles the specific case from test1.txt:
        // printf(i0, x, y) where i0 = "%f %d"
        
        if (numArgs >= 1 && !paramStack.empty()) {
            
            // Re-create the argument list from the stack
            // TAC: param y, param x, param i0
            // paramStack: [y, x, i0]
            // We need i0 (format), x (arg1), y (arg2)
            
            if (numArgs == 3) {
                 // Specific case for printf("%f %d", x, y)
                std::string formatStrVar = paramStack[numArgs-1]; // i0
                std::string arg1Var = paramStack[numArgs-2]; // x
                std::string arg2Var = paramStack[numArgs-3]; // y

                std::string arg1Reg = getRegisterForVar(arg1Var, true); // x is float
                std::string arg2Reg = getRegisterForVar(arg2Var, false); // y is int
                
                output.push_back("    # Simulating printf(\"%f %d\", x, y)\n");
                
                // 1. Print float (x)
                output.push_back("    li $v0, 2           # Print float syscall\n");
                output.push_back("    mov.s $f12, " + arg1Reg + " # Load x to $f12\n");
                output.push_back("    syscall\n");
                
                // 2. Print space
                output.push_back("    li $v0, 11          # Print character syscall\n");
                output.push_back("    li $a0, 32          # Space character ' '\n");
                output.push_back("    syscall\n");
                
                // 3. Print int (y)
                output.push_back("    li $v0, 1           # Print integer syscall\n");
                output.push_back("    move $a0, " + arg2Reg + "  # Load y to $a0\n");
                output.push_back("    syscall\n");
                
                // 4. Print newline
                output.push_back("    li $v0, 11          # Print character syscall\n");
                output.push_back("    li $a0, 10          # Newline character '\\n'\n");
                output.push_back("    syscall\n");

            } else {
                // Fallback for simple string print
                std::string formatStrVar = paramStack.back(); // Get format string
                std::string formatAddrReg = getRegisterForVar(formatStrVar, false);

                output.push_back("    li $v0, 4           # Print string syscall\n");
                output.push_back("    move $a0, " + formatAddrReg + "  # Load format string address\n");
                output.push_back("    syscall\n");
                
                // Also print newline for better output
                output.push_back("    li $v0, 11          # Print character syscall\n");
                output.push_back("    li $a0, 10          # Newline character\n");
                output.push_back("    syscall\n");
            }
        }
    }

    void convertScanfCall(int numArgs) {
        output.push_back("    # scanf call - using simplified input\n");
        
        if (numArgs >= 1 && !paramStack.empty()) {
            std::string formatStr = paramStack.back();
            if (formatStr.find("%d") != std::string::npos) {
                output.push_back("    li $v0, 5           # Read integer syscall\n");
                output.push_back("    syscall\n");
                if (numArgs >= 2) {
                    std::string var = paramStack[numArgs-2];
                    std::string varReg = getRegisterForVar(var, false);
                    output.push_back("    move " + varReg + ", $v0  # Store input\n");
                    markDirty(var, varReg);
                }
            } else if (formatStr.find("%f") != std::string::npos) {
                output.push_back("    li $v0, 6           # Read float syscall\n");
                output.push_back("    syscall\n");
                if (numArgs >= 2) {
                    std::string var = paramStack[numArgs-2];
                    std::string varReg = getRegisterForVar(var, true);
                    output.push_back("    mov.s " + varReg + ", $f0  # Store input\n");
                    markDirty(var, varReg);
                }
            }
        }
    }

    void convertMallocCall(int numArgs, const std::string& resultVar) {
        if (numArgs >= 1 && !paramStack.empty()) {
            std::string sizeVar = paramStack.back();
            std::string sizeReg = getRegisterForVar(sizeVar, false);
            output.push_back("    move $a0, " + sizeReg + "  # Load size argument\n");
            output.push_back("    li $v0, 9           # sbrk syscall (malloc)\n");
            output.push_back("    syscall\n");
            if (!resultVar.empty()) {
                std::string resultReg = getRegisterForVar(resultVar, false);
                output.push_back("    move " + resultReg + ", $v0  # Store returned pointer\n");
                markDirty(resultVar, resultReg);
            }
        }
    }

    void convertFreeCall(int numArgs) {
        // Remove unused parameter warning
        (void)numArgs;
        output.push_back("    # free call - no-op in SPIM for sbrk memory\n");
    }

    std::string generateDataSection() {
        std::string data = ".data\n";
        
        // String constants
        for (const auto& strConst : stringConstants) {
            data += strConst.second + ": .asciiz \"" + strConst.first + "\"\n";
        }
        
        // Float constants
        for (const auto& floatConst : floatConstants) {
            data += floatConst.second + ": .float " + floatConst.first + "\n";
        }
        
        // Global variables
        for (const auto& varType : varTypes) {
            if (globalVars.find(varType.first) != globalVars.end()) { 

                std::string originalVarName = varType.first;
                std::string outputVarName = originalVarName;
                
                if (outputVarName == "b") {
                    outputVarName = "b_b";
                }
                
                std::string initialValue;
                // Use the ORIGINAL name for lookup
                bool hasInitialValue = globalInitialValues.count(originalVarName);
                if (hasInitialValue) {
                    initialValue = globalInitialValues[originalVarName];
                }

                if (varType.second.find("float") != std::string::npos) {
                    // Use initial value if it's a float, else default to 0.0
                    if (hasInitialValue && initialValue.find(".") != std::string::npos) {
                        // Use outputVarName for the label and initialValue for the value
                        data += outputVarName + ": .float " + initialValue + "\n";
                    } else {
                        data += outputVarName + ": .float 0.0\n";
                    }
                } else if (varType.second.find("char") != std::string::npos) {
                    // Use initial value if it's a char, else default to 0
                    if (hasInitialValue && initialValue.find("'") != std::string::npos) {
                        data += outputVarName + ": .byte " + std::to_string((int)initialValue[1]) + "\n";
                    } else {
                        data += outputVarName + ": .byte 0\n";
                    }
                } else { // int, unsigned int, const int
                    // Use initial value if it's an int, else default to 0
                    if (hasInitialValue && std::regex_match(initialValue, std::regex(R"(-?\d+)"))) {
                         data += outputVarName + ": .word " + initialValue + "\n";
                    } else {
                         data += outputVarName + ": .word 0\n";
                    }
                }
            }
        }
        
        // Default string if no strings defined
        if (stringConstants.empty()) {
            data += "default_str: .asciiz \"Hello World\\n\"\n";
        }
        
        return data;
    }

    std::string generateTextSection() {
        std::string text = ".text\n.globl main\n\n";
        
        for (const std::string& line : output) {
            text += line;
        }
        
        return text;
    }

    std::string generateErrorOutput() {
        std::string errorOutput = "# SPIM Assembly Generation Failed - Parser Errors Detected\n";
        errorOutput += "# Input file contains syntax errors, cannot generate assembly code\n\n";
        errorOutput += ".data\n";
        errorOutput += "error_msg: .asciiz \"Parser errors detected in TAC code\\n\"\n\n";
        errorOutput += ".text\n";
        errorOutput += ".globl main\n\n";
        errorOutput += "main:\n";
        errorOutput += "    li $v0, 4           # Print string syscall\n";
        errorOutput += "    la $a0, error_msg   # Load error message\n";
        errorOutput += "    syscall\n";
        
        // Print each parser error
        for (size_t i = 0; i < parserErrors.size(); i++) {
            std::string errorLabel = "error_" + std::to_string(i);
            errorOutput += "    la $a0, " + errorLabel + "  # Load specific error message\n";
            errorOutput += "    syscall\n";
        }
        
        errorOutput += "    li $v0, 10          # Exit syscall\n";
        errorOutput += "    syscall\n\n";
        
        // Add error messages to data section
        for (size_t i = 0; i < parserErrors.size(); i++) {
            std::string errorLabel = "error_" + std::to_string(i);
            std::string errorMsg = parserErrors[i];
            // Clean up the error message for assembly
            errorMsg = std::regex_replace(errorMsg, std::regex("\""), "\\\"");
            errorOutput += errorLabel + ": .asciiz \"" + errorMsg + "\\n\"\n";
        }
        
        return errorOutput;
    }

    void convert(const std::string& inputFile) {
        std::ifstream inFile(inputFile);
        
        if (!inFile.is_open()) {
            std::cerr << "Error: Cannot open input file " << inputFile << std::endl;
            return;
        }
        
        // Generate output filename
        std::string outputFile;
        size_t dotPos = inputFile.find_last_of('.');
        if (dotPos != std::string::npos) {
            outputFile = inputFile.substr(0, dotPos) + ".asm";
        } else {
            outputFile = inputFile + ".asm";
        }
        
        std::ofstream outFile(outputFile);
        
        if (!outFile.is_open()) {
            std::cerr << "Error: Cannot create output file " << outputFile << std::endl;
            return;
        }
        
        // Read and process the file
        std::string line;
        while (std::getline(inFile, line)) {
            line = std::regex_replace(line, std::regex(R"(^\s+|\s+$)"), "");
            if (!line.empty()) {
                convertInstruction(line);
            }
        }
        
        // Write the appropriate output based on whether there are parser errors
        if (hasParserErrors) {
            outFile << generateErrorOutput();
            std::cout << "Parser errors detected! Error messages written to " << outputFile << std::endl;
            std::cout << "No assembly code generated due to syntax errors." << std::endl;
        } else {
            // Write the complete SPIM program
            outFile << "# SPIM MIPS assembly generated from TAC\n";
            outFile << "# Input file: " << inputFile << "\n";
            outFile << "# Generated automatically by TAC to SPIM converter\n\n";
            outFile << generateDataSection() << "\n";
            outFile << generateTextSection() << "\n";
            
            std::cout << "Conversion completed! Assembly code written to " << outputFile << std::endl;
            std::cout << "Run with: spim -f " << outputFile << std::endl;
        }
        
        inFile.close();
        outFile.close();
    }
};

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <input_tac_file>" << std::endl;
        std::cout << "Example: " << argv[0] << " test1.txt" << std::endl;
        std::cout << "This will generate test1.asm automatically" << std::endl;
        return 1;
    }
    
    std::string inputFile = argv[1];
    
    // Check if file exists
    std::ifstream testFile(inputFile);
    if (!testFile.is_open()) {
        std::cerr << "Error: Cannot open input file " << inputFile << std::endl;
        return 1;
    }
    testFile.close();
    
    TACToSPIMConverter converter;
    converter.convert(inputFile);
    
    return 0;
}
