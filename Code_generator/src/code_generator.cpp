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
    int intParamIndex;
    int floatParamIndex;
    int stackAllocPlaceholderIndex;
    bool hasParserErrors;
    std::vector<std::string> parserErrors;
    std::map<std::string, std::string> globalInitialValues;

    // Set of reserved SPIM/MIPS keywords
    std::set<std::string> spimKeywords;

    // Register and Address Descriptors
    struct RegisterInfo {
       std::set<std::string> varNames;      // Variable currently in register (empty if free)
        bool isFloat;             // Whether register holds float value
        int lastUsed;             // For LRU eviction
    };
    
    struct VariableInfo {
        std::set<std::string> registers; // Registers where variable is cached
        int stackOffset;          // Stack offset (-1 if not on stack)
        bool dirty;               // Whether memory copy is stale
        bool isFloat;             // Whether variable is float type
        bool isSpilled;           // Whether variable is spilled to stack
        std::string pointsTo;
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

    std::string sanitizeName(const std::string& name) {
        if (spimKeywords.count(name)) {
            return name + "_1"; // Append _1
        }
        return name;
    }

public:
    TACToSPIMConverter() : labelCount(0), stringConstCount(0), floatConstCount(0), 
                          tempVarCount(0), stackOffset(0), argStackOffset(0), useCounter(0),
                          intParamIndex(0), floatParamIndex(0),
                          stackAllocPlaceholderIndex(-1),
                          hasParserErrors(false) {
        initializeRegisters();
        
        // Initialize the set of SPIM keywords to avoid conflicts
        spimKeywords = {
            "add", "addi", "addu", "addiu", "sub", "subu", "mul", "mult", "div",
            "and", "andi", "or", "ori", "xor", "xori", "nor",
            "sll", "srl", "sra", "sllv", "srlv", "srav",
            "lw", "sw", "lh", "sh", "lb", "sb", "lui",
            "li", "la", "move",
            "mfhi", "mflo", "mthi", "mtlo",
            "j", "jal", "jr", "jalr",
            "beq", "bne", "blez", "bgtz", "bltz", "bgez",
            "slt", "slti", "sltu", "sltiu",
            "syscall",
            "l.s", "s.s", "mov.s", "add.s", "sub.s", "mul.s", "div.s",
            "c.eq.s", "c.le.s", "c.lt.s",
            "bc1t", "bc1f",
            "mfc1", "mtc1", "cvt.s.w", "trunc.w.s",
            "b", "abs" // Common problematic names
        };
    }

    void initializeRegisters() {
        for (const auto& reg : availableIntRegs) {
            intRegisters[reg].varNames.clear();
            intRegisters[reg].isFloat = false;
            intRegisters[reg].lastUsed = 0;
        }
        for (const auto& reg : argIntRegs) {
            intRegisters[reg].varNames.clear();
            intRegisters[reg].isFloat = false;
            intRegisters[reg].lastUsed = 0;
        }
        for (const auto& reg : availableFloatRegs) {
            floatRegisters[reg].varNames.clear();
            floatRegisters[reg].isFloat = true;
            floatRegisters[reg].lastUsed = 0;
        }
        for (const auto& reg : argFloatRegs) {
            floatRegisters[reg].varNames.clear();
            floatRegisters[reg].isFloat = true;
            floatRegisters[reg].lastUsed = 0;
        }
        intRegisters["$v0"].varNames.clear(); intRegisters["$v0"].isFloat = false; intRegisters["$v0"].lastUsed = 0;
        intRegisters["$v1"].varNames.clear(); intRegisters["$v1"].isFloat = false; intRegisters["$v1"].lastUsed = 0;
        floatRegisters["$f0"].varNames.clear(); floatRegisters["$f0"].isFloat = true; floatRegisters["$f0"].lastUsed = 0;
    }

    void resetRegisters() {
        for (auto& reg : intRegisters) {
            reg.second.varNames.clear();
            reg.second.isFloat = false;
            reg.second.lastUsed = 0;
        }
        for (auto& reg : floatRegisters) {
            reg.second.varNames.clear();
            reg.second.isFloat = true;
            reg.second.lastUsed = 0;
        }
        intRegisters["$v0"].varNames.clear(); intRegisters["$v0"].isFloat = false; intRegisters["$v0"].lastUsed = 0;
        intRegisters["$v1"].varNames.clear(); intRegisters["$v1"].isFloat = false; intRegisters["$v1"].lastUsed = 0;
        floatRegisters["$f0"].varNames.clear(); floatRegisters["$f0"].isFloat = true; floatRegisters["$f0"].lastUsed = 0;
    }
    
    void invalidateSpecificRegisters(const std::set<std::string>& regsToClear) {
        for (const auto& reg : regsToClear) {
            if (intRegisters.count(reg)) {
                std::set<std::string> vars = intRegisters[reg].varNames;
                if (!vars.empty()) {
                    output.push_back("    # Clearing " + reg + " (was holding: " + *vars.begin() + ")\n");
                    for(const auto& var : vars) {
                        if (variableInfo.count(var)) {
                            variableInfo[var].registers.erase(reg);
                        }
                    }
                    intRegisters[reg].varNames.clear();
                }
            } else if (floatRegisters.count(reg)) {
                std::set<std::string> vars = floatRegisters[reg].varNames;
                if (!vars.empty()) {
                    output.push_back("    # Clearing " + reg + " (was holding: " + *vars.begin() + ")\n");
                    for(const auto& var : vars) {
                        if (variableInfo.count(var)) {
                            variableInfo[var].registers.erase(reg);
                        }
                    }
                    floatRegisters[reg].varNames.clear();
                }
            }
        }
    }
    
    std::string generateLabel(const std::string& base) {
        // Sanitization is not needed here as we control the base
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
        if (!cleanValue.empty() && cleanValue.front() == '"' && cleanValue.back() == '"') {
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

    std::string getRegisterForVar(const std::string& var, bool preferFloat = false, bool loadValue = true) {
    bool isFloat = preferFloat || isFloatVar(var);
    auto& registers = isFloat ? floatRegisters : intRegisters;
    auto& availableRegs = isFloat ? availableFloatRegs : availableIntRegs;
    
    // Check if variable is already in a register
    for (const auto& reg : availableRegs) {
        if (registers[reg].varNames.count(var)) {
            if (loadValue && variableInfo.find(var) != variableInfo.end() && 
                variableInfo[var].stackOffset != -1 && !variableInfo[var].dirty) {
                // The variable exists on stack and register copy might be stale, reload it
                output.push_back("    # Reloading " + var + " from stack to " + reg + " (stale register)\n");
                if (isFloat) {
                    output.push_back("    l.s " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
                } else {
                    output.push_back("    lw " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
                }
            }
            registers[reg].lastUsed = ++useCounter;
            return reg;
        }
    }
        
        // Find free register
        for (const auto& reg : availableRegs) {
            if (registers[reg].varNames.empty()) {
                registers[reg].varNames.insert(var); // Add var to set
                registers[reg].isFloat = isFloat;
                registers[reg].lastUsed = ++useCounter;

                if (loadValue) {
                    if (variableInfo.find(var) != variableInfo.end() && variableInfo[var].stackOffset != -1) {
                        output.push_back("    # Loading " + var + " from stack to " + reg + "\n");
                        if (isFloat) {
                            output.push_back("    l.s " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
                        } else {
                            output.push_back("    lw " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
                        }
                    }
                    else if (isGlobalVar(var)) {
                        output.push_back("    # Loading global " + var + " into " + reg + "\n");
                        if (isFloat) {
                            output.push_back("    l.s " + reg + ", " + sanitizeName(var) + "\n");
                        } else {
                            output.push_back("    lw " + reg + ", " + sanitizeName(var) + "\n");
                        }
                    }
                } else {
                     output.push_back("    # Allocating " + reg + " for " + var + "\n");
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
        
        spillRegister(lruReg);
        
        std::set<std::string> evictedVars = registers[lruReg].varNames;
        for(const auto& evictedVar : evictedVars) {
            if (variableInfo.find(evictedVar) != variableInfo.end()) {
                variableInfo[evictedVar].registers.erase(lruReg);
            }
        }
        
        // Assign register to new variable
        registers[lruReg].varNames.clear(); // Clear old vars
        registers[lruReg].varNames.insert(var); // Add new var
        registers[lruReg].isFloat = isFloat;
        registers[lruReg].lastUsed = ++useCounter;
        
        if (loadValue) {
            if (variableInfo.find(var) != variableInfo.end() && variableInfo[var].stackOffset != -1) {
                output.push_back("    # Loading " + var + " from stack to " + lruReg + "\n");
                if (isFloat) {
                    output.push_back("    l.s " + lruReg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
                } else {
                    output.push_back("    lw " + lruReg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
                }
            }
            else if (isGlobalVar(var)) {
                output.push_back("    # Loading global " + var + " into " + lruReg + "\n");
                if (isFloat) {
                    output.push_back("    l.s " + lruReg + ", " + sanitizeName(var) + "\n");
                } else {
                    output.push_back("    lw " + lruReg + ", " + sanitizeName(var) + "\n");
                }
            }
        } else {
            output.push_back("    # Allocating " + lruReg + " for " + var + " (no load)\n");
        }
        
        // Update variable info
        if (variableInfo.find(var) == variableInfo.end()) {
            variableInfo[var] = {{lruReg}, -1, false, isFloat, false};
        } else {
            variableInfo[var].registers.insert(lruReg);
        }
        
        return lruReg;
    }

    void spillVariable(const std::string& reg, const std::string& var) {
        // Don't spill temporary variables
        if (var.empty() || var[0] == 't' || var[0] == 'i' || var[0] == 'f') {
             // Check for temp float 'f' as well
            if (var[0] == 'f' && var.length() > 1 && isdigit(var[1])) {
                 return;
            } else if (var[0] != 'f') {
                return;
            }
        }
        
        if (variableInfo.find(var) == variableInfo.end()) {
            return; // No info, can't spill
        }

        bool isFloat = variableInfo[var].isFloat;
        
        if (isGlobalVar(var)) {
            // Spill to global memory
            output.push_back("    # Spilling " + var + " from " + reg + " to global memory\n");
            if (isFloat) {
                output.push_back("    s.s " + reg + ", " + sanitizeName(var) + "\n");
            } else {
                output.push_back("    sw " + reg + ", " + sanitizeName(var) + "\n");
            }
        } else {
            // Spill to stack
            // Ensure variable has stack allocation
            if (variableInfo[var].stackOffset == -1) {
                allocateStackSpace(var);
            }
            
            output.push_back("    # Spilling " + var + " from " + reg + " to stack\n");
            if (isFloat) {
                output.push_back("    s.s " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
            } else {
                output.push_back("    sw " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
            }
        }
        
        variableInfo[var].dirty = false;
    }

    void spillRegister(const std::string& reg) {
        RegisterInfo reg_info;
        bool isFloatReg = false;

        if (intRegisters.find(reg) != intRegisters.end()) {
            reg_info = intRegisters[reg];
            isFloatReg = false;
        } else if (floatRegisters.find(reg) != floatRegisters.end()) {
            reg_info = floatRegisters[reg];
            isFloatReg = true;
        } else {
            return; // Not a spillable register
        }

        if (reg_info.varNames.empty()) {
            return;
        }

        for (const auto& var : reg_info.varNames) {
            if (variableInfo.find(var) != variableInfo.end() && variableInfo[var].dirty) {
                spillVariable(reg, var);
            }
        }
    }

    void allocateStackSpace(const std::string& var) {
        if (variableInfo.find(var) == variableInfo.end()) {
            variableInfo[var] = {{}, -1, false, isFloatVar(var), false};
        }
        
        if (variableInfo[var].stackOffset == -1 && !var.empty() && var[0] != 't' && var[0] != 'i') {
            // Only allocate for non-temporary variables
            variableInfo[var].stackOffset = stackOffset;
            stackOffset -= 4; 
            output.push_back("    # Allocated stack space for " + var + " at offset " + std::to_string(variableInfo[var].stackOffset) + "\n");
        }
    }

    void markDirty(const std::string& var, const std::string& reg) {
        if (variableInfo.find(var) == variableInfo.end()) {
            // Variable doesn't exist yet, create it
            bool isFloat = floatRegisters.count(reg) > 0;
            variableInfo[var] = {{reg}, -1, true, isFloat, false};
        } else {
            variableInfo[var].dirty = true;
        }
    }

    void spillAllDirtyRegisters() {
        for (const auto& reg : availableIntRegs) {
            spillRegister(reg);
        }
        
        for (const auto& reg : availableFloatRegs) {
            spillRegister(reg);
        }
    }

    void invalidateTempRegisters() {
        // Invalidate ALL integer $t registers
        for (const auto& reg : availableIntRegs) {
            std::set<std::string> vars = intRegisters[reg].varNames;
            if (!vars.empty()) {
                for(const auto& var : vars) {
                    if (variableInfo.count(var)) {
                        variableInfo[var].registers.erase(reg);
                    }
                }
                intRegisters[reg].varNames.clear();
            }
        }
        
        // Invalidate ALL float $t registers
        for (const auto& reg : availableFloatRegs) {
            std::set<std::string> vars = floatRegisters[reg].varNames;
            if (!vars.empty()) {
                for(const auto& var : vars) {
                    if (variableInfo.count(var)) {
                        variableInfo[var].registers.erase(reg);
                    }
                }
                floatRegisters[reg].varNames.clear();
            }
        }
        
        // Also invalidate $a registers
        for (const auto& reg : argIntRegs) {
            std::set<std::string> vars = intRegisters[reg].varNames;
            if (!vars.empty()) {
                for(const auto& var : vars) {
                    if (variableInfo.count(var)) {
                        variableInfo[var].registers.erase(reg);
                    }
                }
                intRegisters[reg].varNames.clear();
            }
        }
        
        for (const auto& reg : argFloatRegs) {
            std::set<std::string> vars = floatRegisters[reg].varNames;
            if (!vars.empty()) {
                for(const auto& var : vars) {
                    if (variableInfo.count(var)) {
                        variableInfo[var].registers.erase(reg);
                    }
                }
                floatRegisters[reg].varNames.clear();
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
            std::string argReg = getRegisterForVar(arg, isFloat, true); 
            
            if (i < 4) {
                // First 4 arguments go in registers
                if (isFloat && floatArgCount < 2) {
                    std::string reg = argFloatRegs[floatArgCount++];
                    output.push_back("    mov.s " + reg + ", " + argReg + "  # Float argument " + std::to_string(i+1) + "\n");
                } else if (!isFloat && intArgCount < 4) {
                    std::string reg = argIntRegs[intArgCount++];
                    output.push_back("    move " + reg + ", " + argReg + "  # Integer argument " + std::to_string(i+1) + "\n");
                } else {
                    // Fall back to stack for mixed types beyond available registers
                    int stackOffset = -(i - 3) * 4; 
                    output.push_back("    # Argument " + std::to_string(i+1) + " on stack at offset " + std::to_string(stackOffset) + "\n");
                    if (isFloat) {
                        output.push_back("    s.s " + argReg + ", " + std::to_string(stackOffset) + "($sp)\n");
                    } else {
                        output.push_back("    sw " + argReg + ", " + std::to_string(stackOffset) + "($sp)\n");
                    }
                }
            } else {
                // Arguments 5+ go on stack
                int stackOffset = -(i - 3) * 4; 
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
            
            // Only add to globalVars if we are not inside a function
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
        
        if (hasParserError(line)) {
            hasParserErrors = true;
            parserErrors.push_back(line);
            return "";
        }
        
        if (line.find("//") == 0) {
            if (line.find("Variable declaration:") != std::string::npos) {
                parseVariableDeclaration(line);
            }
            else if (line.find("Parameter:") != std::string::npos && !currentFunc.empty()) {
                std::regex paramComment(R"(//\s*Parameter:\s*(\w+))");
                std::smatch paramMatch;
                if (std::regex_search(line, paramMatch, paramComment)) {
                    std::string paramName = paramMatch[1];
                    bool isParamFloat = isFloatVar(paramName);
                    
                    allocateStackSpace(paramName);
                    
                    std::string destReg = getRegisterForVar(paramName, isParamFloat, false);
                    
                    if (isParamFloat) {
                        if (floatParamIndex < argFloatRegs.size()) {
                            output.push_back("    mov.s " + destReg + ", " + argFloatRegs[floatParamIndex] + "  # Copy param " + paramName + "\n");
                            output.push_back("    s.s " + destReg + ", " + std::to_string(variableInfo[paramName].stackOffset) + "($fp)  # Store param " + paramName + " to stack\n");
                            floatParamIndex++;
                        } else {
                            int stackOffset = 8 + (floatParamIndex - argFloatRegs.size()) * 4;
                            output.push_back("    l.s " + destReg + ", " + std::to_string(stackOffset) + "($fp)  # Load stack param " + paramName + "\n");
                            output.push_back("    s.s " + destReg + ", " + std::to_string(variableInfo[paramName].stackOffset) + "($fp)  # Store param " + paramName + " to local stack\n");
                            floatParamIndex++;
                        }
                    } else {
                        if (intParamIndex < argIntRegs.size()) {
                            output.push_back("    move " + destReg + ", " + argIntRegs[intParamIndex] + "  # Copy param " + paramName + "\n");
                            output.push_back("    sw " + destReg + ", " + std::to_string(variableInfo[paramName].stackOffset) + "($fp)  # Store param " + paramName + " to stack\n");
                            intParamIndex++;
                        } else {
                            int stackOffset = 8 + (intParamIndex - argIntRegs.size()) * 4;
                            output.push_back("    lw " + destReg + ", " + std::to_string(stackOffset) + "($fp)  # Load stack param " + paramName + "\n");
                            output.push_back("    sw " + destReg + ", " + std::to_string(variableInfo[paramName].stackOffset) + "($fp)  # Store param " + paramName + " to local stack\n");
                            intParamIndex++;
                        }
                    }
                    markDirty(paramName, destReg);
                }
            }
            return "# " + line + "\n";
        }
        
        if (line.find("BeginFunc") != std::string::npos) {
            std::regex funcRegex(R"(BeginFunc\s+(\w+))");
            std::smatch match;
            if (std::regex_search(line, match, funcRegex)) {
                currentFunc = match[1];
                variableInfo.clear();
                stackOffset = -8; 
                argStackOffset = 0;
                intParamIndex = 0;
                floatParamIndex = 0;
                paramStack.clear();
                resetRegisters();
                
                output.push_back("\n" + sanitizeName(currentFunc) + ":\n");
                
                output.push_back("    addiu $sp, $sp, -8    # Allocate space for ra/fp\n");
                output.push_back("    sw $ra, 4($sp)        # Save return address\n");
                output.push_back("    sw $fp, 0($sp)        # Save frame pointer\n");
                output.push_back("    move $fp, $sp         # Set new frame pointer\n");
                
                stackAllocPlaceholderIndex = output.size();
                output.push_back("    # STACK_ALLOC_PLACEHOLDER\n");
            }
            return "";
        }
        
        if (line.find("EndFunc") != std::string::npos) {
            spillAllDirtyRegisters();
            
            output.push_back(sanitizeName(currentFunc) + "_return:\n");
            
            // Ensure we allocate enough space for all local variables
int localStackSize = ((-stackOffset) > 0) ? ((-stackOffset + 7) & ~7) : 16;
// Force minimum allocation
if (currentFunc == "main" && localStackSize < 16) {
    localStackSize = 16;
}
            
            if (stackAllocPlaceholderIndex != -1 && stackAllocPlaceholderIndex < output.size()) {
                if (localStackSize > 0) {
                    output[stackAllocPlaceholderIndex] = "    addiu $sp, $sp, -" + std::to_string(localStackSize) + "    # Allocate stack for locals\n";
                } else {
                    output[stackAllocPlaceholderIndex] = "    # No local stack needed\n";
                }
            }
            stackAllocPlaceholderIndex = -1; 
            
            output.push_back("    # Function epilogue\n");
            output.push_back("    move $sp, $fp         # Reset $sp to $fp (deallocates locals)\n");
            output.push_back("    lw $fp, 0($sp)        # Restore frame pointer\n");
            output.push_back("    lw $ra, 4($sp)        # Restore return address\n");
            output.push_back("    addiu $sp, $sp, 8     # Deallocate ra/fp space\n");

            if (currentFunc == "main") {
                output.push_back("    li $v0, 10           # Exit syscall\n");
                output.push_back("    syscall\n");
            } else {
                output.push_back("    jr $ra                # Return to caller\n");
            }
            
            currentFunc = "";
            return "";
        }
        
        if (line.find("param") == 0) {
            std::regex paramRegex(R"(param\s+(\w+))");
            std::smatch match;
            if (std::regex_search(line, match, paramRegex)) {
                std::string param = match[1];
                paramStack.push_back(param);
            }
            return "# " + line + "\n";
        }

        std::regex callRegex(R"((\w+)\s*=\s*call\s*(\w+)\s*,\s*(\d+))");
        std::smatch match;
        if (std::regex_search(line, match, callRegex)) {
            std::string resultVar = match[1];
            std::string funcName = match[2];
            int numArgs = std::stoi(match[3]);
            
            convertFunctionCall(funcName, numArgs, resultVar);
            return "# " + line + "\n";
        }

        std::regex binOpRegex(R"((\w+)\s*=\s*(\w+)\s*(<<|>>|[\+\-\*\/&\|\^])\s*([\w\.-]+)(?:\s*\[(\w+)\])?)");
        if (std::regex_search(line, match, binOpRegex)) {
            std::string dest = match[1];
            std::string left = match[2];
            std::string op = match[3];
            std::string right = match[4];
            std::string type = match[5].str();
            
            if (type == "FLOAT") {
                convertFloatOperation(dest, left, op, right);
            } else if (type == "INT") {
                convertIntOperation(dest, left, op, right);
            } else {
                if (isFloatVar(left) || isFloatVar(right) || std::regex_match(left, std::regex(R"(-?\d+\.\d+)")) || std::regex_match(right, std::regex(R"(-?\d+\.\d+)"))) {
                    output.push_back("    # Inferred type as FLOAT from operands\n");
                    convertFloatOperation(dest, left, op, right);
                } else { 
                    output.push_back("    # Inferred type as INT from operands\n");
                    convertIntOperation(dest, left, op, right);
                }
            }
            return "# " + line + "\n";
        }
        
        std::regex cmpRegex(R"((\w+)\s*=\s*([\w\.-]+)\s*([><=!]+)\s*([\w\.-]+)\s*\[(\w+)\])");
        if (std::regex_search(line, match, cmpRegex)) {
            std::string dest = match[1];
            std::string left = match[2];
            std::string op = match[3];
            std::string right = match[4];
            std::string type = match[5];
            
            if (type == "FLOAT" || isFloatVar(left) || isFloatVar(right) || std::regex_match(left, std::regex(R"(-?\d+\.\d+)")) || std::regex_match(right, std::regex(R"(-?\d+\.\d+)"))) {
                convertFloatComparison(dest, left, op, right);
            } else {
                convertIntComparison(dest, left, op, right);
            }
            return "# " + line + "\n";
        }
        
        std::regex simpleAssign(R"((\w+)\s*=\s*([^;\[\+\-\*\/><=!]+)(?:\s*\[([^\]]+)\])?)");
        
        if (std::regex_search(line, match, simpleAssign)) {

            if (currentFunc.empty()) {
                std::string dest = match[1];
                std::string src = match[2];
                src = std::regex_replace(src, std::regex(R"(\s*$)"), "");
                globalInitialValues[dest] = src;
                if (src.find("\"") != std::string::npos) {
                    getStringConstant(src);
                } else if (std::regex_match(src, std::regex(R"(-?\d+\.\d+)"))) {
                    getFloatConstant(src);
                }
                return "# " + line + " (Global assignment - storing for .data)\n";
            }
            
            std::string dest = match[1];
            std::string src = match[2];
            std::string type = match[3];
            
            src = std::regex_replace(src, std::regex(R"(\s*$)"), "");

            // Handle &var (address-of)
            if (src.rfind("&", 0) == 0) {
                std::string varName = src.substr(1);
                std::string destReg = getRegisterForVar(dest, false, false); 

                if (varName.front() == '"') {
                    std::string label = getStringConstant(varName);
                    output.push_back("    la " + destReg + ", " + label + "  # " + line);
                    globalInitialValues[dest] = varName; 
                }
                else if (globalVars.count(varName)) {
                    output.push_back("    la " + destReg + ", " + sanitizeName(varName) + "  # " + line);
                }
                else {
                    // orce allocation
                    allocateStackSpace(varName);
                    
                    // Spill if dirty
                    if (variableInfo.find(varName) != variableInfo.end() && variableInfo[varName].dirty) {
                        for(const auto& reg : variableInfo[varName].registers) {
                            output.push_back("    # Storing " + varName + " before taking address (for scanf)\n");
                            spillVariable(reg, varName); // This also clears dirty flag
                            break; // Only need to spill once
                        }
                    }
                    // Invalidate all registers holding varName
                    if (variableInfo.find(varName) != variableInfo.end()) {
                         output.push_back("    # Invalidating registers for " + varName + "\n");
                        for(const auto& reg : variableInfo[varName].registers) {
                            (variableInfo[varName].isFloat ? floatRegisters : intRegisters)[reg].varNames.erase(varName);
                        }
                        variableInfo[varName].registers.clear();
                    }

                    int offset = variableInfo[varName].stackOffset;
                    output.push_back("    addiu " + destReg + ", $fp, " + std::to_string(offset) + " # " + line);
                }

                markDirty(dest, destReg);
                if (variableInfo.find(dest) == variableInfo.end()) {
                    // Initialize with default values, setting pointsTo
                    variableInfo[dest] = {{destReg}, -1, true, false, false, varName};
                } else {
                    variableInfo[dest].pointsTo = varName;
                }
                return "# " + line + "\n";
            }
            
            if (!type.empty()) {
                varTypes[dest] = type;
            }
            
            bool isDestFloat = isFloatVar(dest);
            
            // Check for literals first
            
            // Case 1: Source is a float literal
            if (std::regex_match(src, std::regex(R"(-?\d+\.\d+)"))) {
                std::string destReg = getRegisterForVar(dest, isDestFloat, false);
                std::string floatLabel = getFloatConstant(src);
                if (isDestFloat) {
                    output.push_back("    l.s " + destReg + ", " + floatLabel + " # " + dest + " = " + src + "\n");
                } else {
                    std::string tempFloatReg = "$f31"; 
                    output.push_back("    l.s " + tempFloatReg + ", " + floatLabel + "\n");
                    output.push_back("    trunc.w.s " + tempFloatReg + ", " + tempFloatReg + "\n");
                    output.push_back("    mfc1 " + destReg + ", " + tempFloatReg + " # " + dest + " = (int)" + src + "\n");
                }
                markDirty(dest, destReg);
            } 
            // Case 2: Source is an integer literal
            else if (std::regex_match(src, std::regex(R"(-?\d+)"))) {
                std::string destReg = getRegisterForVar(dest, isDestFloat, false);
                if (isDestFloat) {
                    std::string tempIntReg = "$v1";
                    output.push_back("    li " + tempIntReg + ", " + src + "\n");
                    output.push_back("    mtc1 " + tempIntReg + ", " + destReg + " # " + dest + " = " + src + ".0\n");
                    output.push_back("    cvt.s.w " + destReg + ", " + destReg + "\n");
                } else {
                    output.push_back("    li " + destReg + ", " + src + " # " + dest + " = " + src + "\n");
                }
                markDirty(dest, destReg);
            } 
            // Case 3: Source is a character literal
            else if (src.find("'") == 0 && src.length() == 3) {
                std::string destReg = getRegisterForVar(dest, isDestFloat, false);
                char c = src[1];
                if (isDestFloat) {
                    std::string tempIntReg = "$v1";
                    output.push_back("    li " + tempIntReg + ", " + std::to_string((int)c) + "\n");
                    output.push_back("    mtc1 " + tempIntReg + ", " + destReg + " # " + dest + " = (float)'" + c + "'\n");
                    output.push_back("    cvt.s.w " + destReg + ", " + destReg + "\n");
                } else {
                    output.push_back("    li " + destReg + ", " + std::to_string((int)c) + " # " + dest + " = '" + std::string(1, c) + "'\n");
                }
                markDirty(dest, destReg);
            } 
            // Case 4: Source is a string literal
            else if (src.find("\"") == 0) {
                std::string destReg = getRegisterForVar(dest, isDestFloat, false);
                std::string stringLabel = getStringConstant(src);
                globalInitialValues[dest] = src;
                if (isDestFloat) {
                    output.push_back("    # WARNING: Assigning string address to float var\n");
                    output.push_back("    la " + destReg + ", " + stringLabel + " # " + dest + " = &\"...\"\n");
                } else {
                    output.push_back("    la " + destReg + ", " + stringLabel + " # " + dest + " = &\"...\"\n");
                }
                markDirty(dest, destReg);
            } 
            // Case 5: Source is a VARIABLE
            else {
                bool isSrcFloat = isFloatVar(src);
                
                if (isDestFloat == isSrcFloat) {
                    // Get source register (load the value)
                    std::string srcReg = getRegisterForVar(src, isSrcFloat, true);
                    // Get destination register (don't load old value)
                    std::string destReg = getRegisterForVar(dest, isDestFloat, false);
                    
                    if (isDestFloat) {
                        output.push_back("    mov.s " + destReg + ", " + srcReg + " # " + dest + " = " + src + "\n");
                    } else {
                        output.push_back("    move " + destReg + ", " + srcReg + " # " + dest + " = " + src + "\n");
                    }
                    markDirty(dest, destReg);
                } else {
                    // Handle typecasting as before
                    std::string srcReg = getRegisterForVar(src, isSrcFloat, true);
                    std::string destReg = getRegisterForVar(dest, isDestFloat, false);

                    if (isDestFloat && !isSrcFloat) {
                        // float_dest = int_var
                        output.push_back("    mtc1 " + srcReg + ", " + destReg + " # " + dest + " = (float)" + src + "\n");
                        output.push_back("    cvt.s.w " + destReg + ", " + destReg + "\n");
                    } else if (!isDestFloat && isSrcFloat) {
                        // int_dest = float_var
                        std::string tempFloatReg = srcReg;
                        output.push_back("    trunc.w.s " + tempFloatReg + ", " + tempFloatReg + "\n");
                        output.push_back("    mfc1 " + destReg + ", " + tempFloatReg + " # " + dest + " = (int)" + src + "\n");
                    }
                    markDirty(dest, destReg);
                }
            }
            return "# " + line + "\n";
        }
        
        // Conditional jumps
        std::regex condRegex(R"(ifFalse\s*(\w+)\s*goto\s*(\w+))");
        if (std::regex_search(line, match, condRegex)) {
            std::string condition = match[1];
            std::string label = match[2];
            std::string condReg = getRegisterForVar(condition, false, true);
            output.push_back("    beq " + condReg + ", $zero, " + sanitizeName(label) + " # ifFalse " + condition + "\n");
            return "# " + line + "\n";
        }
        
        std::regex condTrueRegex(R"(if\s*(\w+)\s*goto\s*(\w+))");
        if (std::regex_search(line, match, condTrueRegex)) {
            std::string condition = match[1];
            std::string label = match[2];
            std::string condReg = getRegisterForVar(condition, false, true);
            output.push_back("    bne " + condReg + ", $zero, " + sanitizeName(label) + " # if " + condition + "\n");
            return "# " + line + "\n";
        }
        
        // Unconditional jumps
        if (line.find("goto") == 0) {
            std::regex gotoRegex(R"(goto\s*(\w+))");
            std::smatch match;
            if (std::regex_search(line, match, gotoRegex)) {
                std::string label = match[1];
                spillAllDirtyRegisters(); // Spill before all jumps
                output.push_back("    j " + sanitizeName(label) + "\n");
            }
            return "# " + line + "\n";
        }
        
        // Labels
        if (std::regex_match(line, std::regex(R"(\w+:)"))) {
            spillAllDirtyRegisters();
            std::string label = line.substr(0, line.length() - 1);
            output.push_back(sanitizeName(label) + ":\n");
            return "# " + line + " (Label)\n"; 
        }
        
        // Return statement
        std::regex returnRegex(R"(return\s+(.+))");
        if (std::regex_search(line, match, returnRegex)) {
            std::string returnValue = match[1];
            
            if (std::regex_match(returnValue, std::regex(R"(-?\d+)"))) {
                output.push_back("    li $v0, " + returnValue + " # Return value\n");
            } 
            else if (std::regex_match(returnValue, std::regex(R"(-?\d+\.\d+)"))) {
                std::string floatLabel = getFloatConstant(returnValue);
                output.push_back("    l.s $f0, " + floatLabel + " # Return value\n");
            } 
            else if (returnValue.find("'") == 0 && returnValue.length() == 3) {
                 char c = returnValue[1];
                 output.push_back("    li $v0, " + std::to_string((int)c) + " # Return value '" + c + "'\n");
            }
            else {
                bool isFloat = isFloatVar(returnValue);
                std::string reg = getRegisterForVar(returnValue, isFloat, true);
                if (isFloat) {
                    output.push_back("    mov.s $f0, " + reg + " # Return value " + returnValue + "\n");
                } else {
                    output.push_back("    move $v0, " + reg + " # Return value " + returnValue + "\n");
                }
            }
            
            spillAllDirtyRegisters();
            output.push_back("    j " + sanitizeName(currentFunc) + "_return # Jump to epilogue\n");
            return "# " + line + "\n";
        }
        
        // Return without value
        if (line.find("return") == 0) {
            spillAllDirtyRegisters();
            output.push_back("    j " + sanitizeName(currentFunc) + "_return # Jump to epilogue\n");
            return "# " + line + "\n";
        }

        // Store
        std::regex storeRegex(R"(\*(\w+)\s*=\s*([\w\.-]+)\s*\[(\w+)\])");
        if (std::regex_search(line, match, storeRegex)) {
            std::string ptr = match[1];
            std::string src = match[2];
            std::string type = match[3];
            
            std::string ptrReg = getRegisterForVar(ptr, false, true);
            
            if (type == "FLOAT") {
                std::string srcReg = getRegisterForVar(src, true, true);
                output.push_back("    s.s " + srcReg + ", 0(" + ptrReg + ")  # *" + ptr + " = " + src + "\n");
            } else {
                std::string srcReg = getRegisterForVar(src, false, true);
                output.push_back("    sw " + srcReg + ", 0(" + ptrReg + ")  # *" + ptr + " = " + src + "\n");
            }
            return "# " + line + "\n";
        }

std::regex loadSimpleRegex(R"((\w+)\s*=\s*\*(\w+))");
if (std::regex_search(line, match, loadSimpleRegex)) {
    std::string dest = match[1];
    std::string ptr = match[2];
    
    output.push_back("    # LOAD OPERATION: " + dest + " = *" + ptr + "\n");
    
    // Infer type from the pointer variable
    bool isFloat = isFloatVar(ptr);
    
    // Load the pointer value (address) into a register
    std::string ptrReg = getRegisterForVar(ptr, false, true); // loadValue = true
    
    output.push_back("    # Pointer " + ptr + " contains address in register " + ptrReg + "\n");
    
    if (isFloat) {
        std::string destReg = getRegisterForVar(dest, true, false); // loadValue = false
        output.push_back("    l.s " + destReg + ", 0(" + ptrReg + ")  # " + dest + " = *" + ptr + " (float)\n");
        markDirty(dest, destReg);
    } else {
        std::string destReg = getRegisterForVar(dest, false, false); // loadValue = false
        output.push_back("    lw " + destReg + ", 0(" + ptrReg + ")  # " + dest + " = *" + ptr + " (int)\n");
        markDirty(dest, destReg);
    }
    return "# " + line + "\n";
}
std::regex loadRegex(R"((\w+)\s*=\s*\*(\w+)\s*\[(\w+)\])");
if (std::regex_search(line, match, loadRegex)) {
    std::string dest = match[1];
    std::string ptr = match[2];
    std::string type = match[3];

    output.push_back("    # LOAD OPERATION: " + dest + " = *" + ptr + " [" + type + "]\n");
    
    std::string ptrReg = getRegisterForVar(ptr, false, true); // loadValue = true
    
    output.push_back("    # Pointer " + ptr + " contains address in register " + ptrReg + "\n");
    
    if (type == "FLOAT") {
        std::string destReg = getRegisterForVar(dest, true, false); // loadValue = false
        output.push_back("    l.s " + destReg + ", 0(" + ptrReg + ")  # " + dest + " = *" + ptr + " (float)\n");
        markDirty(dest, destReg);
    } else {
        std::string destReg = getRegisterForVar(dest, false, false); // loadValue = false
        output.push_back("    lw " + destReg + ", 0(" + ptrReg + ")  # " + dest + " = *" + ptr + " (int)\n");
        markDirty(dest, destReg);
    }
    return "# " + line + "\n";
}
        
        return "# UNHANDLED: " + line + "\n";
    }

    void convertIntOperation(const std::string& dest, const std::string& left, const std::string& op, const std::string& right) {
    bool isFloat = false;
    std::string destReg = getRegisterForVar(dest, isFloat, false);
    std::string leftReg = getRegisterForVar(left, isFloat, true);
    
    std::string rightReg;
    bool rightIsLiteral = std::regex_match(right, std::regex(R"(-?\d+)"));

    std::string opCode;
    if (op == "+") opCode = rightIsLiteral ? "addiu" : "addu";
    else if (op == "-") opCode = rightIsLiteral ? "addiu" : "subu"; // addiu dest, src, -literal
    else if (op == "*") opCode = "mul";
    else if (op == "/") opCode = "div";
    else if (op == "&") opCode = rightIsLiteral ? "andi" : "and";
    else if (op == "|") opCode = rightIsLiteral ? "ori" : "or";
    else if (op == "^") opCode = rightIsLiteral ? "xori" : "xor";
    else if (op == "<<") opCode = rightIsLiteral ? "sll" : "sllv";
    else if (op == ">>") opCode = rightIsLiteral ? "srl" : "srlv";
    
    if (rightIsLiteral) {
        std::string rightVal = right;
        if (op == "-") {
            // MIPS doesn't have 'subi', so we use 'addi' with a negated literal
            try {
                rightVal = std::to_string(-std::stoi(right));
            } catch (...) { rightVal = "0"; }
            opCode = "addiu"; 
        }
        
        if (opCode == "sll" || opCode == "srl") {
            output.push_back("    " + opCode + " " + destReg + ", " + leftReg + ", " + rightVal + " # " + dest + " = " + left + " " + op + " " + right + "\n");
        } else {
            output.push_back("    " + opCode + " " + destReg + ", " + leftReg + ", " + rightVal + " # " + dest + " = " + left + " " + op + " " + right + "\n");
        }
    } 
    else {
        rightReg = getRegisterForVar(right, isFloat, true);
        if (op == "*") {
            // Use 3-operand mul instruction
            output.push_back("    mul " + destReg + ", " + leftReg + ", " + rightReg + " # " + dest + " = " + left + " * " + right + "\n");
        } else if (op == "/") {
            // Correct division syntax
            output.push_back("    div " + leftReg + ", " + rightReg + " # " + dest + " = " + left + " / " + right + "\n");
            output.push_back("    mflo " + destReg + "\n");
        } else if (op == "sllv" || op == "srlv") {
            // Shift variable operations
            output.push_back("    " + opCode + " " + destReg + ", " + leftReg + ", " + rightReg + " # " + dest + " = " + left + " " + op + " " + right + "\n");
        } else {
            output.push_back("    " + opCode + " " + destReg + ", " + leftReg + ", " + rightReg + " # " + dest + " = " + left + " " + op + " " + right + "\n");
        }
    }
    markDirty(dest, destReg);
}

    void convertFloatOperation(const std::string& dest, const std::string& left, const std::string& op, const std::string& right) {
        bool isFloat = true;
        std::string destReg = getRegisterForVar(dest, isFloat, false);
        std::string leftReg = getRegisterForVar(left, isFloat, true);
        
        std::string rightReg;
        
        if (std::regex_match(right, std::regex(R"(-?\d+\.\d+)"))) {
            std::string floatLabel = getFloatConstant(right);
            rightReg = "$f31"; // Use reserved temp
            output.push_back("    l.s " + rightReg + ", " + floatLabel + " # Load literal " + right + "\n");
        } 
        else if (std::regex_match(right, std::regex(R"(-?\d+)"))) {
             rightReg = "$f31";
             std::string tempIntReg = "$v1";
             output.push_back("    li " + tempIntReg + ", " + right + "\n");
             output.push_back("    mtc1 " + tempIntReg + ", " + rightReg + "\n");
             output.push_back("    cvt.s.w " + rightReg + ", " + rightReg + " # Convert literal " + right + " to float\n");
        }
        else {
            rightReg = getRegisterForVar(right, isFloat, true);
        }

        std::string opCode;
        if (op == "+") opCode = "add.s";
        else if (op == "-") opCode = "sub.s";
        else if (op == "*") opCode = "mul.s";
        else if (op == "/") opCode = "div.s";

        output.push_back("    " + opCode + " " + destReg + ", " + leftReg + ", " + rightReg + " # " + dest + " = " + left + " " + op + " " + right + "\n");
        markDirty(dest, destReg);
    }

    void convertIntComparison(const std::string& dest, const std::string& left, const std::string& op, const std::string& right) {
    std::string destReg = getRegisterForVar(dest, false, false);
    std::string leftReg = getRegisterForVar(left, false, true);
    
    if (std::regex_match(right, std::regex(R"(-?\d+)"))) {
        // Handle literal comparisons
        if (op == "<") {
            output.push_back("    slti " + destReg + ", " + leftReg + ", " + right + " # " + dest + " = " + left + " < " + right + "\n");
        } else if (op == "<=") {
            // slti + equality check
            std::string tempReg = (leftReg == "$t0") ? "$t1" : "$t0";
            output.push_back("    slti " + destReg + ", " + leftReg + ", " + std::to_string(std::stoi(right) + 1) + " # " + dest + " = " + left + " <= " + right + "\n");
        } else if (op == ">") {
            // Use slt with swapped operands
            std::string tempReg = "$v1";
            output.push_back("    li " + tempReg + ", " + right + "\n");
            output.push_back("    slt " + destReg + ", " + tempReg + ", " + leftReg + " # " + dest + " = " + left + " > " + right + "\n");
        } else if (op == ">=") {
            // slt + invert
            std::string tempReg = "$v1";
            output.push_back("    li " + tempReg + ", " + right + "\n");
            output.push_back("    slt " + destReg + ", " + leftReg + ", " + tempReg + " # " + dest + " = " + left + " < " + right + "\n");
            output.push_back("    xori " + destReg + ", " + destReg + ", 1 # Invert to get >= " + right + "\n");
        } else if (op == "==") {
            std::string tempReg = "$v1";
            output.push_back("    li " + tempReg + ", " + right + "\n");
            output.push_back("    xor " + destReg + ", " + leftReg + ", " + tempReg + " # " + dest + " = " + left + " == " + right + "\n");
            output.push_back("    sltiu " + destReg + ", " + destReg + ", 1 # Set to 1 if equal\n");
        } else if (op == "!=") {
            std::string tempReg = "$v1";
            output.push_back("    li " + tempReg + ", " + right + "\n");
            output.push_back("    xor " + destReg + ", " + leftReg + ", " + tempReg + " # " + dest + " = " + left + " != " + right + "\n");
            output.push_back("    sltu " + destReg + ", $zero, " + destReg + " # Set to 1 if not equal\n");
        }
    } else {
        // Handle variable comparisons
        std::string rightReg = getRegisterForVar(right, false, true);
        if (op == "<") {
            output.push_back("    slt " + destReg + ", " + leftReg + ", " + rightReg + " # " + dest + " = " + left + " < " + right + "\n");
        } else if (op == "<=") {
            // slt + equality check
            output.push_back("    slt " + destReg + ", " + rightReg + ", " + leftReg + " # " + dest + " = " + right + " < " + left + "\n");
            output.push_back("    xori " + destReg + ", " + destReg + ", 1 # Invert to get " + left + " <= " + right + "\n");
        } else if (op == ">") {
            output.push_back("    slt " + destReg + ", " + rightReg + ", " + leftReg + " # " + dest + " = " + left + " > " + right + "\n");
        } else if (op == ">=") {
            output.push_back("    slt " + destReg + ", " + leftReg + ", " + rightReg + " # " + dest + " = " + left + " < " + right + "\n");
            output.push_back("    xori " + destReg + ", " + destReg + ", 1 # Invert to get " + left + " >= " + right + "\n");
        } else if (op == "==") {
            output.push_back("    xor " + destReg + ", " + leftReg + ", " + rightReg + " # " + dest + " = " + left + " == " + right + "\n");
            output.push_back("    sltiu " + destReg + ", " + destReg + ", 1 # Set to 1 if equal\n");
        } else if (op == "!=") {
            output.push_back("    xor " + destReg + ", " + leftReg + ", " + rightReg + " # " + dest + " = " + left + " != " + right + "\n");
            output.push_back("    sltu " + destReg + ", $zero, " + destReg + " # Set to 1 if not equal\n");
        }
    }
    markDirty(dest, destReg);
}
    
    void convertFloatComparison(const std::string& dest, const std::string& left, const std::string& op, const std::string& right) {
        std::string leftReg = getRegisterForVar(left, true, true);
        std::string rightReg;
        
        if (std::regex_match(right, std::regex(R"(-?\d+\.\d+)"))) {
            std::string floatLabel = getFloatConstant(right);
            rightReg = "$f31"; // Use reserved temp
            output.push_back("    l.s " + rightReg + ", " + floatLabel + " # Load literal " + right + "\n");
        } else {
            rightReg = getRegisterForVar(right, true, true);
        }

        std::string destReg = getRegisterForVar(dest, false, false);
        std::string trueLabel = generateLabel("float_true_");
        std::string endLabel = generateLabel("float_end_");

        std::string opCode;
        if (op == "==") opCode = "c.eq.s";
        else if (op == "<") opCode = "c.lt.s";
        else if (op == "<=") opCode = "c.le.s";
        else if (op == "!=") opCode = "c.eq.s"; // Inverted logic
        else if (op == ">") opCode = "c.lt.s"; // Swapped operands
        else if (op == ">=") opCode = "c.le.s"; // Swapped operands

        if (op == ">" || op == ">=") {
            output.push_back("    " + opCode + " " + rightReg + ", " + leftReg + " # " + right + " " + (op == ">" ? "<" : "<=") + " " + left + "\n");
        } else {
            output.push_back("    " + opCode + " " + leftReg + ", " + rightReg + " # " + left + " " + op + " " + right + "\n");
        }
        
        output.push_back("    li " + destReg + ", 0 # Assume false\n");
        
        if (op == "!=") {
            output.push_back("    bc1f " + endLabel + " # Branch if false (not equal)\n");
        } else {
            output.push_back("    bc1t " + trueLabel + " # Branch if true\n");
            output.push_back("    j " + endLabel + "\n");
            output.push_back(trueLabel + ":\n");
            output.push_back("    li " + destReg + ", 1 # Set true\n");
            output.push_back(endLabel + ":\n");
        }
        
        if (op == "!=") {
             output.push_back(endLabel + ":\n");
             output.push_back("    li " + destReg + ", 1 # Set true\n");
        }

        markDirty(dest, destReg);
    }

    void convertFunctionCall(const std::string& funcName, int numArgs, const std::string& resultVar) {
    
    spillAllDirtyRegisters();
    if (funcName == "printf") {
        convertPrintfCall(numArgs);
    } else if (funcName == "scanf") {
        convertScanfCall(numArgs,"");
    } else if (funcName == "malloc") {
        convertMallocCall(numArgs, resultVar);
    } else if (funcName == "free") {
        convertFreeCall(numArgs);
    } 
    else {
        // For user-defined functions, save critical registers
        output.push_back("    # Saving critical registers before call to " + funcName + "\n");
        
        // Save important temporary registers that might be clobbered
        std::vector<std::string> regsToSave;
        for (const auto& reg : availableIntRegs) {
            if (!intRegisters[reg].varNames.empty()) {
                output.push_back("    addiu $sp, $sp, -4\n");
                output.push_back("    sw " + reg + ", 0($sp)\n");
                regsToSave.push_back(reg);
            }
        }
        
        // Also save argument registers if they contain important values
        for (const auto& reg : argIntRegs) {
            if (!intRegisters[reg].varNames.empty()) {
                output.push_back("    addiu $sp, $sp, -4\n");
                output.push_back("    sw " + reg + ", 0($sp)\n");
                regsToSave.push_back(reg);
            }
        }

        // Handle arguments (from paramStack)
        if (numArgs > 0) {
            int intArgCount = 0;
            int floatArgCount = 0;
            int stackArgCount = 0;

            // Allocate stack space for arguments 5+
            if (numArgs > 4) {
                int extraArgs = numArgs - 4;
                argStackOffset = -(extraArgs * 4);
                output.push_back("    addiu $sp, $sp, " + std::to_string(argStackOffset) + " # Make space for " + std::to_string(extraArgs) + " extra arguments\n");
            }

            for (int i = 0; i < numArgs; ++i) {
                std::string arg = paramStack.back();
                paramStack.pop_back();
                
                bool isFloat = isFloatVar(arg);
                std::string argReg = getRegisterForVar(arg, isFloat, true);

                if (isFloat && floatArgCount < argFloatRegs.size()) {
                    output.push_back("    mov.s " + argFloatRegs[floatArgCount++] + ", " + argReg + "  # Float argument " + std::to_string(i+1) + "\n");
                } else if (!isFloat && intArgCount < argIntRegs.size()) {
                    output.push_back("    move " + argIntRegs[intArgCount++] + ", " + argReg + "  # Integer argument " + std::to_string(i+1) + "\n");
                } else {
                    // Argument goes on the stack
                    int stackOffset = stackArgCount * 4;
                    output.push_back("    # Argument " + std::to_string(i+1) + " on stack at offset " + std::to_string(stackOffset) + "\n");
                    if (isFloat) {
                        output.push_back("    s.s " + argReg + ", " + std::to_string(stackOffset) + "($sp)\n");
                    } else {
                        output.push_back("    sw " + argReg + ", " + std::to_string(stackOffset) + "($sp)\n");
                    }
                    stackArgCount++;
                }
            }
        }

        output.push_back("    jal " + sanitizeName(funcName) + "  # Function call\n");
        
        // Deallocate stack space for arguments 5+
        if (argStackOffset < 0) {
            output.push_back("    addiu $sp, $sp, " + std::to_string(-argStackOffset) + " # Deallocate stack arguments\n");
            argStackOffset = 0;
        }

        // Restore saved registers
        for (int i = regsToSave.size() - 1; i >= 0; i--) {
            output.push_back("    lw " + regsToSave[i] + ", 0($sp)\n");
            output.push_back("    addiu $sp, $sp, 4\n");
        }

        invalidateTempRegisters();
        
        // Store return value
        if (!resultVar.empty()) {
            bool isFloat = isFloatVar(resultVar);
            std::string destReg = getRegisterForVar(resultVar, isFloat, false); // Don't load old value
            
            if (isFloat) {
                output.push_back("    mov.s " + destReg + ", $f0  # Store float return value\n");
            } else {
                output.push_back("    move " + destReg + ", $v0  # Store return value\n");
            }
            markDirty(resultVar, destReg);
        }
    }
}

    void convertPrintfCall(int numArgs) {
    output.push_back("    # printf call\n");

    // Get the format string variable and remove it from paramStack
    std::string formatStrVar = paramStack.back();
    paramStack.pop_back();
    std::string actualFormatString;

    // Look up its literal value
    if (globalInitialValues.count(formatStrVar)) {
        actualFormatString = globalInitialValues[formatStrVar];
    } else {
        output.push_back("    # ERROR: Could not find format string for " + formatStrVar + "\n");
        return;
    }

    // Clean the quotes
    if (!actualFormatString.empty() && actualFormatString.front() == '"' && actualFormatString.back() == '"') {
        actualFormatString = actualFormatString.substr(1, actualFormatString.length() - 2);
    }

    std::vector<std::string> printfVars;
    for (int i = 0; i < numArgs - 1; i++) {
        if (!paramStack.empty()) {
            printfVars.push_back(paramStack.back());
            paramStack.pop_back();
        }
    }

    int varIndex = 0;
    int lastIndex = 0;

    for (int i = 0; i < actualFormatString.length(); ++i) {
        if (actualFormatString[i] == '%') {
            // Print any literal chunk that came before '%'
            if (i > lastIndex) {
                std::string chunk = actualFormatString.substr(lastIndex, i - lastIndex);
                if (!chunk.empty()) {
                    std::string label = getStringConstant(chunk);
                    output.push_back("    li $v0, 4           # Print string\n");
                    output.push_back("    la $a0, " + label + "\n");
                    output.push_back("    syscall\n");
                }
            }

            // Process the format specifier
            if (i + 1 < actualFormatString.length()) {
                char specifier = actualFormatString[i+1];
                
                if (specifier == '%') {
                    // Literal %%
                    output.push_back("    li $v0, 11          # Print char\n");
                    output.push_back("    li $a0, 37          # '%' character\n");
                    output.push_back("    syscall\n");
                    i++; // Skip the second %
                } 
                else if (varIndex < printfVars.size()) {
                    std::string varName = printfVars[varIndex];
                    varIndex++;

                    // Load the variable value from its storage location
                    if (specifier == 'd' || specifier == 'i') {
                        if (isGlobalVar(varName)) {
                            // Load from global
                            output.push_back("    lw $t9, " + sanitizeName(varName) + "  # Load " + varName + "\n");
                            output.push_back("    li $v0, 1           # Print integer\n");
                            output.push_back("    move $a0, $t9\n");
                            output.push_back("    syscall\n");
                        } 
                        else if (variableInfo.count(varName) && variableInfo[varName].stackOffset != -1) {
                            // Load from stack
                            int offset = variableInfo[varName].stackOffset;
                            output.push_back("    lw $t9, " + std::to_string(offset) + "($fp)  # Load " + varName + "\n");
                            output.push_back("    li $v0, 1           # Print integer\n");
                            output.push_back("    move $a0, $t9\n");
                            output.push_back("    syscall\n");
                        }
                        else {
                            // Fallback: try register
                            std::string varReg = getRegisterForVar(varName, false, true);
                            output.push_back("    li $v0, 1           # Print integer\n");
                            output.push_back("    move $a0, " + varReg + "\n");
                            output.push_back("    syscall\n");
                        }
                    } 
                    else if (specifier == 'f') {
                        if (isGlobalVar(varName)) {
                            output.push_back("    l.s $f12, " + sanitizeName(varName) + "  # Load float " + varName + "\n");
                        } 
                        else if (variableInfo.count(varName) && variableInfo[varName].stackOffset != -1) {
                            int offset = variableInfo[varName].stackOffset;
                            output.push_back("    l.s $f12, " + std::to_string(offset) + "($fp)  # Load float " + varName + "\n");
                        }
                        else {
                            std::string varReg = getRegisterForVar(varName, true, true);
                            output.push_back("    mov.s $f12, " + varReg + "  # Load float " + varName + "\n");
                        }
                        output.push_back("    li $v0, 2           # Print float\n");
                        output.push_back("    syscall\n");
                    } 
                    else if (specifier == 's') {
                        std::string varReg = getRegisterForVar(varName, false, true);
                        output.push_back("    li $v0, 4           # Print string\n");
                        output.push_back("    move $a0, " + varReg + "\n");
                        output.push_back("    syscall\n");
                    } 
                    else if (specifier == 'c') {
                        if (isGlobalVar(varName)) {
                            output.push_back("    lb $t9, " + sanitizeName(varName) + "  # Load char " + varName + "\n");
                        } 
                        else if (variableInfo.count(varName) && variableInfo[varName].stackOffset != -1) {
                            int offset = variableInfo[varName].stackOffset;
                            output.push_back("    lb $t9, " + std::to_string(offset) + "($fp)  # Load char " + varName + "\n");
                        }
                        else {
                            std::string varReg = getRegisterForVar(varName, false, true);
                            output.push_back("    move $t9, " + varReg + "  # Load char " + varName + "\n");
                        }
                        output.push_back("    li $v0, 11          # Print char\n");
                        output.push_back("    move $a0, $t9\n");
                        output.push_back("    syscall\n");
                    }
                    i++; // Skip the specifier character
                }
                lastIndex = i + 1; // Update start of next chunk
            }
        }
    }

    // Print any remaining literal chunk after the last specifier
    if (lastIndex < actualFormatString.length()) {
        std::string chunk = actualFormatString.substr(lastIndex);
        if (!chunk.empty()) {
            std::string label = getStringConstant(chunk);
            output.push_back("    li $v0, 4           # Print string\n");
            output.push_back("    la $a0, " + label + "\n");
            output.push_back("    syscall\n");
        }
    }
    
    invalidateSpecificRegisters({"$v0", "$a0", "$f12", "$t9"});
}

    void convertScanfCall(int numArgs, const std::string& resultVar) {
    (void)resultVar;
    
    output.push_back("    # scanf call - \n");

    // Get format string
    std::string formatStrVar = paramStack.back();
    std::string actualFormatString;

    if (globalInitialValues.count(formatStrVar)) {
        actualFormatString = globalInitialValues[formatStrVar];
    } else {
        output.push_back("    # ERROR: Could not find format string\n");
        return;
    }

    // Clean the quotes
    if (!actualFormatString.empty() && actualFormatString.front() == '"' && actualFormatString.back() == '"') {
        actualFormatString = actualFormatString.substr(1, actualFormatString.length() - 2);
    }

    // Save all registers that might be clobbered
    output.push_back("    addiu $sp, $sp, -40     # Save ALL registers\n");
    output.push_back("    sw $t0, 0($sp)\n");
    output.push_back("    sw $t1, 4($sp)\n");
    output.push_back("    sw $t2, 8($sp)\n");
    output.push_back("    sw $t3, 12($sp)\n");
    output.push_back("    sw $t4, 16($sp)\n");
    output.push_back("    sw $t5, 20($sp)\n");
    output.push_back("    sw $t6, 24($sp)\n");
    output.push_back("    sw $t7, 28($sp)\n");
    output.push_back("    sw $t8, 32($sp)\n");
    output.push_back("    sw $ra, 36($sp)\n");

    // Read input into buffer
    output.push_back("    addiu $sp, $sp, -100    # Allocate 100 bytes for input buffer\n");
    output.push_back("    li $v0, 8               # Read string syscall\n");
    output.push_back("    move $a0, $sp           # Buffer address\n");
    output.push_back("    li $a1, 100             # Buffer size\n");
    output.push_back("    syscall\n");
    
    // Use a safe register for buffer pointer
    output.push_back("    move $t8, $sp           # $t8 = safe buffer pointer\n");

    // Extract address variables from paramStack
    std::vector<std::string> addressVars;
    
    // Remove format string first
    if (!paramStack.empty()) {
        paramStack.pop_back();
    }
    
    // Extract address variables
    for (int i = 0; i < numArgs - 1; i++) {
        if (!paramStack.empty()) {
            addressVars.push_back(paramStack.back());
            paramStack.pop_back();
        }
    }
    
    output.push_back("    # Processing " + std::to_string(addressVars.size()) + " variables for scanf\n");

    // Parse variables according to format string specifiers
    int addrIndex = 0;
    for (size_t i = 0; i < actualFormatString.length() && addrIndex < addressVars.size(); i++) {
        if (actualFormatString[i] == '%' && i + 1 < actualFormatString.length()) {
            char specifier = actualFormatString[i + 1];
            if (specifier == '%') {
                // Skip literal %%
                i++;
                continue;
            }
            
            std::string addrHolderVar = addressVars[addrIndex];
            addrIndex++;

            // Get the pointer register for this address
            std::string addrReg = getRegisterForVar(addrHolderVar, false, true); // loadValue = true

            output.push_back("    # Parsing " + std::string(1, specifier) + " for variable " + addrHolderVar + "\n");

            if (specifier == 'd' || specifier == 'i') {
                // Parse integer
                output.push_back("    move $a0, $t8          # Pass buffer pointer\n");
                output.push_back("    jal parse_int_safe\n");
                output.push_back("    move $t8, $v1          # Update buffer pointer\n");
                output.push_back("    sw $v0, 0(" + addrReg + ")  # Store integer via pointer\n");
            }
            else if (specifier == 'f') {
                // Parse float
                output.push_back("    move $a0, $t8          # Pass buffer pointer\n");
                output.push_back("    jal parse_float_safe\n");
                output.push_back("    move $t8, $v1          # Update buffer pointer\n");
                output.push_back("    s.s $f0, 0(" + addrReg + ")  # Store float via pointer\n");
            }
            else if (specifier == 'c') {
                // Parse character
                output.push_back("    move $a0, $t8          # Pass buffer pointer\n");
                output.push_back("    jal parse_char_safe\n");
                output.push_back("    move $t8, $v1          # Update buffer pointer\n");
                output.push_back("    sb $v0, 0(" + addrReg + ")  # Store char via pointer\n");
            }
            else if (specifier == 's') {
                // Parse string
                output.push_back("    move $a0, $t8          # Pass buffer pointer\n");
                output.push_back("    move $a1, " + addrReg + "    # Pass destination buffer\n");
                output.push_back("    jal parse_string_safe\n");
                output.push_back("    move $t8, $v0          # Update buffer pointer\n");
            }
            i++; // Skip specifier
        }
    }

    // Cleanup
    output.push_back("    addiu $sp, $sp, 100     # Deallocate buffer\n");
    output.push_back("    lw $t0, 0($sp)\n");
    output.push_back("    lw $t1, 4($sp)\n");
    output.push_back("    lw $t2, 8($sp)\n");
    output.push_back("    lw $t3, 12($sp)\n");
    output.push_back("    lw $t4, 16($sp)\n");
    output.push_back("    lw $t5, 20($sp)\n");
    output.push_back("    lw $t6, 24($sp)\n");
    output.push_back("    lw $t7, 28($sp)\n");
    output.push_back("    lw $t8, 32($sp)\n");
    output.push_back("    lw $ra, 36($sp)\n");
    output.push_back("    addiu $sp, $sp, 40      # Restore all registers\n");

    // Invalidate cached registers for all modified variables
    for (const auto& addrVar : addressVars) {
        // Get the actual target variable from the address holder
        std::string targetVar;
        if (variableInfo.count(addrVar) && !variableInfo[addrVar].pointsTo.empty()) {
            targetVar = variableInfo[addrVar].pointsTo;
        } else {
            targetVar = addrVar;
        }
        
        output.push_back("    # Invalidating cached registers for " + targetVar + "\n");
        if (variableInfo.count(targetVar)) {
            variableInfo[targetVar].dirty = false;
            std::set<std::string> regsToRemove = variableInfo[targetVar].registers;
            for (const auto& reg : regsToRemove) {
                if (intRegisters.count(reg)) {
                    intRegisters[reg].varNames.erase(targetVar);
                } else if (floatRegisters.count(reg)) {
                    floatRegisters[reg].varNames.erase(targetVar);
                }
            }
            variableInfo[targetVar].registers.clear();
        }
    }

    output.push_back("    j scanf_complete        # Skip parsing routines\n");

    // Include all parsing subroutines
    output.push_back("\n    #--------------------------------------\n");
    output.push_back("    # scanf parsing subroutines\n");
    output.push_back("    #--------------------------------------\n");

    // Integer parsing
    output.push_back("parse_int_safe:\n");
    output.push_back("    move $t9, $a0           # $t9 = buffer pointer\n");
    output.push_back("    addiu $sp, $sp, -16     # Save registers\n");
    output.push_back("    sw $s0, 0($sp)\n");
    output.push_back("    sw $s1, 4($sp)\n");
    output.push_back("    sw $s2, 8($sp)\n");
    output.push_back("    sw $s3, 12($sp)\n");
    output.push_back("    \n");
    output.push_back("    li $v0, 0               # result = 0\n");
    output.push_back("    li $s1, 1               # sign = 1\n");
    output.push_back("    li $s3, 0               # digit count\n");
    output.push_back("    \n");
    output.push_back("    # Skip leading whitespace\n");
    output.push_back("skip_ws_loop_int:\n");
    output.push_back("    lb $s0, 0($t9)\n");
    output.push_back("    beqz $s0, parse_int_end\n");
    output.push_back("    beq $s0, 32, skip_ws_int    # space\n");
    output.push_back("    beq $s0, 9, skip_ws_int     # tab\n");
    output.push_back("    beq $s0, 10, skip_ws_int    # newline\n");
    output.push_back("    beq $s0, 13, skip_ws_int    # carriage return\n");
    output.push_back("    j check_sign_int\n");
    output.push_back("skip_ws_int:\n");
    output.push_back("    addiu $t9, $t9, 1\n");
    output.push_back("    j skip_ws_loop_int\n");
    output.push_back("    \n");
    output.push_back("check_sign_int:\n");
    output.push_back("    lb $s0, 0($t9)\n");
    output.push_back("    bne $s0, 45, parse_digits_int\n");
    output.push_back("    li $s1, -1              # negative sign\n");
    output.push_back("    addiu $t9, $t9, 1\n");
    output.push_back("    \n");
    output.push_back("parse_digits_int:\n");
    output.push_back("    lb $s0, 0($t9)\n");
    output.push_back("    beqz $s0, parse_int_end\n");
    output.push_back("    \n");
    output.push_back("    # Check for delimiter\n");
    output.push_back("    beq $s0, 32, parse_int_end  # space\n");
    output.push_back("    beq $s0, 9, parse_int_end   # tab\n");
    output.push_back("    beq $s0, 10, parse_int_end  # newline\n");
    output.push_back("    beq $s0, 13, parse_int_end  # carriage return\n");
    output.push_back("    beq $s0, 44, parse_int_end  # comma\n");
    output.push_back("    \n");
    output.push_back("    # Check if digit\n");
    output.push_back("    blt $s0, 48, parse_int_end  # < '0'\n");
    output.push_back("    bgt $s0, 57, parse_int_end  # > '9'\n");
    output.push_back("    \n");
    output.push_back("    # Convert and accumulate\n");
    output.push_back("    mul $v0, $v0, 10\n");
    output.push_back("    addiu $s0, $s0, -48\n");
    output.push_back("    add $v0, $v0, $s0\n");
    output.push_back("    addiu $s3, $s3, 1       # increment digit count\n");
    output.push_back("    addiu $t9, $t9, 1\n");
    output.push_back("    j parse_digits_int\n");
    output.push_back("    \n");
    output.push_back("parse_int_end:\n");
    output.push_back("    mul $v0, $v0, $s1       # apply sign\n");
    output.push_back("    move $v1, $t9           # return updated pointer\n");
    output.push_back("    \n");
    output.push_back("    lw $s0, 0($sp)\n");
    output.push_back("    lw $s1, 4($sp)\n");
    output.push_back("    lw $s2, 8($sp)\n");
    output.push_back("    lw $s3, 12($sp)\n");
    output.push_back("    addiu $sp, $sp, 16\n");
    output.push_back("    jr $ra\n");

    // Float parsing
    output.push_back("parse_float_safe:\n");
    output.push_back("    move $t9, $a0           # $t9 = buffer pointer\n");
    output.push_back("    addiu $sp, $sp, -24     # Save registers\n");
    output.push_back("    sw $s0, 0($sp)\n");
    output.push_back("    sw $s1, 4($sp)\n");
    output.push_back("    sw $s2, 8($sp)\n");
    output.push_back("    sw $s3, 12($sp)\n");
    output.push_back("    sw $s4, 16($sp)\n");
    output.push_back("    sw $ra, 20($sp)\n");
    output.push_back("    \n");
    output.push_back("    # Parse integer part\n");
    output.push_back("    move $a0, $t9\n");
    output.push_back("    jal parse_int_safe\n");
    output.push_back("    move $t9, $v1           # Update pointer\n");
    output.push_back("    mtc1 $v0, $f0           # Convert to float\n");
    output.push_back("    cvt.s.w $f0, $f0\n");
    output.push_back("    \n");
    output.push_back("    # Check for decimal point\n");
    output.push_back("    lb $s0, 0($t9)\n");
    output.push_back("    bne $s0, 46, parse_float_end  # Not '.', done\n");
    output.push_back("    addiu $t9, $t9, 1       # Skip '.'\n");
    output.push_back("    \n");
    output.push_back("    # Parse fractional part\n");
    output.push_back("    li $s1, 0               # fractional value\n");
    output.push_back("    li $s2, 1               # divisor\n");
    output.push_back("    li $s3, 0               # digit count\n");
    output.push_back("    \n");
    output.push_back("parse_frac_digits:\n");
    output.push_back("    lb $s0, 0($t9)\n");
    output.push_back("    beqz $s0, parse_frac_end\n");
    output.push_back("    blt $s0, 48, parse_frac_end  # < '0'\n");
    output.push_back("    bgt $s0, 57, parse_frac_end  # > '9'\n");
    output.push_back("    \n");
    output.push_back("    # Convert and accumulate fractional part\n");
    output.push_back("    mul $s1, $s1, 10\n");
    output.push_back("    addiu $s0, $s0, -48\n");
    output.push_back("    add $s1, $s1, $s0\n");
    output.push_back("    mul $s2, $s2, 10        # Update divisor\n");
    output.push_back("    addiu $t9, $t9, 1\n");
    output.push_back("    j parse_frac_digits\n");
    output.push_back("    \n");
    output.push_back("parse_frac_end:\n");
    output.push_back("    # Convert fractional part to float and add\n");
    output.push_back("    mtc1 $s1, $f1\n");
    output.push_back("    cvt.s.w $f1, $f1\n");
    output.push_back("    mtc1 $s2, $f2\n");
    output.push_back("    cvt.s.w $f2, $f2\n");
    output.push_back("    div.s $f1, $f1, $f2     # fractional part\n");
    output.push_back("    add.s $f0, $f0, $f1     # add to integer part\n");
    output.push_back("    \n");
    output.push_back("parse_float_end:\n");
    output.push_back("    move $v1, $t9           # return updated pointer\n");
    output.push_back("    \n");
    output.push_back("    lw $s0, 0($sp)\n");
    output.push_back("    lw $s1, 4($sp)\n");
    output.push_back("    lw $s2, 8($sp)\n");
    output.push_back("    lw $s3, 12($sp)\n");
    output.push_back("    lw $s4, 16($sp)\n");
    output.push_back("    lw $ra, 20($sp)\n");
    output.push_back("    addiu $sp, $sp, 24\n");
    output.push_back("    jr $ra\n");

    // Character parsing
    output.push_back("parse_char_safe:\n");
    output.push_back("    move $t9, $a0           # $t9 = buffer pointer\n");
    output.push_back("    addiu $sp, $sp, -4      # Save register\n");
    output.push_back("    sw $s0, 0($sp)\n");
    output.push_back("    \n");
    output.push_back("    # Skip leading whitespace\n");
    output.push_back("skip_ws_char:\n");
    output.push_back("    lb $s0, 0($t9)\n");
    output.push_back("    beqz $s0, parse_char_end\n");
    output.push_back("    beq $s0, 32, skip_ws_char_next    # space\n");
    output.push_back("    beq $s0, 9, skip_ws_char_next     # tab\n");
    output.push_back("    beq $s0, 10, skip_ws_char_next    # newline\n");
    output.push_back("    beq $s0, 13, skip_ws_char_next    # carriage return\n");
    output.push_back("    j parse_char_found\n");
    output.push_back("skip_ws_char_next:\n");
    output.push_back("    addiu $t9, $t9, 1\n");
    output.push_back("    j skip_ws_char\n");
    output.push_back("    \n");
    output.push_back("parse_char_found:\n");
    output.push_back("    move $v0, $s0           # return character\n");
    output.push_back("    addiu $t9, $t9, 1       # advance past character\n");
    output.push_back("    \n");
    output.push_back("parse_char_end:\n");
    output.push_back("    move $v1, $t9           # return updated pointer\n");
    output.push_back("    lw $s0, 0($sp)\n");
    output.push_back("    addiu $sp, $sp, 4\n");
    output.push_back("    jr $ra\n");

    // String parsing
    output.push_back("parse_string_safe:\n");
    output.push_back("    move $t9, $a0           # $t9 = buffer pointer\n");
    output.push_back("    move $t7, $a1           # $t7 = destination buffer\n");
    output.push_back("    addiu $sp, $sp, -8      # Save registers\n");
    output.push_back("    sw $s0, 0($sp)\n");
    output.push_back("    sw $s1, 4($sp)\n");
    output.push_back("    \n");
    output.push_back("    # Skip leading whitespace\n");
    output.push_back("skip_ws_string:\n");
    output.push_back("    lb $s0, 0($t9)\n");
    output.push_back("    beqz $s0, parse_string_end\n");
    output.push_back("    beq $s0, 32, skip_ws_string_next    # space\n");
    output.push_back("    beq $s0, 9, skip_ws_string_next     # tab\n");
    output.push_back("    beq $s0, 10, skip_ws_string_next    # newline\n");
    output.push_back("    beq $s0, 13, skip_ws_string_next    # carriage return\n");
    output.push_back("    j parse_string_found\n");
    output.push_back("skip_ws_string_next:\n");
    output.push_back("    addiu $t9, $t9, 1\n");
    output.push_back("    j skip_ws_string\n");
    output.push_back("    \n");
    output.push_back("parse_string_found:\n");
    output.push_back("    li $s1, 0               # character count\n");
    output.push_back("parse_string_loop:\n");
    output.push_back("    lb $s0, 0($t9)\n");
    output.push_back("    beqz $s0, parse_string_end\n");
    output.push_back("    beq $s0, 32, parse_string_end  # space\n");
    output.push_back("    beq $s0, 9, parse_string_end   # tab\n");
    output.push_back("    beq $s0, 10, parse_string_end  # newline\n");
    output.push_back("    beq $s0, 13, parse_string_end  # carriage return\n");
    output.push_back("    \n");
    output.push_back("    sb $s0, 0($t7)          # store character\n");
    output.push_back("    addiu $t7, $t7, 1       # advance destination\n");
    output.push_back("    addiu $t9, $t9, 1       # advance source\n");
    output.push_back("    addiu $s1, $s1, 1       # increment count\n");
    output.push_back("    j parse_string_loop\n");
    output.push_back("    \n");
    output.push_back("parse_string_end:\n");
    output.push_back("    sb $zero, 0($t7)        # null terminate\n");
    output.push_back("    move $v0, $t9           # return updated pointer\n");
    output.push_back("    \n");
    output.push_back("    lw $s0, 0($sp)\n");
    output.push_back("    lw $s1, 4($sp)\n");
    output.push_back("    addiu $sp, $sp, 8\n");
    output.push_back("    jr $ra\n");

    output.push_back("scanf_complete:\n");
    output.push_back("    # scanf completed successfully\n");

    // Invalidate temporary registers
    invalidateSpecificRegisters({"$v0", "$v1", "$a0", "$a1", "$f0", "$f1", "$f2", "$t7", "$t8", "$t9"});
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
            std::string outputVarName = sanitizeName(originalVarName);
            
            std::string initialValue;
            bool hasInitialValue = globalInitialValues.count(originalVarName);
            if (hasInitialValue) {
                initialValue = globalInitialValues[originalVarName];
            }

            if (varType.second.find("float") != std::string::npos) {
                if (hasInitialValue && initialValue.find(".") != std::string::npos) {
                    data += outputVarName + ": .float " + initialValue + "\n";
                } else {
                    data += outputVarName + ": .float 0.0\n";
                }
            } else if (varType.second.find("char") != std::string::npos) {
                if (hasInitialValue && initialValue.find("'") != std::string::npos) {
                    data += outputVarName + ": .byte " + std::to_string((int)initialValue[1]) + "\n";
                } else {
                    data += outputVarName + ": .byte 0\n";
                }
            } else {
                if (hasInitialValue && std::regex_match(initialValue, std::regex(R"(-?\d+)"))) {
                    data += outputVarName + ": .word " + initialValue + "\n";
                } else {
                    data += outputVarName + ": .word 0\n";
                }
            }
        }
    }
    
    // Parser error strings
    if (hasParserErrors) {
        data += "parser_error_header: .asciiz \"=== PARSER ERRORS DETECTED ===\\n\"\n";
        data += "parser_error_footer: .asciiz \"=== PROGRAM EXECUTION ABORTED ===\\n\"\n";
        data += "newline: .asciiz \"\\n\"\n";
        
        // Create labels for each parser error
        for (size_t i = 0; i < parserErrors.size(); i++) {
            std::string cleanError = parserErrors[i];
            
            // Escape special characters for assembly strings
            std::string escapedError;
            for (char c : cleanError) {
                if (c == '\n') {
                    escapedError += "\\n";
                } else if (c == '\t') {
                    escapedError += "\\t";
                } else if (c == '\r') {
                    escapedError += "\\r";
                } else if (c == '"') {
                    escapedError += "\\\"";
                } else if (c == '\\') {
                    escapedError += "\\\\";
                } else {
                    escapedError += c;
                }
            }
            
            data += "parser_error_" + std::to_string(i) + ": .asciiz \"" + escapedError + "\"\n";
        }
    } else {
        // Default string if no strings defined and no errors
        if (stringConstants.empty()) {
            data += "default_str: .asciiz \"Hello World\\n\"\n";
        }
    }
    
    return data;
}

    std::string generateTextSection() {
    std::string textSection = ".text\n.globl main\n";
    
    if (hasParserErrors) {
        // Generate code to print parser errors at runtime
        textSection += "\nmain:\n";
        textSection += "    # Print parser error header\n";
        textSection += "    li $v0, 4\n";
        textSection += "    la $a0, parser_error_header\n";
        textSection += "    syscall\n\n";
        
        // Print each parser error exactly as they appear
        for (size_t i = 0; i < parserErrors.size(); i++) {
            textSection += "    # Printing parser error " + std::to_string(i + 1) + "\n";
            textSection += "    li $v0, 4\n";
            textSection += "    la $a0, parser_error_" + std::to_string(i) + "\n";
            textSection += "    syscall\n";
            
            // Add newline after each error
            textSection += "    li $v0, 4\n";
            textSection += "    la $a0, newline\n";
            textSection += "    syscall\n\n";
        }
        
        // Print footer and exit
        textSection += "    # Print footer and exit\n";
        textSection += "    li $v0, 4\n";
        textSection += "    la $a0, parser_error_footer\n";
        textSection += "    syscall\n";
        textSection += "    li $v0, 10\n";
        textSection += "    syscall\n";
    } else {
        // Normal generated code
        for (const std::string& line : output) {
            textSection += line;
        }
    }
    
    return textSection;
}

    std::string trim(const std::string& str) {
        size_t first = str.find_first_not_of(" \t");
        if (std::string::npos == first) {
            return str;
        }
        size_t last = str.find_last_not_of(" \t\r\n");
        return str.substr(first, (last - first + 1));
    }

public:
    void convert(const std::string& inputFile, const std::string& outputFile) {
    std::ifstream inFile(inputFile);
    std::ofstream outFile(outputFile);
    
    if (!inFile.is_open()) {
        std::cerr << "Error: Cannot open input file " << inputFile << std::endl;
        return;
    }
    
    if (!outFile.is_open()) {
        std::cerr << "Error: Cannot create output file " << outputFile << std::endl;
        return;
    }
    
    // First pass: Identify global variables and function names
    std::string line;
    while (std::getline(inFile, line)) {
        line = trim(line);
        if (line.empty()) continue;
        
        if (line.find("Variable declaration:") != std::string::npos) {
            parseVariableDeclaration(line);
        }
        if (line.find("BeginFunc") != std::string::npos) {
            currentFunc = "in_function";
        }
        if (line.find("EndFunc") != std::string::npos) {
            currentFunc = "";
        }
    }
    
    // Reset for second pass
    inFile.clear();
    inFile.seekg(0, std::ios::beg);
    currentFunc = "";
    
    // Second pass: Generate assembly and collect parser errors
    while (std::getline(inFile, line)) {
        line = trim(line);
        if (line.empty()) continue;
        
        std::string asmLine = convertInstruction(line);
    }
    
    // Write the complete SPIM program
    outFile << "# SPIM MIPS assembly generated from TAC\n";
    outFile << "# Input file: " << inputFile << "\n";
    
    if (hasParserErrors) {
        outFile << "\n";
        std::cerr << "Parser errors detected! Error messages will be printed when running the assembly." << std::endl;
        std::cerr << "Number of parser errors: " << parserErrors.size() << std::endl;
    }
    
    outFile << generateDataSection() << "\n";
    outFile << generateTextSection() << "\n";
    
    if (!hasParserErrors) {
        std::cout << "Conversion completed! Assembly code written to " << outputFile << std::endl;
        std::cout << "Run with: spim -f " << outputFile << std::endl;
    } else {
        std::cout << "Assembly with error reporting generated! Run with: spim -f " << outputFile << std::endl;
        std::cout << "The program will display " << parserErrors.size() << " parser errors when executed." << std::endl;
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
    
    // Determine output file name
    std::string outputFile = inputFile;
    size_t dotPos = outputFile.rfind('.');
    if (dotPos != std::string::npos) {
        outputFile = outputFile.substr(0, dotPos) + ".asm";
    } else {
        outputFile += ".asm";
    }
    
    TACToSPIMConverter converter;
    converter.convert(inputFile, outputFile);
    
    return 0;
}
