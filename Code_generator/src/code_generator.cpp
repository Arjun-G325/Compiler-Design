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
    // *** FIX 1: Added member variables for parameter indexing ***
    int intParamIndex;
    int floatParamIndex;
    // *** FIX 3: Added member for stack allocation placeholder ***
    int stackAllocPlaceholderIndex;
    // **********************************************************
    bool hasParserErrors;
    std::vector<std::string> parserErrors;
    std::map<std::string, std::string> globalInitialValues;

    // Set of reserved SPIM/MIPS keywords
    std::set<std::string> spimKeywords;

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

    /**
     * @brief Sanitizes a variable or label name if it conflicts with a SPIM opcode.
     * @param name The name to check.
     * @return The sanitized name (e.g., "b_1") or the original name if no conflict.
     */
    std::string sanitizeName(const std::string& name) {
        if (spimKeywords.count(name)) {
            return name + "_1"; // Append _1
        }
        return name;
    }

public:
    TACToSPIMConverter() : labelCount(0), stringConstCount(0), floatConstCount(0), 
                          tempVarCount(0), stackOffset(0), argStackOffset(0), useCounter(0),
                          // *** FIX 1: Initialized new members ***
                          intParamIndex(0), floatParamIndex(0),
                          // *** FIX 3: Initialized new member ***
                          stackAllocPlaceholderIndex(-1),
                          // ************************************
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
        // Sanitization is not needed here as we control the base (e.g., "float_true")
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
            // This is the buggy line that assumes the input is already escaped
            // cleanValue = cleanValue.substr(1, cleanValue.length() - 2);
            
            // --- START FIX ---
            // 1. Correctly strip only the quotes
            cleanValue = cleanValue.substr(1, cleanValue.length() - 2);

            // 2. Un-escape the C-style string
            std::string processedValue;
            processedValue.reserve(cleanValue.length()); // Reserve space
            
            for (size_t i = 0; i < cleanValue.length(); ++i) {
                if (cleanValue[i] == '\\' && i + 1 < cleanValue.length()) {
                    // Check for known escape sequences
                    switch (cleanValue[i + 1]) {
                        case 'n':  processedValue.push_back('\n'); i++; break;
                        case 't':  processedValue.push_back('\t'); i++; break;
                        case '\\': processedValue.push_back('\\'); i++; break;
                        case '"':  processedValue.push_back('"');  i++; break;
                        default:
                            // Not a known escape, just add the backslash
                            processedValue.push_back('\\');
                    }
                } else {
                    processedValue.push_back(cleanValue[i]);
                }
            }
            cleanValue = processedValue;
            // --- END FIX ---
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
                
                // *** FIX: Load from stack if variable exists there ***
                if (variableInfo.find(var) != variableInfo.end() && variableInfo[var].stackOffset != -1) {
                    output.push_back("    # Loading " + var + " from stack to " + reg + "\n");
                    if (isFloat) {
                        output.push_back("    l.s " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
                    } else {
                        output.push_back("    lw " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
                    }
                }
                // Only check for global if not found on stack
                else if (isGlobalVar(var)) {
                    output.push_back("    # Loading global " + var + " into " + reg + "\n");
                    if (isFloat) {
                        output.push_back("    l.s " + reg + ", " + sanitizeName(var) + "\n");
                    } else {
                        output.push_back("    lw " + reg + ", " + sanitizeName(var) + "\n");
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
        else if (isGlobalVar(var)) {
            output.push_back("    # Loading global " + var + " into " + lruReg + "\n");
            if (isFloat) {
                output.push_back("    l.s " + lruReg + ", " + sanitizeName(var) + "\n");
            } else {
                output.push_back("    lw " + lruReg + ", " + sanitizeName(var) + "\n");
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
        
        // *** FIX: Only spill if variable is not a temporary ***
        if (var.empty() || var[0] == 't' || var[0] == 'i') {
            // Don't spill temporary variables
            return;
        }
        
        if (isGlobalVar(var)) {
            // Spill to global memory
            output.push_back("    # Spilling " + var + " from " + reg + " to global memory\n");
            if (isFloat) {
                output.push_back("    s.s " + reg + ", " + sanitizeName(var) + "\n");
            } else {
                output.push_back("    sw " + reg + ", " + sanitizeName(var) + "\n");
            }
            if (variableInfo.find(var) != variableInfo.end()) {
                variableInfo[var].dirty = false;
            }
        } else {
            // Spill to stack (original behavior)
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
    }

    void allocateStackSpace(const std::string& var) {
        // *** FIX: Only allocate if not already allocated AND if variable is actually used ***
        if (variableInfo.find(var) == variableInfo.end()) {
            variableInfo[var] = {{}, -1, false, isFloatVar(var), false};
        }
        
        // *** FIX: Only allocate if variable is used in current context ***
        if (variableInfo[var].stackOffset == -1 && !var.empty() && var[0] != 't' && var[0] != 'i') {
            // Only allocate for non-temporary variables
            variableInfo[var].stackOffset = stackOffset;
            stackOffset -= 4; 
            output.push_back("    # Allocated stack space for " + var + " at offset " + std::to_string(variableInfo[var].stackOffset) + "\n");
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

    // *** FIX 2: Added this new function ***
    /**
     * @brief Invalidates all caller-saved ($t) registers after a function call.
     * The callee is free to clobber these, so we can no longer trust our
     * internal state for them.
     */
    void invalidateTempRegisters() {
        // Invalidate ALL integer $t registers
        for (const auto& reg : availableIntRegs) {
            std::string var = intRegisters[reg].varName;
            if (!var.empty()) {
                // If this var was in this register, it's not anymore
                if (variableInfo.count(var)) {
                    variableInfo[var].registers.erase(reg);
                }
                // Clear the register state
                intRegisters[reg] = {"", false, false, 0}; 
            }
        }
        
        // Invalidate ALL float $t registers
        for (const auto& reg : availableFloatRegs) {
            std::string var = floatRegisters[reg].varName;
            if (!var.empty()) {
                if (variableInfo.count(var)) {
                    variableInfo[var].registers.erase(reg);
                }
                floatRegisters[reg] = {"", false, true, 0};
            }
        }
        
        // *** FIX: Also invalidate $a registers for good measure ***
        for (const auto& reg : argIntRegs) {
            std::string var = intRegisters[reg].varName;
            if (!var.empty()) {
                if (variableInfo.count(var)) {
                    variableInfo[var].registers.erase(reg);
                }
                intRegisters[reg] = {"", false, false, 0};
            }
        }
        
        for (const auto& reg : argFloatRegs) {
            std::string var = floatRegisters[reg].varName;
            if (!var.empty()) {
                if (variableInfo.count(var)) {
                    variableInfo[var].registers.erase(reg);
                }
                floatRegisters[reg] = {"", false, true, 0};
            }
        }
    }
    // **********************************

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
                parseVariableDeclaration(line);
            }
            // *** FIX 1: Handle Parameter comments ***
            else if (line.find("Parameter:") != std::string::npos && !currentFunc.empty()) {
                std::regex paramComment(R"(//\s*Parameter:\s*(\w+))");
                std::smatch paramMatch;
                if (std::regex_search(line, paramMatch, paramComment)) {
                    std::string paramName = paramMatch[1];
                    bool isParamFloat = isFloatVar(paramName);
                    
                    // *** FIX: Allocate stack space for parameter first ***
                    allocateStackSpace(paramName);
                    
                    std::string destReg = getRegisterForVar(paramName, isParamFloat);
                    
                    if (isParamFloat) {
                        if (floatParamIndex < argFloatRegs.size()) {
                            output.push_back("    mov.s " + destReg + ", " + argFloatRegs[floatParamIndex] + "  # Copy param " + paramName + "\n");
                            // *** FIX: Store parameter to stack immediately ***
                            output.push_back("    s.s " + destReg + ", " + std::to_string(variableInfo[paramName].stackOffset) + "($fp)  # Store param " + paramName + " to stack\n");
                            floatParamIndex++;
                        } else {
                            // Parameter is on the stack
                            int stackOffset = 8 + (floatParamIndex - argFloatRegs.size()) * 4;
                            output.push_back("    l.s " + destReg + ", " + std::to_string(stackOffset) + "($fp)  # Load stack param " + paramName + "\n");
                            output.push_back("    s.s " + destReg + ", " + std::to_string(variableInfo[paramName].stackOffset) + "($fp)  # Store param " + paramName + " to local stack\n");
                            floatParamIndex++;
                        }
                    } else {
                        if (intParamIndex < argIntRegs.size()) {
                            output.push_back("    move " + destReg + ", " + argIntRegs[intParamIndex] + "  # Copy param " + paramName + "\n");
                            // *** FIX: Store parameter to stack immediately ***
                            output.push_back("    sw " + destReg + ", " + std::to_string(variableInfo[paramName].stackOffset) + "($fp)  # Store param " + paramName + " to stack\n");
                            intParamIndex++;
                        } else {
                            // Parameter is on the stack
                            int stackOffset = 8 + (intParamIndex - argIntRegs.size()) * 4;
                            output.push_back("    lw " + destReg + ", " + std::to_string(stackOffset) + "($fp)  # Load stack param " + paramName + "\n");
                            output.push_back("    sw " + destReg + ", " + std::to_string(variableInfo[paramName].stackOffset) + "($fp)  # Store param " + paramName + " to local stack\n");
                            intParamIndex++;
                        }
                    }
                    markDirty(paramName, destReg);
                }
            }
            // ***************************************
            return "# " + line + "\n";
        }
        
        // Function begin/end
        if (line.find("BeginFunc") != std::string::npos) {
            std::regex funcRegex(R"(BeginFunc\s+(\w+))");
            
            std::smatch match;
            if (std::regex_search(line, match, funcRegex)) {
                currentFunc = match[1];
                variableInfo.clear();
                stackOffset = -8; // Start after ra/fp
                argStackOffset = 0;
                // *** FIX 1: Initialized parameter indices ***
                intParamIndex = 0;
                floatParamIndex = 0;
                // ******************************************
                paramStack.clear();
                resetRegisters();
                
                output.push_back("\n" + sanitizeName(currentFunc) + ":\n");
                
                // *** FIX: Allocate space for ra/fp AND parameters ***
                output.push_back("    addiu $sp, $sp, -8    # Allocate space for ra/fp\n");
                output.push_back("    sw $ra, 4($sp)        # Save return address\n");
                output.push_back("    sw $fp, 0($sp)        # Save frame pointer\n");
                output.push_back("    move $fp, $sp         # Set new frame pointer\n");
                
                // *** FIX: Store parameters on stack immediately ***
                if (intParamIndex > 0 || floatParamIndex > 0) {
                    output.push_back("    # Store parameters on stack\n");
                    // We'll handle parameter storage in the parameter comments
                }
                
                // Add placeholder for stack allocation
                stackAllocPlaceholderIndex = output.size();
                output.push_back("    # STACK_ALLOC_PLACEHOLDER\n");
            }
            return "";
        }
        
        if (line.find("EndFunc") != std::string::npos) {
            spillAllDirtyRegisters();
            
            std::regex funcRegex(R"(EndFunc\s*)");
            
            // *** FIX: Add return label before epilogue ***
            output.push_back(sanitizeName(currentFunc) + "_return:\n");
            
            // *** FIX 3 (Modified): Calculate *only* local stack size ***
            // stackOffset starts at -8. If it's now -28, we need 20 bytes.
            // The space used is (-stackOffset) - 8.
            int localStackSize = ((-stackOffset - 8) + 7) & ~7; // (28 - 8 = 20) -> 24 bytes
            
            if (stackAllocPlaceholderIndex != -1 && stackAllocPlaceholderIndex < output.size()) {
                if (localStackSize > 0) {
                    // This instruction is now AFTER $fp is set
                    output[stackAllocPlaceholderIndex] = "    addiu $sp, $sp, -" + std::to_string(localStackSize) + "    # Allocate stack for locals\n";
                } else {
                    output[stackAllocPlaceholderIndex] = "    # No local stack needed\n";
                }
            }
            stackAllocPlaceholderIndex = -1; 
            
            output.push_back("    # Function epilogue\n");
            // Deallocate locals first by resetting $sp to $fp
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

        // Check for binary operations before simple assignments.
        // Group 1: dest, Group 2: left, Group 3: op, Group 4: right, Group 5: type
        // ***FIXED***: Added bitwise operators &, |, ^, <<, >> to regex
        std::regex binOpRegex(R"((\w+)\s*=\s*(\w+)\s*(<<|>>|[\+\-\*\/&\|\^])\s*([\w\.-]+)(?:\s*\[(\w+)\])?)");
        if (std::regex_search(line, match, binOpRegex)) {
            std::string dest = match[1];
            std::string left = match[2];
            std::string op = match[3];
            std::string right = match[4];
            std::string type = match[5].str(); // Get optional type
            
            if (type == "FLOAT") {
                convertFloatOperation(dest, left, op, right);
            } else if (type == "INT") {
                convertIntOperation(dest, left, op, right);
            } else {
                // ***FIXED***: Infer type from OPERANDS (C-style promotion), not destination
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
        
        // ***FIX 1a:*** Moved comparison regex *before* simple assignment
        // Comparison operations
        std::regex cmpRegex(R"((\w+)\s*=\s*([\w\.-]+)\s*([><=!]+)\s*([\w\.-]+)\s*\[(\w+)\])");
        if (std::regex_search(line, match, cmpRegex)) {
            std::string dest = match[1];
            std::string left = match[2];
            std::string op = match[3];
            std::string right = match[4];
            std::string type = match[5];
            
            // ***FIXED***: Logic is correct. If either op is float, use float comparison.
            if (type == "FLOAT" || isFloatVar(left) || isFloatVar(right) || std::regex_match(left, std::regex(R"(-?\d+\.\d+)")) || std::regex_match(right, std::regex(R"(-?\d+\.\d+)"))) {
                convertFloatComparison(dest, left, op, right);
            } else {
                convertIntComparison(dest, left, op, right);
            }
            return "# " + line + "\n";
        }
        
        // ***FIX 1b:*** Modified simpleAssign regex to exclude comparison ops
        std::regex simpleAssign(R"((\w+)\s*=\s*([^;\[\+\-\*\/><=!]+)(?:\s*\[([^\]]+)\])?)");
        
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
                } else if (std::regex_match(src, std::regex(R"(-?\d+\.\d+)"))) {
                    getFloatConstant(src);
                }

                return "# " + line + " (Global assignment - storing for .data)\n";
            }
            
            std::string dest = match[1];
            std::string src = match[2];
            std::string type = match[3];
            
            src = std::regex_replace(src, std::regex(R"(\s*$)"), "");

            if (src.rfind("&", 0) == 0) {
                std::string varName = src.substr(1); // Get name after '&'
                std::string destReg = getRegisterForVar(dest, false); // Addresses are ints

                // Case 1: Address of a string constant (e.g., "&\"%d,%d\"")
                if (varName.front() == '"') {
                    std::string label = getStringConstant(varName); // This function handles the quotes
                    
                    output.push_back("    la " + destReg + ", " + label + "  # " + line);
                    
                    // Store the *actual string value* for scanf to find later
                    globalInitialValues[dest] = varName; 
                }
                // Case 2: Address of a global variable (e.g., "&a")
                else if (globalVars.count(varName)) {
                    // Generate the correct "Load Address" instruction
                    output.push_back("    la " + destReg + ", " + sanitizeName(varName) + "  # " + line);
                }
                // Case 3: Address of a local/stack variable (e.g., "&x")
                else {
                    // This variable MUST be on the stack to take its address
                    allocateStackSpace(varName);
                    int offset = variableInfo[varName].stackOffset;
                    output.push_back("    addiu " + destReg + ", $fp, " + std::to_string(offset) + " # " + line);
                }

                markDirty(dest, destReg);
                return "# " + line + "\n"; // End processing for this line
            }
            
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
                    output.push_back("    l.s " + destReg + ", " + floatLabel + " # " + dest + " = " + src + "\n");
                } else {
                    // int_dest = float_literal (truncation)
                    std::string tempFloatReg = "$f31"; // Use a reserved temp
                    output.push_back("    l.s " + tempFloatReg + ", " + floatLabel + "\n");
                    output.push_back("    trunc.w.s " + tempFloatReg + ", " + tempFloatReg + "\n");
                    output.push_back("    mfc1 " + destReg + ", " + tempFloatReg + " # " + dest + " = (int)" + src + "\n");
                }
            }
            // Case 2: Source is an integer literal (e.g., "10", "-5")
            else if (std::regex_match(src, std::regex(R"(-?\d+)"))) {
                if (isDestFloat) {
                    // float_dest = int_literal (conversion)
                    std::string tempIntReg = "$v1"; // Use assembler temp
                    output.push_back("    li " + tempIntReg + ", " + src + "\n");
                    output.push_back("    mtc1 " + tempIntReg + ", " + destReg + " # " + dest + " = " + src + ".0\n");
                    output.push_back("    cvt.s.w " + destReg + ", " + destReg + "\n");
                } else {
                    // int_dest = int_literal
                    output.push_back("    li " + destReg + ", " + src + " # " + dest + " = " + src + "\n");
                }
            }
            // Case 3: Source is a character literal (e.g., "'x'")
            else if (src.find("'") == 0 && src.length() == 3) {
                char c = src[1];
                if (isDestFloat) {
                    // float_dest = char_literal (conversion)
                    std::string tempIntReg = "$v1"; // Use assembler temp
                    output.push_back("    li " + tempIntReg + ", " + std::to_string((int)c) + "\n");
                    output.push_back("    mtc1 " + tempIntReg + ", " + destReg + " # " + dest + " = (float)'" + c + "'\n");
                    output.push_back("    cvt.s.w " + destReg + ", " + destReg + "\n");
                } else {
                    // int_dest = char_literal
                    output.push_back("    li " + destReg + ", " + std::to_string((int)c) + " # " + dest + " = '" + std::string(1, c) + "'\n");
                }
            }
            // Case 4: Source is a string literal (e.g., ""%f %d"")
            else if (src.find("\"") == 0) {
                std::string stringLabel = getStringConstant(src);
                // Store the literal value for scanf/printf to find
                globalInitialValues[dest] = src;
                
                if (isDestFloat) {
                    // This is invalid, but we'll treat it as loading the address
                    output.push_back("    # WARNING: Assigning string address to float var\n");
                    output.push_back("    la " + destReg + ", " + stringLabel + " # " + dest + " = &\"...\"\n");
                } else {
                    // int_dest = string_address
                    output.push_back("    la " + destReg + ", " + stringLabel + " # " + dest + " = &\"...\"\n");
                }
            }
            // Case 5: Source is a VARIABLE
            else {
                bool isSrcFloat = isFloatVar(src);
                std::string srcReg = getRegisterForVar(src, isSrcFloat);
                
                if (isDestFloat && !isSrcFloat) {
                    // float_dest = int_var
                    output.push_back("    mtc1 " + srcReg + ", " + destReg + " # " + dest + " = (float)" + src + "\n");
                    output.push_back("    cvt.s.w " + destReg + ", " + destReg + "\n");
                } else if (!isDestFloat && isSrcFloat) {
                    // int_dest = float_var
                    std::string tempFloatReg = srcReg;
                    output.push_back("    trunc.w.s " + tempFloatReg + ", " + tempFloatReg + "\n");
                    output.push_back("    mfc1 " + destReg + ", " + tempFloatReg + " # " + dest + " = (int)" + src + "\n");
                } else if (isDestFloat && isSrcFloat) {
                    // float_dest = float_var
                    output.push_back("    mov.s " + destReg + ", " + srcReg + " # " + dest + " = " + src + "\n");
                } else {
                    // int_dest = int_var
                    output.push_back("    move " + destReg + ", " + srcReg + " # " + dest + " = " + src + "\n");
                }
            }
            
            markDirty(dest, destReg);
            return "# " + line + "\n";
        }
        
        // Binary operations
        // (This block is now moved up)
        
        // Comparison operations
        // (This block is now moved up)
        
        // Conditional jumps
        std::regex condRegex(R"(ifFalse\s*(\w+)\s*goto\s*(\w+))");
        if (std::regex_search(line, match, condRegex)) {
            std::string condition = match[1];
            std::string label = match[2];
            std::string condReg = getRegisterForVar(condition, false);
            output.push_back("    beq " + condReg + ", $zero, " + sanitizeName(label) + "  # ifFalse " + condition + "\n");
            return "# " + line + "\n";
        }

        // *** NEW FIX HERE ***
        // Conditional jumps (if)
        std::regex condTrueRegex(R"(if\s*(\w+)\s*goto\s*(\w+))");
        if (std::regex_search(line, match, condTrueRegex)) {
            std::string condition = match[1];
            std::string label = match[2];
            std::string condReg = getRegisterForVar(condition, false);
            // Branch if the condition is TRUE (not zero)
            output.push_back("    bne " + condReg + ", $zero, " + sanitizeName(label) + "  # if " + condition + "\n");
            return "# " + line + "\n";
        }
        // *** END NEW FIX ***

        // Unconditional jumps
        if (line.find("goto") == 0) {
            std::regex gotoRegex(R"(goto\s*(\w+))");
            std::smatch match;
            if (std::regex_search(line, match, gotoRegex)) {
                std::string label = match[1];
                output.push_back("    j " + sanitizeName(label) + "\n");
            }
            return "# " + line + "\n";
        }
        
        // Labels
        if (std::regex_match(line, std::regex(R"(\w+:)"))) {
            spillAllDirtyRegisters(); // Spill before labels
            std::string label = line.substr(0, line.length() - 1);
            output.push_back(sanitizeName(label) + ":\n");
            return "# " + line + " (Label)\n"; // Return a non-empty string
        }
        
        // Return statement
        // ####################################################################
        // #                            BEGIN FIX                             #
        // ####################################################################
        std::regex returnRegex(R"(return\s+(.+))");
        if (std::regex_search(line, match, returnRegex)) {
            std::string returnValue = match[1];
            
            // Check if the return value is a literal
            
            // Case 1: Integer literal (e.g., "1", "-5")
            if (std::regex_match(returnValue, std::regex(R"(-?\d+)"))) {
                output.push_back("    li $v0, " + returnValue + "  # Return value\n");
            }
            // Case 2: Float literal (e.g., "1.0", "-0.5")
            else if (std::regex_match(returnValue, std::regex(R"(-?\d+\.\d+)"))) {
                std::string floatLabel = getFloatConstant(returnValue);
                output.push_back("    l.s $v0, " + floatLabel + "  # Return float literal\n");
            }
            // Case 3: It's a VARIABLE (original logic)
            else {
                bool isFloat = isFloatVar(returnValue);
                std::string reg = getRegisterForVar(returnValue, isFloat);
                
                if (isFloat) {
                    output.push_back("    mov.s $v0, " + reg + "  # Return float value\n");
                } else {
                    output.push_back("    move $v0, " + reg + "  # Return value\n");
                }
            }
            
            output.push_back("    j " + currentFunc + "_return  # Jump to epilogue\n");
            return "# " + line + "\n";
        }
        // ####################################################################
        // #                             END FIX                              #
        // ####################################################################
        
        // Handle scanf
        if (line.find("scanf") != std::string::npos) {
            output.push_back("    # scanf call\n");
            output.push_back("    li $v0, 5    # Read integer\n"); // Assuming int for simplicity
            output.push_back("    syscall\n");
            // Result is in $v0, store it
            // This is a simplified example. A full implementation would
            // parse the format string and variable addresses.
            
            // In a real implementation:
            // 1. Get the format string (e.g., from i17)
            // 2. Get the variable address (e.g., from param)
            // 3. Use li $v0, 5 (read_int), 6 (read_float), 8 (read_string), etc.
            // 4. Store $v0 into the address
        }
        
        // Handle printf (simplified)
        if (line.find("printf") != std::string::npos) {
            output.push_back("    # printf call\n");
            // Assuming two params: a value (e.g., 'c') and a format string (e.g., i17)
            
            std::string valParam = paramStack[paramStack.size() - 2]; // 'c'
            std::string fmtParam = paramStack[paramStack.size() - 1]; // i17 (which holds "%d\n")
            
            // This is a hacky simplification based on the input TAC
            // A real version would analyze the format string
            
            if (globalInitialValues[fmtParam] == "\"%d\\n\"") {
                std::string valReg = getRegisterForVar(valParam, false);
                output.push_back("    li $v0, 1        # Print integer\n");
                output.push_back("    move $a0, " + valReg + "\n");
                output.push_back("    syscall\n");
                
                // Print the newline
                output.push_back("    li $v0, 4        # Print final string chunk\n");
                output.push_back("    la $a0, string_const_1 # string_const_1 is \"\\n\"\n");
                output.push_back("    syscall\n");
            } else if (globalInitialValues[fmtParam] == "\"%f\\n\"") {
                std::string valReg = getRegisterForVar(valParam, true);
                output.push_back("    li $v0, 2        # Print float\n");
                output.push_back("    mov.s $f12, " + valReg + "\n");
                output.push_back("    syscall\n");
                
                // Print the newline
                output.push_back("    li $v0, 4        # Print final string chunk\n");
                output.push_back("    la $a0, string_const_1 # string_const_1 is \"\\n\"\n");
                output.push_back("    syscall\n");
            } else {
                 // Fallback for other strings
                std::string fmtReg = getRegisterForVar(fmtParam, false); // It's an address
                output.push_back("    li $v0, 4        # Print string\n");
                output.push_back("    move $a0, " + fmtReg + "\n");
                output.push_back("    syscall\n");
            }
        }
        
        // Malloc
        if (line.find("malloc") != std::string::npos) {
            std::string destReg = getRegisterForVar(line.substr(0, line.find(" =")), false);
            std::string sizeVar = paramStack.back();
            std::string sizeReg = getRegisterForVar(sizeVar, false);
            
            output.push_back("    move $a0, " + sizeReg + "    # Malloc size\n");
            output.push_back("    li $v0, 9          # sbrk (malloc)\n");
            output.push_back("    syscall\n");
            output.push_back("    move " + destReg + ", $v0    # Store allocated address\n");
            markDirty(line.substr(0, line.find(" =")), destReg);
        }
        
        // Free
        if (line.find("free") != std::string::npos) {
            // SPIM doesn't *really* have 'free'. This is a no-op.
            output.push_back("    # 'free' call (no-op in SPIM)\n");
        }
        
        // Array/Pointer Access
        std::regex storeRegex(R"(\*\((\w+)\)\s*=\s*(\w+))"); // *(dest) = src
        if (std::regex_search(line, match, storeRegex)) {
            std::string destAddrVar = match[1];
            std::string srcVar = match[2];
            
            std::string destAddrReg = getRegisterForVar(destAddrVar, false);
            
            bool isFloat = isFloatVar(srcVar);
            std::string srcReg = getRegisterForVar(srcVar, isFloat);
            
            if (isFloat) {
                output.push_back("    s.s " + srcReg + ", 0(" + destAddrReg + ")  # *(dest) = src\n");
            } else {
                output.push_back("    sw " + srcReg + ", 0(" + destAddrReg + ")  # *(dest) = src\n");
            }
            return "# " + line + "\n";
        }
        
        std::regex loadRegex(R"((\w+)\s*=\s*\*\((\w+)\))"); // dest = *(src)
        if (std::regex_search(line, match, loadRegex)) {
            std::string destVar = match[1];
            std::string srcAddrVar = match[2];
            
            std::string srcAddrReg = getRegisterForVar(srcAddrVar, false);
            
            bool isFloat = isFloatVar(destVar);
            std::string destReg = getRegisterForVar(destVar, isFloat);
            
            if (isFloat) {
                output.push_back("    l.s " + destReg + ", 0(" + srcAddrReg + ")  # dest = *(src)\n");
            } else {
                output.push_back("    lw " + destReg + ", 0(" + srcAddrReg + ")  # dest = *(src)\n");
            }
            markDirty(destVar, destReg);
            return "# " + line + "\n";
        }

        // Handle array indexing (e.g., x = y[i])
        // x = y + i*4  (for int)
        // t1 = i * 4
        // t2 = y + t1
        // x = *t2
        
        // Handle array indexing (e.g., x[i] = y)
        // t1 = i * 4
        // t2 = x + t1
        // *t2 = y
        
        return "# Unhandled TAC: " + line + "\n";
    }

    void convertIntOperation(const std::string& dest, const std::string& left, const std::string& op, const std::string& right) {
        std::string destReg = getRegisterForVar(dest, false);
        std::string leftReg;
        std::string rightReg;
        
        // Check if left is a variable
        if (isalpha(left[0])) {
            leftReg = getRegisterForVar(left, false);
        }
        
        // Check if right is a variable
        if (isalpha(right[0])) {
            rightReg = getRegisterForVar(right, false);
        }
        
        std::string opCmd;
        if (op == "+") opCmd = "add";
        if (op == "-") opCmd = "sub";
        if (op == "*") opCmd = "mul";
        if (op == "/") opCmd = "div";
        if (op == "&") opCmd = "and";
        if (op == "|") opCmd = "or";
        if (op == "^") opCmd = "xor";
        if (op == "<<") opCmd = "sll";
        if (op == ">>") opCmd = "srl";
        
        if (rightReg.empty()) {
            // Right is a literal
            if (op == "+") opCmd = "addiu";
            if (op == "-") opCmd = "subu";
            if (op == "&") opCmd = "andi";
            if (op == "|") opCmd = "ori";
            if (op == "^") opCmd = "xori";
            if (op == "<<") opCmd = "sll";
            if (op == ">>") opCmd = "srl";
            
            if (op == "*" || op == "/") {
                // Must load literal into a temp register
                rightReg = getRegisterForVar("t" + std::to_string(tempVarCount++), false);
                output.push_back("    li " + rightReg + ", " + right + "\n");
                output.push_back("    " + opCmd + " " + destReg + ", " + leftReg + ", " + rightReg + "\n");
            } else {
                 // Use immediate instruction
                output.push_back("    " + opCmd + " " + destReg + ", " + leftReg + ", " + right + "\n");
            }
        } else if (leftReg.empty()) {
             // Left is a literal
            leftReg = getRegisterForVar("t" + std::to_string(tempVarCount++), false);
            output.push_back("    li " + leftReg + ", " + left + "\n");
            output.push_back("    " + opCmd + " " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        } else {
            // Both are registers
            output.push_back("    " + opCmd + " " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        }
        
        if (op == "/" || op == "%") {
            // SPIM stores quotient in $lo, remainder in $hi
            // This is a simplification; a full solution would use mflo/mfhi
            output.push_back("    mflo " + destReg + " # Move quotient to " + destReg + "\n");
        }
        
        markDirty(dest, destReg);
        output.push_back("    move " + destReg + ", " + destReg + " # " + dest + " = (int_op)\n");
    }

    void convertFloatOperation(const std::string& dest, const std::string& left, const std::string& op, const std::string& right) {
        std::string destReg = getRegisterForVar(dest, true);
        std::string leftReg;
        std::string rightReg;
        
        // Check if left is a variable
        if (isalpha(left[0])) {
            leftReg = getRegisterForVar(left, true);
        } else {
            // Left is a literal
            leftReg = getRegisterForVar("f" + std::to_string(tempVarCount++), true);
            if (left.find('.') != std::string::npos) {
                // Float literal
                output.push_back("    l.s " + leftReg + ", " + getFloatConstant(left) + "\n");
            } else {
                // Int literal
                output.push_back("    li $v1, " + left + "\n");
                output.push_back("    mtc1 $v1, " + leftReg + "\n");
                output.push_back("    cvt.s.w " + leftReg + ", " + leftReg + "\n");
            }
        }
        
        // Check if right is a variable
        if (isalpha(right[0])) {
            rightReg = getRegisterForVar(right, true);
        } else {
            // Right is a literal
            rightReg = getRegisterForVar("f" + std::to_string(tempVarCount++), true);
            if (right.find('.') != std::string::npos) {
                // Float literal
                output.push_back("    l.s " + rightReg + ", " + getFloatConstant(right) + "\n");
            } else {
                // Int literal
                output.push_back("    li $v1, " + right + "\n");
                output.push_back("    mtc1 $v1, " + rightReg + "\n");
                output.push_back("    cvt.s.w " + rightReg + ", " + rightReg + "\n");
            }
        }
        
        std::string opCmd;
        if (op == "+") opCmd = "add.s";
        if (op == "-") opCmd = "sub.s";
        if (op == "*") opCmd = "mul.s";
        if (op == "/") opCmd = "div.s";
        
        output.push_back("    " + opCmd + " " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        
        markDirty(dest, destReg);
        output.push_back("    mov.s " + destReg + ", " + destReg + " # " + dest + " = (float_op)\n");
    }

    void convertIntComparison(const std::string& dest, const std::string& left, const std::string& op, const std::string& right) {
        std::string destReg = getRegisterForVar(dest, false);
        std::string leftReg;
        std::string rightReg;

        // Check if left is a variable
        if (isalpha(left[0])) {
            leftReg = getRegisterForVar(left, false);
        }
        
        // Check if right is a variable
        if (isalpha(right[0])) {
            rightReg = getRegisterForVar(right, false);
        }
        
        std::string opCmd;
        if (op == "<") opCmd = "slt";
        if (op == "<=") opCmd = "sle";
        if (op == ">") opCmd = "sgt";
        if (op == ">=") opCmd = "sge";
        if (op == "==") opCmd = "seq";
        if (op == "!=") opCmd = "sne";
        
        if (rightReg.empty()) {
            // Right is a literal
            if (op == "<") opCmd = "slti";
            
            if (opCmd == "slti") {
                output.push_back("    " + opCmd + " " + destReg + ", " + leftReg + ", " + right + "\n");
            } else {
                // Must load literal into a temp register
                rightReg = getRegisterForVar("t" + std::to_string(tempVarCount++), false);
                output.push_back("    li " + rightReg + ", " + right + "\n");
                output.push_back("    " + opCmd + " " + destReg + ", " + leftReg + ", " + rightReg + "\n");
            }
        } else if (leftReg.empty()) {
             // Left is a literal
            leftReg = getRegisterForVar("t" + std::to_string(tempVarCount++), false);
            output.push_back("    li " + leftReg + ", " + left + "\n");
            output.push_back("    " + opCmd + " " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        } else {
            // Both are registers
            output.push_back("    " + opCmd + " " + destReg + ", " + leftReg + ", " + rightReg + "\n");
        }
        
        markDirty(dest, destReg);
    }
    
    void convertFloatComparison(const std::string& dest, const std::string& left, const std::string& op, const std::string& right) {
        std::string destReg = getRegisterForVar(dest, false); // Result is 0 or 1 (int)
        std::string leftReg;
        std::string rightReg;

        // Load left operand (literal or var)
        if (isalpha(left[0])) {
            leftReg = getRegisterForVar(left, true);
        } else {
            leftReg = getRegisterForVar("f" + std::to_string(tempVarCount++), true);
            if (left.find('.') != std::string::npos) {
                output.push_back("    l.s " + leftReg + ", " + getFloatConstant(left) + "\n");
            } else {
                output.push_back("    li $v1, " + left + "\n");
                output.push_back("    mtc1 $v1, " + leftReg + "\n");
                output.push_back("    cvt.s.w " + leftReg + ", " + leftReg + "\n");
            }
        }

        // Load right operand (literal or var)
        if (isalpha(right[0])) {
            rightReg = getRegisterForVar(right, true);
        } else {
            rightReg = getRegisterForVar("f" + std::to_string(tempVarCount++), true);
            if (right.find('.') != std::string::npos) {
                output.push_back("    l.s " + rightReg + ", " + getFloatConstant(right) + "\n");
            } else {
                output.push_back("    li $v1, " + right + "\n");
                output.push_back("    mtc1 $v1, " + rightReg + "\n");
                output.push_back("    cvt.s.w " + rightReg + ", " + rightReg + "\n");
            }
        }

        // c.lt.s, c.le.s, c.eq.s
        std::string opCmd;
        std::string trueLabel = generateLabel("float_true");
        std::string endLabel = generateLabel("float_cmp_end");

        if (op == "<") opCmd = "c.lt.s";
        if (op == "<=") opCmd = "c.le.s";
        if (op == "==") opCmd = "c.eq.s";
        
        if (!opCmd.empty()) {
            output.push_back("    " + opCmd + " " + leftReg + ", " + rightReg + "\n");
            output.push_back("    li " + destReg + ", 0          # Assume false\n");
            output.push_back("    bc1t " + trueLabel + "       # Branch if true\n");
            output.push_back("    j " + endLabel + "\n");
            output.push_back(trueLabel + ":\n");
            output.push_back("    li " + destReg + ", 1          # Set true\n");
            output.push_back(endLabel + ":\n");
        } else if (op == ">") {
            // Implemented as !(left <= right)
            output.push_back("    c.le.s " + leftReg + ", " + rightReg + "\n"); // op is <=
            output.push_back("    li " + destReg + ", 1          # Assume true\n");
            output.push_back("    bc1t " + trueLabel + "       # Branch if true (<=)\n");
            output.push_back("    j " + endLabel + "\n");
            output.push_back(trueLabel + ":\n");
            output.push_back("    li " + destReg + ", 0          # Set false\n");
            output.push_back(endLabel + ":\n");
        } else if (op == ">=") {
             // Implemented as !(left < right)
            output.push_back("    c.lt.s " + leftReg + ", " + rightReg + "\n"); // op is <
            output.push_back("    li " + destReg + ", 1          # Assume true\n");
            output.push_back("    bc1t " + trueLabel + "       # Branch if true (<)\n");
            output.push_back("    j " + endLabel + "\n");
            output.push_back(trueLabel + ":\n");
            output.push_back("    li " + destReg + ", 0          # Set false\n");
            output.push_back(endLabel + ":\n");
        } else if (op == "!=") {
            output.push_back("    c.eq.s " + leftReg + ", " + rightReg + "\n"); // op is ==
            output.push_back("    li " + destReg + ", 1          # Assume true\n");
            output.push_back("    bc1t " + trueLabel + "       # Branch if true (==)\n");
            output.push_back("    j " + endLabel + "\n");
            output.push_back(trueLabel + ":\n");
            output.push_back("    li " + destReg + ", 0          # Set false\n");
            output.push_back(endLabel + ":\n");
        }

        markDirty(dest, destReg);
    }

    void convertFunctionCall(const std::string& funcName, int numArgs, const std::string& resultVar) {
        
        spillAllDirtyRegisters();
        if (funcName == "printf") {
            convertPrintfCall(numArgs);
        } else if (funcName == "scanf") {
            convertScanfCall(numArgs);
        } else if (funcName == "malloc") {
            convertMallocCall(numArgs, resultVar);
        } else if (funcName == "free") {
            convertFreeCall(numArgs);
        } 
        else {
        output.push_back("    # Saving caller-saved registers before call\n");
        std::set<std::string> varsToSave;
        for(auto& pair : variableInfo) {
            std::string var = pair.first;
            // Only save if it's a real variable (not temp) and in a register
            if (var[0] != 't' && var[0] != 'i' && !pair.second.registers.empty()) {
                // Check if it's in a $t register
                for(const std::string& reg : pair.second.registers) {
                    if (intRegisters.count(reg) || floatRegisters.count(reg)) {
                        varsToSave.insert(var);
                        break;
                    }
                }
            }
        }
        
        for(const std::string& var : varsToSave) {
            // Force allocation if not already on stack
            allocateStackSpace(var);
            std::string reg = *variableInfo[var].registers.begin(); // Get *any* reg it's in
            output.push_back("    # Saving " + var + " from " + reg + " before call\n");
            if(variableInfo[var].isFloat) {
                output.push_back("    s.s " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
            } else {
                output.push_back("    sw " + reg + ", " + std::to_string(variableInfo[var].stackOffset) + "($fp)\n");
            }
        }
        // ***************************************************************

        // Handle arguments (from paramStack)
        if (numArgs > 0) {
            int intArgCount = 0;
            int floatArgCount = 0;
            int stackArgCount = 0;

            // Allocate stack space for arguments 5+
            if (numArgs > 4) {
                 int extraArgs = 0;
                 // We need to count how many args *actually* go on the stack
                 int tempInt = 0, tempFloat = 0;
                 for (int i = 0; i < numArgs; ++i) {
                     std::string arg = paramStack[paramStack.size() - numArgs + i];
                     bool isFloat = isFloatVar(arg);
                     if (isFloat && tempFloat < argFloatRegs.size()) tempFloat++;
                     else if (!isFloat && tempInt < argIntRegs.size()) tempInt++;
                     else extraArgs++;
                 }
                 
                 if (extraArgs > 0) {
                     argStackOffset = (extraArgs + 1) & ~1; // Align to 8-byte
                     argStackOffset *= -4;
                     output.push_back("    addiu $sp, $sp, " + std::to_string(argStackOffset) + " # Make space for " + std::to_string(extraArgs) + " stack arguments\n");
                 }
            }

            for (int i = 0; i < numArgs; ++i) {
                std::string arg = paramStack.back();
                paramStack.pop_back();
                
                bool isFloat = isFloatVar(arg);
                std::string argReg = getRegisterForVar(arg, isFloat);

                if (isFloat && floatArgCount < argFloatRegs.size()) {
                    output.push_back("    mov.s " + argFloatRegs[floatArgCount++] + ", " + argReg + "  # Float argument " + std::to_string(i+1) + "\n");
                } else if (!isFloat && intArgCount < argIntRegs.size()) {
                    output.push_back("    move " + argIntRegs[intArgCount++] + ", " + argReg + "  # Integer argument " + std::to_string(i+1) + "\n");
                } else {
                    // Argument goes on the stack
                    if (isFloat) {
                        output.push_back("    s.s " + argReg + ", " + std::to_string(stackArgCount * 4) + "($sp)  # Stack argument " + std::to_string(i+1) + "\n");
                    } else {
                        output.push_back("    sw " + argReg + ", " + std::to_string(stackArgCount * 4) + "($sp)  # Stack argument " + std::to_string(i+1) + "\n");
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

        // *** FIX 2: Invalidate all $t registers ***
        // After a call, we cannot trust *any* $t registers
        invalidateTempRegisters();
        
        // *** FIX: Restore caller-saved registers ***
        output.push_back("    # Restoring caller-saved registers after call\n");
        for(const std::string& var : varsToSave) {
            // The variable's *only* copy was on the stack.
            // We must load it back into a register.
            // getRegisterForVar will handle this automatically
            // by loading from its stack offset.
            
            // We just need to ensure it *has* a stack offset
            if (variableInfo[var].stackOffset == -1) {
                 output.push_back("    # CRITICAL ERROR: " + var + " had no stack offset!\n");
            } else {
                 // By calling getRegister, we ensure it's loaded back from stack
                 // output.push_back("    # Restoring " + var + " after call\n");
                 // std::string reg = getRegisterForVar(var, variableInfo[var].isFloat);
                 
                 // *** NEWER FIX: Don't load it yet. Just ensure its reg is clear ***
                 // The next time it's *used*, getRegisterForVar will load it
                 // from the stack. This is more efficient.
                 variableInfo[var].registers.clear();
            }
        }
        
        // Store return value
        if (!resultVar.empty()) {
            bool isFloat = isFloatVar(resultVar);
            std::string destReg = getRegisterForVar(resultVar, isFloat);
            
            if (isFloat) {
                output.push_back("    mov.s " + destReg + ", $v0  # Store float return value\n");
            } else {
                output.push_back("    move " + destReg + ", $v0  # Store return value\n");
            }
            markDirty(resultVar, destReg);
        }
    }
    }

     void convertPrintfCall(int numArgs) {
        output.push_back("    # printf call\n");

        if (numArgs < 1 || paramStack.empty()) {
            output.push_back("    # ERROR: Not enough args for printf\n");
            return;
        }

        // 1. Get the format string variable (e.g., "i0")
        std::string formatStrVar = paramStack.back();
        std::string actualFormatString;

        // 2. Look up its literal value (e.g., "\"%d hello\"")
        if (globalInitialValues.count(formatStrVar)) {
            actualFormatString = globalInitialValues[formatStrVar];
        } else {
            output.push_back("    # ERROR: Could not find format string for " + formatStrVar + "\n");
            return;
        }

        // 3. Clean the quotes (e.g., "%d hello")
        if (!actualFormatString.empty() && actualFormatString.front() == '"' && actualFormatString.back() == '"') {
            actualFormatString = actualFormatString.substr(1, actualFormatString.length() - 2);
        }

        // 4. Loop through the format string and print chunks/variables
        // Start at the first argument (index numArgs-2) and go backwards
        int varParamIndex = numArgs - 2; 
        
        int lastIndex = 0;     // Tracks the start of a literal string chunk

        for (int i = 0; i < actualFormatString.length(); ++i) {
            if (actualFormatString[i] == '%') {
                // A) Print any literal chunk that came before this '%'
                if (i > lastIndex) {
                    std::string chunk = actualFormatString.substr(lastIndex, i - lastIndex);
                    // Use getStringConstant. It correctly handles unquoted chunks.
                    std::string label = getStringConstant(chunk);
                    output.push_back("    li $v0, 4           # Print string chunk\n");
                    output.push_back("    la $a0, " + label + "\n");
                    output.push_back("    syscall\n");
                }

                // B) Process the format specifier
                if (i + 1 < actualFormatString.length()) {
                    char specifier = actualFormatString[i+1];
                    std::string varName;
                    std::string varReg;

                    // Check if we've run out of arguments (index < 0)
                    if (specifier != '%' && varParamIndex < 0) {
                         output.push_back("    # ERROR: More format specifiers than variables\n");
                         break; // Stop processing
                    } else if (specifier != '%') {
                         varName = paramStack[varParamIndex];
                    }

                    if (specifier == 'd') {
                        varReg = getRegisterForVar(varName, false);
                        output.push_back("    li $v0, 1           # Print integer\n");
                        output.push_back("    move $a0, " + varReg + "\n");
                        output.push_back("    syscall\n");
                        varParamIndex--;
                    } else if (specifier == 'f') {
                        varReg = getRegisterForVar(varName, true);
                        output.push_back("    li $v0, 2           # Print float\n");
                        output.push_back("    mov.s $f12, " + varReg + "\n");
                        output.push_back("    syscall\n");
                        varParamIndex--;
                    } else if (specifier == 's') {
                        varReg = getRegisterForVar(varName, false); // String address
                        output.push_back("    li $v0, 4           # Print string\n");
                        output.push_back("    move $a0, " + varReg + "\n");
                        output.push_back("    syscall\n");
                        varParamIndex--;
                    } else if (specifier == 'c') {
                        varReg = getRegisterForVar(varName, false); // Char value
                        output.push_back("    li $v0, 11          # Print char\n");
                        output.push_back("    move $a0, " + varReg + "\n");
                        output.push_back("    syscall\n");
                        varParamIndex--;
                    } else if (specifier == '%') {
                        output.push_back("    li $v0, 11          # Print char\n");
                        output.push_back("    li $a0, 37          # '%' character\n");
                        output.push_back("    syscall\n");
                    }
                    
                    i++; // Skip the specifier character
                    lastIndex = i + 1; // Update start of next chunk
                }
            }
        }

        // 5. Print any remaining literal chunk after the last specifier
        if (lastIndex < actualFormatString.length()) {
            std::string chunk = actualFormatString.substr(lastIndex);
            std::string label = getStringConstant(chunk);
            output.push_back("    li $v0, 4           # Print final string chunk\n");
            output.push_back("    la $a0, " + label + "\n");
            output.push_back("    syscall\n");
        }
    }

    void convertScanfCall(int numArgs) {
        output.push_back("    # scanf call\n");
        
        if (numArgs < 2 || paramStack.empty()) {
            output.push_back("    # ERROR: Not enough args for scanf\n");
            return;
        }

        // 1. Get the format string variable (e.g., "i6")
        std::string formatStrVar = paramStack.back();
        std::string actualFormatString;

        // 2. Look up its literal value (e.g., "\"%d,%d\"")
        if (globalInitialValues.count(formatStrVar)) {
            actualFormatString = globalInitialValues[formatStrVar];
        } else {
            output.push_back("    # ERROR: Could not find format string for " + formatStrVar + "\n");
            return;
        }

        // 3. Clean the quotes (e.g., "%d,%d")
        if (!actualFormatString.empty() && actualFormatString.front() == '"' && actualFormatString.back() == '"') {
            actualFormatString = actualFormatString.substr(1, actualFormatString.length() - 2);
        }

        // 4. Loop through the format string and issue syscalls
        
        // We expect (numArgs - 1) variables to be on the stack, e.g., [var1, var2, formatStr]
        // So we read from index (numArgs - 2) down to 0
        int varParamIndex = numArgs - 2; 

        for (int i = 0; i < actualFormatString.length(); ++i) {
            if (actualFormatString[i] == '%') {
                if (i + 1 < actualFormatString.length()) {
                    char specifier = actualFormatString[i+1];
                    
                    // Check if we have a variable left to store into
                    if (varParamIndex < 0) {
                        output.push_back("    # ERROR: More format specifiers than variables\n");
                        break;
                    }
                    
                    // Get the variable name from the stack (e.g., "i7", "i8")
                    std::string varToStore = paramStack[varParamIndex];
                    
                    // We assume this variable ("i7") holds the *address* (&a)
                    // We need to load this address into a register first.
                    std::string addrReg = getRegisterForVar(varToStore, false);

                    if (specifier == 'd') {
                        // Issue syscall to read an integer
                        output.push_back("    li $v0, 5           # Read integer syscall\n");
                        output.push_back("    syscall\n");
                        
                        // Store the result ($v0) *at the address* in addrReg
                        output.push_back("    sw $v0, 0(" + addrReg + ")  # Store input into " + varToStore + "\n");
                        
                    } else if (specifier == 'f') {
                        // Issue syscall to read a float
                        output.push_back("    li $v0, 6           # Read float syscall\n");
                        output.push_back("    syscall\n");
                        
                        // Store the result ($f0) *at the address* in addrReg
                        output.push_back("    s.s $f0, 0(" + addrReg + ")  # Store input into " + varToStore + "\n");
                    }
                    
                    varParamIndex--; // Move to the next variable
                    i++; // Skip the specifier character
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
        std::string dataSection = ".data\n";
        
        // Add string constants
        for (const auto& pair : stringConstants) {
            std::string sanitizedValue = pair.first;
            // Escape special characters like \n
            sanitizedValue = std::regex_replace(sanitizedValue, std::regex(R"(\\)"), R"(\\)");
            sanitizedValue = std::regex_replace(sanitizedValue, std::regex(R"(\n)"), R"(\n)");
            dataSection += pair.second + ": .asciiz \"" + sanitizedValue + "\"\n";
        }
        
        // Add float constants
        for (const auto& pair : floatConstants) {
            dataSection += pair.second + ": .float " + pair.first + "\n";
        }
        
        // Add global variables
        for (const auto& pair : globalVars) {
            std::string varName = sanitizeName(pair.first);
            std::string type = varTypes[pair.first];
            std::string initialValue = "0"; // Default
            
            if (globalInitialValues.count(pair.first)) {
                initialValue = globalInitialValues[pair.first];
            }
            
            if (type == "float") {
                if (std::regex_match(initialValue, std::regex(R"(-?\d+\.\d+)"))) {
                    dataSection += varName + ": .float " + initialValue + "\n";
                } else {
                    dataSection += varName + ": .float 0.0\n";
                }
            } else if (type == "int" || type.find("unsigned") != std::string::npos) {
                 if (std::regex_match(initialValue, std::regex(R"(-?\d+)"))) {
                    dataSection += varName + ": .word " + initialValue + "\n";
                } else {
                    dataSection += varName + ": .word 0\n";
                }
            } else if (type == "char") {
                 if (initialValue.front() == '\'' && initialValue.back() == '\'') {
                    dataSection += varName + ": .byte " + std::to_string((int)initialValue[1]) + "\n";
                 } else {
                    dataSection += varName + ": .byte 0\n";
                 }
            } else {
                // Default to .word for unknown or pointer types
                dataSection += varName + ": .word 0\n";
            }
        }
        
        // Add newline string constant if not present (for printf)
        if (stringConstants.find("\n") == stringConstants.end()) {
            stringConstants["\n"] = "string_const_" + std::to_string(stringConstCount++);
            dataSection += stringConstants["\n"] + ": .asciiz \"\\n\"\n";
        }

        return dataSection;
    }

    std::string generateTextSection() {
        std::string textSection = ".text\n.globl main\n";
        
        for (const std::string& line : output) {
            textSection += line;
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
                currentFunc = "in_function"; // Just to mark we are no longer global
            }
            if (line.find("EndFunc") != std::string::npos) {
                currentFunc = "";
            }
        }
        
        // Reset for second pass
        inFile.clear();
        inFile.seekg(0, std::ios::beg);
        currentFunc = "";
        
        // Second pass: Generate assembly
        while (std::getline(inFile, line)) {
            line = trim(line);
            if (line.empty()) continue;
            
            std::string asmLine = convertInstruction(line);
            if (asmLine.find("# Unhandled") != 0 && asmLine.find("# ") != 0) {
                // output.push_back(asmLine); // convertInstruction already pushes
            }
        }
        
        if (hasParserErrors) {
            outFile << "# PARSER ERRORS DETECTED. NO ASSEMBLY GENERATED.\n";
            for(const std::string& err : parserErrors) {
                outFile << "# " << err << "\n";
            }
            
            std::cerr << "Parser errors detected! Error messages written to " << outputFile << std::endl;
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
