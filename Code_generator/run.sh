#!/bin/bash
set -e

cd "$(dirname "$0")"

echo "[*] Building compiler components..."
make >/dev/null 2>&1

mkdir -p tac_output
mkdir -p asm_output

# Check if we have the required binaries
if [[ ! -f "./parser" ]]; then
    echo "[!] Error: parser not found. Build failed."
    exit 1
fi

# Check if code_generator exists, if not try to build it
if [[ ! -f "./code_generator" ]]; then
    if [[ -f "src/code_generator.cpp" ]]; then
        echo "[*] Building code generator..."
        g++ -std=c++11 -Wall -Wextra -O2 -o code_generator src/code_generator.cpp
    else
        echo "[!] Error: code_generator.cpp not found in src/ directory."
        echo "[!] Please ensure code_generator.cpp is in Code_generator/src folder."
        exit 1
    fi
fi

echo "[*] Step 1: Generating TAC files from C sources..."

# First pass: Generate TAC files from all C sources
for testfile in test/*.c; do
    if [[ -f "$testfile" ]]; then
        base=$(basename "$testfile")
        tacfile="tac_output/${base%.*}.txt"
        
        echo "[*] Generating TAC: $base -> $tacfile"
        
        # Use parser to generate TAC from C source
        ./parser < "$testfile" > "$tacfile" 2>&1
        
        # Check if TAC generation was successful
        if [[ $? -eq 0 ]] && [[ -s "$tacfile" ]]; then
            echo "[+] TAC generated: $tacfile"
        else
            echo "[!] TAC generation failed for $base"
            # Create error TAC file
            echo "// Parser Error: Failed to generate TAC from $base" > "$tacfile"
            echo "// Check the C source for syntax errors" >> "$tacfile"
        fi
    fi
done

echo ""
echo "[*] Step 2: Converting TAC files to SPIM assembly..."

# Second pass: Convert TAC files to SPIM assembly
for tacfile in tac_output/*.txt; do
    if [[ -f "$tacfile" ]]; then
        base=$(basename "$tacfile")
        asmfile="asm_output/${base%.*}.asm"
        
        echo "[*] Converting TAC: $base -> $asmfile"
        
        # Check if TAC file has parser errors
        if grep -q "Parser Error" "$tacfile" || \
           grep -q "syntax error" "$tacfile" || \
           grep -q "no member named" "$tacfile" || \
           grep -q "lvalue required" "$tacfile" || \
           grep -q "Called object is not a function" "$tacfile" || \
           grep -q "must be a pointer" "$tacfile" || \
           grep -q "free: argument must be a pointer" "$tacfile"; then
            
            echo "[!] Parser errors detected in $base - generating error assembly"
            # Generate assembly file with error messages
            ./code_generator "$tacfile" >/dev/null 2>&1 || true
            
            # Move the generated .asm file to asm_output
            if [[ -f "${tacfile%.*}.asm" ]]; then
                mv "${tacfile%.*}.asm" "$asmfile"
                echo "[+] Error assembly generated: $asmfile"
            else
                # Create a basic error assembly file
                echo "# Error: Could not generate assembly from TAC with parser errors" > "$asmfile"
                echo ".data" >> "$asmfile"
                echo "error_msg: .asciiz \"Parser errors in TAC file\\n\"" >> "$asmfile"
                echo ".text" >> "$asmfile"
                echo "main:" >> "$asmfile"
                echo "    li \$v0, 4" >> "$asmfile"
                echo "    la \$a0, error_msg" >> "$asmfile"
                echo "    syscall" >> "$asmfile"
                echo "    li \$v0, 10" >> "$asmfile"
                echo "    syscall" >> "$asmfile"
                echo "[+] Created error assembly: $asmfile"
            fi
        else
            # Convert valid TAC to SPIM assembly
            if ./code_generator "$tacfile" >/dev/null 2>&1; then
                if [[ -f "${tacfile%.*}.asm" ]]; then
                    mv "${tacfile%.*}.asm" "$asmfile"
                    echo "[+] Assembly generated: $asmfile"
                else
                    echo "[!] No assembly file generated for $base"
                    # Create error assembly file
                    echo "# Error: TAC to assembly conversion failed" > "$asmfile"
                    echo "[+] Created error assembly: $asmfile"
                fi
            else
                echo "[!] TAC to assembly conversion failed for $base"
                # Create error assembly file
                echo "# Error: TAC to assembly conversion failed" > "$asmfile"
                echo "[+] Created error assembly: $asmfile"
            fi
        fi
    fi
done

echo ""
echo "[*] Pipeline complete."
echo "[*] TAC files: ./tac_output/ ($(ls -1 tac_output/*.txt 2>/dev/null | wc -l) files)"
echo "[*] ASM files: ./asm_output/ ($(ls -1 asm_output/*.asm 2>/dev/null | wc -l) files)"
echo ""
echo "[*] Generated files in current directory:"
ls -la lex.yy.c parser.tab.c parser.tab.h parser code_generator 2>/dev/null | tail -n +2