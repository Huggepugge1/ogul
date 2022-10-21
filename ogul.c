#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct instruction {
    int id;
    int value;
    int ip;
};

const int EXIT = 0;
const int WRITE = 1;
const int READ = 2;
const int PRINT_INTEGER = 3;

const char STRING[] = "string";
const char DB[] = ": db ";
const char LENEQU[] = "len: EQU $ - ";

const int DEAFAULT_LEN = 10000;
const int STRING_LEN = 825;

void data(char strings[DEAFAULT_LEN][DEAFAULT_LEN], size_t stringNumber, FILE *f) {
    fprintf(f, "section .data\n");
    for (size_t i = 0; i < stringNumber; i++) {
        fprintf(f, "   %s%d%s \"%s\", 10\n", STRING, i, DB, strings[i]);
        fprintf(f, "   %s%d%s%s%d\n", STRING, i, LENEQU, STRING, i);
    }
}

void printString(char strings[DEAFAULT_LEN][DEAFAULT_LEN], int stringNumber, int ip, FILE *f) {
    fprintf(f, 
            "ip%zu:\n"
            "   mov rax, 1\n"
            "   mov rbx, 1\n"
            "   mov rsi, %s%d\n"
            "   mov rdx, %s%dlen\n"
            "   syscall\n"
            , ip, STRING, stringNumber, STRING, stringNumber);
}

void exitProgram(int value, FILE *f, int ip) {
    fprintf(f, 
            "ip%d:\n"
            "   mov rax, 60\n"
            "   mov rbx, %d\n"
            "   syscall\n", ip, value);
}

struct instruction * parseFile(char fp[]) {
    FILE *f;
    if (!f == fopen(fp, "r")) {
        printf("File %s does not exist", fp);
        exit(1);
    }
    char buf[STRING_LEN];

    while ((fscanf(f, "%s", buf)) == 1) {
        printf("%s\n", buf);
    }
    
    struct instruction * ptr;

    return ptr;
}

int main() {
    char   strings[DEAFAULT_LEN][STRING_LEN];
    int stringNumber = 1;
    
    struct instruction instructions[DEAFAULT_LEN];

    instructions[0].id = 1;
    instructions[0].value = 0;
    instructions[0].ip = 1;
    instructions[1].id = 0;
    instructions[1].value = 0;
    instructions[1].ip = 2;
   
    strcpy(strings[0], "Hello, World!");
     
    char start[] = 
        "\n"
        "section .text\n"
        "global _start\n"
        "_start:\n"
        "   jmp ip1\n";

    FILE *f = fopen("./out.asm", "w");
    data(strings, stringNumber, f);
    fprintf(f, "%s", start);

    int ip = 0;
    while (instructions[ip].ip != 0) {
        struct instruction ins = instructions[ip];
        if (ins.id == EXIT) {
            exitProgram(ins.value, f, ins.ip);
        
        } else if (ins.id == WRITE) {
            printString(strings, ins.value, ins.ip, f);
        
        } else {
            printf("Not a valid command");
            exit(1);
        }
        ip++;
    }
    fclose(f);
    parseFile("foo.ogul");
    return 0;
}
