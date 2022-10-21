#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

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

void data(int stringNumber, char str[], FILE *f) {
    fprintf(f, "   %s%d%s \"%s\", 10\n", STRING, stringNumber, DB, str);
    fprintf(f, "   %s%d%s%s%d\n", STRING, stringNumber, LENEQU, STRING, stringNumber);
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

void exitProgram(int value, int ip, FILE *f) {
    fprintf(f, 
            "ip%d:\n"
            "   mov rax, 60\n"
            "   mov rbx, %d\n"
            "   syscall\n", ip, value);
}

struct instruction parseToken(char *token, int lineNum, int *stringNumber, int ip, FILE *f) {
    if (strncmp(token, "write", strlen("write"))) {
        char str[STRING_LEN];
        bool found = false;
        for (size_t i = 6; i < sizeof(token); i++) {
            if (found) {
                if (!((size_t)token + i == '"')) {
                str[i] = (size_t)token + i;    
                } else {
                    data(*stringNumber, str, f);
                    struct instruction ins = {
                        .id = 1,
                        .value = *stringNumber,
                        .ip = ip
                    };
                    *stringNumber++;
                    return ins;
                }
            } else {
                if ((size_t)token + i == '"') {
                    found = true;
                }
            }
        }
    } else {
        printf("Not a valid command: %s %s", token, lineNum);
        exit(1);
    }
}

struct instruction * parseFile(char fp[]) {
    FILE *f;
    
    if (!(f = fopen(fp, "r"))) {
        printf("File %s does not exist", fp);
        exit(1);
    }
    
    struct instruction instructions[DEAFAULT_LEN];
    
    int    ip = 0;
    
    char   *fileToken = "";
    size_t  fileTokenSize = STRING_LEN;
    ssize_t read;

    int lineNum = 0;
    int stringNum = 0;

    read = (ssize_t)getline(&fileToken, &fileTokenSize, f);

    /* while ((read = (ssize_t)getline(&buf, &bufsize, f)) == 1) { */
    /*     char *token = strtok(buf, ";"); */
    /*     instructions[ip] = parseToken(token, lineNum, &stringNum, ip, f); */
    /*     while (*token = strtok(NULL, " ") != NULL) { */
    /*         parseToken(token, lineNum, &stringNum, ip, f); */
    /*     } */
    /*     lineNum++; */
    /* } */    
    
    struct instruction * ptr;

    return ptr;
}

int main() {
    char strings[DEAFAULT_LEN][STRING_LEN];
    int  stringNumber = 1;
    
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
    fprintf(f, "section .data");
    fprintf(f, "%s", start);

    int ip = 0;
    while (instructions[ip].ip != 0) {
        struct instruction ins = instructions[ip];
        if (ins.id == EXIT) {
            exitProgram(ins.value, ins.ip, f);
        
        } else if (ins.id == WRITE) {
            printString(strings, ins.value, ins.ip, f);
        
        } else {
            printf("Not a valid command");
            exit(1);
        }
        ip++;
    }

    char* str;
    str = (char *)malloc(50*sizeof(char));

    fclose(f);
    parseFile("foo.ogul");
    return 0;
}
