#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char INPUT[];

typedef enum {
  INSTRUCTION_TYPE_NOP,
  INSTRUCTION_TYPE_HALF,
  INSTRUCTION_TYPE_TRIPLE,
  INSTRUCTION_TYPE_INCR,
  INSTRUCTION_TYPE_JMP,
  INSTRUCTION_TYPE_JMP_IF_EVEN,
  INSTRUCTION_TYPE_JMP_IF_ONE,
  INSTRUCTION_END,
} INSTRUCTION_TYPE;

typedef enum {
  REGISTER_A,
  REGISTER_B,
} REGISTER;

typedef struct {
  INSTRUCTION_TYPE typ;
  REGISTER reg;
  int offset;
} Instruction;

REGISTER parse_register(const char line[]) {
  return (line[4] == 'a') ? REGISTER_A : REGISTER_B;
}

#pragma clang diagnostic push
#pragma ide diagnostic ignored "cert-err34-c"
Instruction parse_line(const char line[]) {
  if (strncmp(line, "jio", 3) == 0) {
    return (Instruction){.typ = INSTRUCTION_TYPE_JMP_IF_ONE,
                         .reg = parse_register(line),
                         .offset = atoi(&line[7])};
  } else if (strncmp(line, "jie", 3) == 0) {
    return (Instruction){.typ = INSTRUCTION_TYPE_JMP_IF_EVEN,
                         .reg = parse_register(line),
                         .offset = atoi(&line[7])};
  } else if (strncmp(line, "jmp", 3) == 0) {
    return (Instruction){.typ = INSTRUCTION_TYPE_JMP, .offset = atoi(&line[4])};
  } else if (strncmp(line, "hlf", 3) == 0) {
    return (Instruction){.typ = INSTRUCTION_TYPE_HALF,
                         .reg = parse_register(line)};
  } else if (strncmp(line, "tpl", 3) == 0) {
    return (Instruction){.typ = INSTRUCTION_TYPE_TRIPLE,
                         .reg = parse_register(line)};
  } else if (strncmp(line, "inc", 3) == 0) {
    return (Instruction){.typ = INSTRUCTION_TYPE_INCR,
                         .reg = parse_register(line)};
  } else {
    return (Instruction){.typ = INSTRUCTION_TYPE_NOP};
  }
}
#pragma clang diagnostic pop

Instruction *parse(char input[]) {
  Instruction *instructions = malloc(100 * sizeof(Instruction));
  char *line = strtok(input, "\n");
  *instructions = parse_line(line);

  Instruction *head = instructions;
  while (line != NULL) {
    line = strtok(NULL, "\n");
    if (line != NULL) {
      instructions++;
      *instructions = parse_line(line);
    }
  }

  instructions++;
  *instructions = (Instruction){.typ = INSTRUCTION_END};
  return head;
}

size_t run_vm(const Instruction *instructions) {
  size_t a = 0;
  size_t b = 0;
  size_t jmp_cmp;
  while (instructions->typ != INSTRUCTION_END) {
    switch (instructions->typ) {
    case INSTRUCTION_TYPE_JMP:
      instructions += instructions->offset;
      continue;
    case INSTRUCTION_TYPE_JMP_IF_EVEN:
      if (instructions->reg == REGISTER_A) {
        jmp_cmp = a;
      } else {
        jmp_cmp = b;
      }
      if (jmp_cmp % 2 == 0) {
        instructions += instructions->offset;
        continue;
      } else {
        break;
      }
    case INSTRUCTION_TYPE_JMP_IF_ONE:
      if (instructions->reg == REGISTER_A) {
        jmp_cmp = a;
      } else {
        jmp_cmp = b;
      }
      if (jmp_cmp == 1) {
        instructions += instructions->offset;
        continue;
      } else {
        break;
      }
    case INSTRUCTION_TYPE_HALF:
      if (instructions->reg == REGISTER_A) {
        a /= 2;
      } else {
        b /= 2;
      }
      break;
    case INSTRUCTION_TYPE_TRIPLE:
      if (instructions->reg == REGISTER_A) {
        a *= 3;
      } else {
        b *= 3;
      }
      break;
    case INSTRUCTION_TYPE_INCR:
      if (instructions->reg == REGISTER_A) {
        a++;
      } else {
        b++;
      }
      break;
    case INSTRUCTION_TYPE_NOP:
      break;
    case INSTRUCTION_END:
      return b;
    }
    instructions++;
  }
  return b;
}

int main() {
  const Instruction *instructions = parse(INPUT);
  printf("%zu\n", run_vm(instructions));
}

char INPUT[] = "jio a, +22\n"
               "inc a\n"
               "tpl a\n"
               "tpl a\n"
               "tpl a\n"
               "inc a\n"
               "tpl a\n"
               "inc a\n"
               "tpl a\n"
               "inc a\n"
               "inc a\n"
               "tpl a\n"
               "inc a\n"
               "inc a\n"
               "tpl a\n"
               "inc a\n"
               "inc a\n"
               "tpl a\n"
               "inc a\n"
               "inc a\n"
               "tpl a\n"
               "jmp +19\n"
               "tpl a\n"
               "tpl a\n"
               "tpl a\n"
               "tpl a\n"
               "inc a\n"
               "inc a\n"
               "tpl a\n"
               "inc a\n"
               "tpl a\n"
               "inc a\n"
               "inc a\n"
               "tpl a\n"
               "inc a\n"
               "inc a\n"
               "tpl a\n"
               "inc a\n"
               "tpl a\n"
               "tpl a\n"
               "jio a, +8\n"
               "inc b\n"
               "jie a, +4\n"
               "tpl a\n"
               "inc a\n"
               "jmp +2\n"
               "hlf a\n"
               "jmp -7\n";