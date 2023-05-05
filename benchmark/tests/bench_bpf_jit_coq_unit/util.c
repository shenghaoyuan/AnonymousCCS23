#include <inttypes.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include "embUnit.h"
#include "timex.h"
#include "ztimer.h"
#include "bpf/shared.h"
#include "bpf/instruction.h"
#include "unaligned.h"
#include "util.h"

void fill_instruction(const bpf_instruction_t *instr, test_application_t *test_app)
{
    test_app->header.data_len = 0;
    test_app->header.rodata_len = 68;
    test_app->header.text_len = sizeof(uint64_t) * (NUM_INSTRUCTIONS + 1);

    for (size_t i = 0; i < NUM_INSTRUCTIONS; i++) {
        memcpy(&test_app->text[i], instr, sizeof(bpf_instruction_t));
    }
    static const bpf_instruction_t return_instr = {
        .opcode = BPF_INSTRUCTION_CLS_BRANCH | BPF_INSTRUCTION_BRANCH_EXIT
    };
    memcpy(&test_app->text[NUM_INSTRUCTIONS], &return_instr, sizeof(bpf_instruction_t));
}

