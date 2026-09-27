/*
 * Copyright (C) 2019-2021 Data Ductus AB
 *
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

void $default__init__($WORD s) {
    return;
}

static void $raise_signed_int_range(int64_t value, const char *type_name) {
    char errmsg[128];
    snprintf(errmsg, sizeof(errmsg), "%s(): value %lld out of range for type %s",
             type_name, (long long)value, type_name);
    $RAISE((B_BaseException)$NEW(B_ValueError, actStrFromCStringCopy(errmsg)));
}

static void $raise_unsigned_int_range(uint64_t value, const char *type_name) {
    char errmsg[128];
    snprintf(errmsg, sizeof(errmsg), "%s(): value %llu out of range for type %s",
             type_name, (unsigned long long)value, type_name);
    $RAISE((B_BaseException)$NEW(B_ValueError, actStrFromCStringCopy(errmsg)));
}

int64_t $checked_int_from_i64(int64_t value, int64_t min, int64_t max, const char *type_name) {
    if (value < min || value > max)
        $raise_signed_int_range(value, type_name);
    return value;
}

uint64_t $checked_uint_from_i64(int64_t value, uint64_t max, const char *type_name) {
    if (value < 0 || (uint64_t)value > max)
        $raise_signed_int_range(value, type_name);
    return (uint64_t)value;
}

int64_t $checked_int_from_u64(uint64_t value, int64_t max, const char *type_name) {
    if (value > (uint64_t)max)
        $raise_unsigned_int_range(value, type_name);
    return (int64_t)value;
}

uint64_t $checked_uint_from_u64(uint64_t value, uint64_t max, const char *type_name) {
    if (value > max)
        $raise_unsigned_int_range(value, type_name);
    return value;
}

/*
void B_printobj(char *mess,$WORD obj) {
    B_value obj1 = (B_value)obj;
    printf("%s %s\n",mess,obj1->$class->__str__(obj1)->str);
}
*/

char *unmangle_name(char *input) {
    // Check for NULL input
    if (input == NULL) {
        return NULL;
    }
    char *output = GC_malloc(strlen(input) + 1);
    // Check for allocation failure
    if (output == NULL) {
        return NULL;
    }

    // Check if string starts with "B_"
    if (strncmp(input, "B_", 2) == 0) {
        // Copy string without "B_"
        strcpy(output, input + 2);
    } else {
        // Copy string as is
        strcpy(output, input);
    }
    // Handle "Q_" mangling for nested modules
    char *pos;
    while ((pos = strstr(output, "Q_")) != NULL) {
        *pos = '.';
        memmove(pos + 1, pos + 2, strlen(pos + 2) + 1);
    }

    return output;
}
