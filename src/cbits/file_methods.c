#include <stdio.h>

int update(char *file_name, int byte, int len, uint8 *data) {
    FILE *file_ptr;

    file_ptr = fopen(file_name, "wr");

    if(file_ptr) {
    }
    else {
        printf("Unable to open the file!");
        return 1;
    }

    fclose(file_ptr);

    return 0; 
}

int delete(char *file_name, int byte, int len) { 
    FILE *file_ptr;

    return 1; 
}
