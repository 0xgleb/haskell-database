#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define CANNOT_OPEN_FILE_ERROR 1
#define CANNOT_WRITE_ALL_DATA 2
#define CANNOT_READ_ALL_DATA 3
#define SUCCESS 0

int update(char *file_name, long byte, size_t len, uint8_t *data) {
    FILE *file_ptr;
		uint8_t *backup;

    file_ptr = fopen(file_name, "r+b");

    if(file_ptr) {
			fseek(file_ptr, byte, SEEK_SET);
			backup = (uint8_t*) malloc(len);

			fread(backup, 1, len, file_ptr);
			fseek(file_ptr, byte, SEEK_SET);

			size_t bytes_written = fwrite(data, 1, len, file_ptr);

			if(bytes_written != len) {
				fwrite(backup, 1, bytes_written, file_ptr);
				fclose(file_ptr);

				return CANNOT_WRITE_ALL_DATA;
			}
    }
    else {
        printf("Unable to open the file %s!\n", file_name);
        return CANNOT_OPEN_FILE_ERROR;
    }

    fclose(file_ptr);

    return SUCCESS;
}

int delete(char *file_name, int byte, int len) {
    FILE *file_ptr;

    return 1;
}
