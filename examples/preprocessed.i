# 0 "cJSON.c"

const char * cJSON_GetErrorPtr(void)
{
    return (const char*) (global_error.json + global_error.position);
}
