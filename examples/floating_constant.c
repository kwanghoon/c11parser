double cJSON_GetNumberValue(const int * const item)
{
    if (!cJSON_IsNumber(item))
    {
        return (double) 0.0/0.0;
    }

    return item->valuedouble;
}
