#include <stdio.h>
#include <stdint.h>

void _puti(int16_t n)
{
	printf("%hd", n);
}

void _putb(int8_t n)
{
	if(n)
		printf("true");
	else
		printf("false");
}

void _putc(int8_t n)
{
    printf("%c", n);
}

void _puts(int8_t *s)
{
    printf("%s", s);
}

int16_t _geti()
{
    int16_t n;
    scanf("%hd", &n);
    return n;
}

int8_t _getb()
{
	int8_t n, m;
	scanf("%c", &n);
	if(n=='t')
		m=1;
	else
		m=0;
	do {
		scanf("%c", &n);
	} while(n!='\n' || n!=EOF);
	return m;
}

int8_t _getc()
{
    int8_t n;
    scanf("%c", &n);
    return n;
}

void _gets(int16_t n, int8_t *s)
{
    for(int16_t i=0; i<n; ++i)
    {
        int8_t c = (i+1 < n) ? getchar() : '\0';
        if(c=='\n' || c==EOF)
        {
            *s++ = '\0';
            return;
        }
        *s++ = (int8_t) c;
    }
    return;
}

int16_t _abs(int16_t n)
{
	return (n>=0) ? n : -n;
}

int16_t _ord(int8_t n)
{
	return (int16_t) n;
}

int8_t _chr(int16_t n)
{
	return (int8_t) n;
}

int16_t _strlen(int8_t *s)
{
    int16_t len = 0;
    while(*s != '\0')
    {
        len++;
        *s++;
    }
    return len;
}

int16_t _strcmp(int8_t *s1, int8_t *s2)
{
    for (int16_t i=0;; ++i)
    {
        if(s1[i]!=s2[i])
            return (s1[i]<s2[i]) ? -1 : 1;
        if(s1[i]=='\0')
            return 0;
    }
    return 0;
}

void _strcpy(int8_t *trg, int8_t *src)
{
    for(int16_t i=0;; ++i)
    {
        trg[i] = src[i];
        if(src[i] == '\0')
            return;
    }
    return;
}

void _strcat(int8_t *trg, int8_t *src)
{
    int16_t i, j;
    for(i=0; trg[i]!='\0'; ++i) {}
    for(j=0;; ++j)
    {
        trg[i+j] = src[j];
        if(src[j]=='\0')
            return;
    }
    return;
}
