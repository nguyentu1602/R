/*
  Program to pre-process the data file downloaded from tickdata.com
*/

#include <stdio.h>
#include<string.h>
#include<stdlib.h>

#define numlines 21500000
#define prefix "2013/"

// This section is for headers of helper functions:
// Remember that BUFSIZ is the standard buffer size for each. My system's buff is 1024


// MAIN :

int main ()
{
FILE * infile = NULL; //Create a pointer to a file, later use with fopen() which returns a file pointer
FILE * outfile = NULL;

char * infilename = (char *) malloc (50 + 1); // Allocate a pointer to store file name 1
char * temp = (char *) malloc (70 + 1);
char * ptr = temp + 6;
char * ptr2 = ptr + 10;
int len;
int i = 0;
int max = 1;

// Allocate 22,000,000 contagious blocks, each 101 chars in size
// So buffer[i] is a pointer to a string of length 70 
char (* buffer)[51] = malloc (numlines * sizeof(* buffer));

//infilename = "/Users/cuongnguyen/Downloads/ES_Sample/ES_Trades.csv"; 
infilename = "/Users/cuongnguyen/Downloads/ES_Sample/ES_Quotes.csv"; 

infile = fopen(infilename, "r+");
outfile = fopen("/Users/cuongnguyen/Downloads/ES_Sample/Quotes_fixed.csv", "wb");

fgets(temp, 100, infile); //This reads lines into temp, including \n

//for(int i = 0; i <= 10000000; i++) {
  
while ( fgets(temp, 100, infile) != NULL) {
*(ptr2) = ' ';
memcpy(buffer[i], &prefix, 5);  
memcpy(buffer[i] + 5, ptr, 5 );
len = strlen(ptr2) - 5;
memcpy(buffer[i] + 10, ptr2, len);
*(buffer[i] + 10 + len) = '\n';
i++;
//max = fmax(max, len);
//printf("%s", buffer[i]);

} // while()

i = i-10;

printf("%s\n", buffer[i]);


fwrite(buffer, 1, numlines * 51 * sizeof(char), outfile);

// close the file
fclose(infile);
fclose(outfile);
free(temp);
free(buffer);

return 0;
}// main()
