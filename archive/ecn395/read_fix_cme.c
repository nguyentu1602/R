/*
  Program to pre-process the FIX data file downloaded from CME group
*/

#include <stdio.h>
#include<string.h>
#include<stdlib.h>

// MAIN :

int main ()
{
  FILE * infile = NULL; //Create a pointer to a file, later use with fopen() which returns a file pointer
  FILE * outfile = NULL;

 char * temp = (char *) malloc (7000 + 1);

  char * infilename = (char *) malloc (50 + 1); // Allocate a pointer to store file name 1

 infilename = "/Users/cuongnguyen/Downloads/MDFF_CME_20130714-20130715_7817_0";
infile = fopen(infilename, "r+");

for (int i = 0; i<5; i++){
 fgets(temp, 7000, infile); //This reads the first line into temp, including \n  
  printf("%s\n\n", temp);
}
  fclose(infile);

 

return 0;
}//main()
