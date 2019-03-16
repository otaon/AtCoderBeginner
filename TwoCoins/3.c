#include <stdio.h>
#include <stdlib.h>
#include <string.h>
int main(int argc, char** argv){
	int a, b, c;
	//char* env;
	//char* yes;

	//env = getenv("OS");
	//printf("%s\n", env);
	//// No
	////printf("%c%c\n", env[2] + env[0] - env[5], env[4]);
	//printf("%c%c\n", 2[env] + 0[env] - 5[env], 4[env]);

	//// Yes
	////printf("%c%c%c\n", env[0] + 2, env[1] + env[1] - env[2] + argc, env[6]);
	//printf("%c%c%c\n", 0[env] + 2, 1[env] + 1[env] - 2[env] + argc, 6[env]);

	scanf("%d %d %d", &a, &b, &c);
	printf((((unsigned int)(c - a - b) >> 0xffff & 0x0001) << 2) + "No\n\0Yes\n");
}
