#ifndef UFSDEFS_H
#define UFSDEFS_H 1
#include "lispemul.h" /* for LispPTR */

LispPTR UFS_getfilename(LispPTR *args);
LispPTR UFS_deletefile(LispPTR *args);
LispPTR UFS_renamefile(LispPTR *args);
LispPTR UFS_directorynamep(LispPTR *args);
void UnixVersionToLispVersion(char *pathname, size_t pathsize, int vlessp);
void LispVersionToUnixVersion(char *pathname, size_t pathsize);
int unixpathname(char *src, char *dst, size_t dstlen, int versionp, int genp);
int lisppathname(char *fullname, char *lispname, size_t lispnamesize, int dirp, int versionp);
#endif
