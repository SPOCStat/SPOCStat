#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

size_t GetPageSize(void) {
#ifdef _WIN32
	SYSTEM_INFO system_info;
	GetSystemInfo(&system_info);
	return system_info.dwPageSize;
#else
#if defined(HAVE_SYSCONF) && defined(_SC_PAGESIZE)
	size_t r = sysconf(_SC_PAGESIZE);
	if (r > 0) return r;
#endif
	return 4096; // low, but common value
#endif
}
