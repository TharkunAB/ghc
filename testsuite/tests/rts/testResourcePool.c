#include "Pool.h"

#include <stdio.h>
#include <stdlib.h>

static int resource;
static unsigned releases;
static bool fail_next;

static void *acquire(void)
{
    if (fail_next) {
        fail_next = false;
        return NULL;
    }
    return &resource;
}

static void release(void *value)
{
    if (value != &resource) abort();
    releases++;
}

static void expect(bool ok, const char *message)
{
    if (!ok) {
        fprintf(stderr, "%s\n", message);
        exit(EXIT_FAILURE);
    }
}

int main(int argc, char **argv)
{
    hs_init(&argc, &argv);
    Pool *p = poolInit(1, 1, acquire, release);
    void *value = poolTake(p);
    expect(value == &resource && poolFree(p) == 1, "busy pool destruction");
    poolRelease(p, value);
    expect(releases == 0, "cached resource released too early");
    expect(poolFree(p) == 0 && releases == 1, "cached resource leaked");

    p = poolInit(1, 0, acquire, release);
    value = poolTake(p);
    poolRelease(p, value);
    expect(releases == 2, "idle size zero retained a resource");
    value = poolTryTake(p);
    expect(value == &resource, "returned resource still counts towards capacity");
    poolRelease(p, value);
    expect(poolFree(p) == 0 && releases == 3, "unused capacity pool cleanup");

    p = poolInit(1, 1, acquire, release);
    fail_next = true;
    expect(poolTake(p) == NULL, "allocation failure not returned");
    value = poolTryTake(p);
    expect(value == &resource, "allocation failure consumed capacity");
    poolFlush(p);
    poolRelease(p, value);
    expect(releases == 4, "flushed resource retained after return");
    value = poolTryTake(p);
    expect(value == &resource, "flushed resource still counts towards capacity");
    poolRelease(p, value);
    expect(poolFree(p) == 0 && releases == 5, "final pool cleanup");
    hs_exit();
    return 0;
}
