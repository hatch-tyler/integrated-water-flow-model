/* Store a regular time series in a new DSS-7 file, read it back, compare. */
#include <stdio.h>
#include <string.h>
#include "heclib.h"

#define N 10

int main(void)
{
    long long ifltab[250];
    const char *file = "roundtrip.dss";
    const char *path = "/BASIN/LOC/FLOW//1DAY/HECLIB-TEST/";
    double values[N];
    int i, status;

    for (i = 0; i < N; i++) values[i] = 1.5 * i - 2.25;
    memset(ifltab, 0, sizeof ifltab);
    remove(file);
    zset("MLVL", "", 1);

    status = hec_dss_zopen(ifltab, file);
    if (status != 0) { printf("zopen failed: %d\n", status); return 1; }

    zStructTimeSeries *tss = zstructTsNewRegDoubles(path, values, N,
        "01Jan2000", "2400", "CFS", "PER-AVER");
    status = ztsStore(ifltab, tss, 0);
    zstructFree(tss);
    if (status != 0) { printf("ztsStore failed: %d\n", status); return 1; }

    zStructTimeSeries *back = zstructTsNewTimes(path,
        "01Jan2000", "2400", "10Jan2000", "2400");
    status = ztsRetrieve(ifltab, back, 0, 2, 0);
    if (status != 0) { printf("ztsRetrieve failed: %d\n", status); return 1; }
    if (back->numberValues != N || back->doubleValues == NULL) {
        printf("expected %d values, got %d\n", N, back->numberValues);
        return 1;
    }
    for (i = 0; i < N; i++) {
        if (back->doubleValues[i] != values[i]) {
            printf("value %d: stored %g, read %g\n", i, values[i], back->doubleValues[i]);
            return 1;
        }
    }
    zstructFree(back);
    zclose(ifltab);

    if (zgetFileVersion(file) != 7) { printf("not a DSS-7 file\n"); return 1; }
    remove(file);
    printf("heclib round trip OK (%d values)\n", N);
    return 0;
}
