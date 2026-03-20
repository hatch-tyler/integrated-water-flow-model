/*
 *  Integrated Water Flow Model (IWFM)
 *  Copyright (C) 2005-2025
 *  State of California, Department of Water Resources
 *
 *  BLAS Level 1 routines: ddot and dnrm2
 *
 *  INDEXING: Fortran passes &array(1) which arrives as C pointer[0].
 *  We subtract 1 from the pointer so that p[1] = Fortran array(1),
 *  allowing identical 1-based indexing as the Fortran original.
 */

#include "iwfm_solver.h"
#include <cmath>

extern "C" {

double iwfm_ddot(int n, const double* dx_0, int incx,
                 const double* dy_0, int incy)
{
    // Create 1-based pointers
    const double* dx = dx_0 - 1;  // dx[1] = dx_0[0] = Fortran dx(1)
    const double* dy = dy_0 - 1;

    double dtemp = 0.0;
    if (n <= 0) return 0.0;

    if (incx == 1 && incy == 1) {
        int m = n % 5;
        for (int i = 1; i <= m; ++i) {
            dtemp += dx[i] * dy[i];
        }
        for (int i = m + 1; i <= n; i += 5) {
            dtemp += dx[i]     * dy[i]
                   + dx[i + 1] * dy[i + 1]
                   + dx[i + 2] * dy[i + 2]
                   + dx[i + 3] * dy[i + 3]
                   + dx[i + 4] * dy[i + 4];
        }
    } else {
        int ix = 1;
        int iy = 1;
        if (incx < 0) ix = (-n + 1) * incx + 1;
        if (incy < 0) iy = (-n + 1) * incy + 1;
        for (int i = 0; i < n; ++i) {
            dtemp += dx[ix] * dy[iy];
            ix += incx;
            iy += incy;
        }
    }
    return dtemp;
}


double iwfm_dnrm2(int n, const double* dx_0, int incx)
{
    const double* dx = dx_0 - 1;  // 1-based pointer
    const double cutlo = 8.232e-11;
    const double cuthi = 1.304e19;

    if (n <= 0) return 0.0;

    double sum = 0.0, xmax = 0.0;
    int nn = n * incx;

    enum { INIT, PHASE1, PHASE2, PHASE3, PHASE4 } phase = INIT;

    int i = 1;
    while (i <= nn) {
        double absi = std::fabs(dx[i]);

        switch (phase) {
        case INIT:
            if (absi > cutlo) goto phase3_entry;
            phase = PHASE1;
            xmax = 0.0;
            // fall through
        case PHASE1:
            if (dx[i] == 0.0) { i += incx; continue; }
            if (absi > cutlo) goto phase3_entry;
            xmax = absi;
            sum = 1.0;
            phase = PHASE2;
            i += incx;
            continue;
        case PHASE2:
            if (absi > cutlo) { sum = (sum * xmax) * xmax; goto phase3_entry; }
            if (absi > xmax) {
                sum = 1.0 + sum * (xmax / dx[i]) * (xmax / dx[i]);
                xmax = absi;
            } else if (xmax != 0.0) {
                sum += (dx[i] / xmax) * (dx[i] / xmax);
            }
            i += incx;
            continue;
        phase3_entry:
        case PHASE3:
            {
                double hitest = cuthi / static_cast<double>(n);
                for (int j = i; j <= nn; j += incx) {
                    if (std::fabs(dx[j]) >= hitest) {
                        sum = (sum / dx[j]) / dx[j];
                        xmax = std::fabs(dx[j]);
                        i = j + incx;
                        phase = PHASE4;
                        goto continue_outer;
                    }
                    sum += dx[j] * dx[j];
                }
                return std::sqrt(sum);
            }
        case PHASE4:
            if (absi > xmax) {
                sum = 1.0 + sum * (xmax / dx[i]) * (xmax / dx[i]);
                xmax = absi;
            } else if (xmax != 0.0) {
                sum += (dx[i] / xmax) * (dx[i] / xmax);
            }
            i += incx;
            continue;
        }
    continue_outer:;
    }
    return xmax * std::sqrt(sum);
}

} // extern "C"
