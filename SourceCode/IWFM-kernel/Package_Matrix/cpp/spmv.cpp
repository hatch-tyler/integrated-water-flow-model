/*
 *  Integrated Water Flow Model (IWFM)
 *  Copyright (C) 2005-2025
 *  State of California, Department of Water Resources
 *
 *  Sparse matrix-vector multiply: y = A*x (CRS format, OpenMP)
 *
 *  INDEXING: All pointers are offset by -1 internally so that
 *  p[1] maps to the first element passed from Fortran. This allows
 *  the C++ code to use identical 1-based indexing as the Fortran original.
 */

#include "iwfm_solver.h"

#ifdef _OPENMP
#include <omp.h>
#endif

extern "C" {

void iwfm_amux(int n, const double* x_0, double* y_0,
               const double* a_0, const int* ja_0, const int* ia_0)
{
    // Create 1-based pointers
    const double* x  = x_0  - 1;
    double*       y  = y_0  - 1;
    const double* a  = a_0  - 1;
    const int*    ja = ja_0 - 1;
    const int*    ia = ia_0 - 1;

    #pragma omp parallel for schedule(static, 128) default(shared)
    for (int i = 1; i <= n; ++i) {
        double t = 0.0;
        #if !defined(_MSC_VER) || defined(__INTEL_COMPILER)
        #pragma omp simd reduction(+:t)
        #endif
        for (int k = ia[i]; k <= ia[i + 1] - 1; ++k) {
            t += a[k] * x[ja[k]];
        }
        y[i] = t;
    }
}

} // extern "C"
