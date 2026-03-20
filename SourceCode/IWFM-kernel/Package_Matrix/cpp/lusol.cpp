/*
 *  Integrated Water Flow Model (IWFM)
 *  Copyright (C) 2005-2025
 *  State of California, Department of Water Resources
 *
 *  LUSOL - Solve (LU)x = y using ILUT factors
 *
 *  INDEXING: Pointers offset by -1 for 1-based indexing.
 *  In-place supported: iwfm_lusol(n, x_ptr, x_ptr, alu, jlu, ju)
 */

#include "iwfm_solver.h"

extern "C" {

void iwfm_lusol(int n, const double* y_0, double* x_0,
                const double* alu_0, const int* jlu_0, const int* ju_0)
{
    // 1-based pointers
    const double* y   = y_0   - 1;
    double*       x   = x_0   - 1;
    const double* alu = alu_0 - 1;
    const int*    jlu = jlu_0 - 1;
    const int*    ju  = ju_0  - 1;

    // Forward solve (L)
    for (int i = 1; i <= n; ++i) {
        x[i] = y[i];
        for (int k = jlu[i]; k <= ju[i] - 1; ++k) {
            x[i] -= alu[k] * x[jlu[k]];
        }
    }

    // Backward solve (U)
    for (int i = n; i >= 1; --i) {
        for (int k = ju[i]; k <= jlu[i + 1] - 1; ++k) {
            x[i] -= alu[k] * x[jlu[k]];
        }
        x[i] = alu[i] * x[i];
    }
}

} // extern "C"
