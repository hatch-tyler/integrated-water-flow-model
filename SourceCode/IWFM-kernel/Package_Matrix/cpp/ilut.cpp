/*
 *  Integrated Water Flow Model (IWFM)
 *  Copyright (C) 2005-2025
 *  State of California, Department of Water Resources
 *
 *  ILUT - Incomplete LU factorization with dual truncation strategy
 *
 *  INDEXING: All pointers offset by -1 for 1-based indexing, matching
 *  the Fortran original exactly. Output arrays (alu, jlu, ju) contain
 *  1-based index values, compatible with Fortran LUSOL and C++ iwfm_lusol.
 */

#include "iwfm_solver.h"
#include <cmath>

static void qsplit(double* a, int* ind, int n, int ncut);

extern "C" {

int iwfm_ilut(int n, const double* a_0, const int* ja_0, const int* ia_0,
              int lfil, double droptol,
              double* alu_0, int* jlu_0, int* ju_0, int iwk,
              double* w_0, int* jw_0)
{
    // 1-based pointers
    const double* a  = a_0  - 1;
    const int*    ja = ja_0 - 1;
    const int*    ia = ia_0 - 1;
    double*       alu = alu_0 - 1;
    int*          jlu = jlu_0 - 1;
    int*          ju  = ju_0  - 1;
    double*       w   = w_0   - 1;
    int*          jw  = jw_0  - 1;

    if (lfil < 0) return -4;

    int ju0 = n + 2;
    jlu[1] = ju0;

    // Initialize nonzero indicator array
    for (int i = n + 1; i <= 2 * n; ++i) {
        jw[i] = 0;
    }

    // Main loop
    for (int ii = 1; ii <= n; ++ii) {
        int j1 = ia[ii];
        int j2 = ia[ii + 1] - 1;

        double tnorm = 0.0;
        for (int k = j1; k <= j2; ++k) {
            tnorm += std::fabs(a[k]);
        }
        if (tnorm == 0.0) return -5 - ii;
        tnorm /= static_cast<double>(j2 - j1 + 1);

        int lenu = 1;
        int lenl = 0;
        jw[ii] = ii;
        w[ii] = 0.0;
        jw[n + ii] = ii;

        for (int j = j1; j <= j2; ++j) {
            int k = ja[j];
            double t = a[j];
            if (k < ii) {
                lenl++;
                jw[lenl] = k;
                w[lenl] = t;
                jw[n + k] = lenl;
            } else if (k == ii) {
                w[ii] = t;
            } else {
                lenu++;
                int jpos = ii + lenu - 1;
                jw[jpos] = k;
                w[jpos] = t;
                jw[n + k] = jpos;
            }
        }

        int jj = 0;
        int iLen = 0;

        // Eliminate previous rows
        while (true) {
            jj++;
            if (jj > lenl) break;

            int jrow = jw[jj];
            int k = jj;
            for (int j = jj + 1; j <= lenl; ++j) {
                if (jw[j] < jrow) {
                    jrow = jw[j];
                    k = j;
                }
            }

            if (k != jj) {
                int jtmp = jw[jj]; jw[jj] = jw[k]; jw[k] = jtmp;
                jw[n + jrow] = jj;
                jw[n + jtmp] = k;
                double stmp = w[jj]; w[jj] = w[k]; w[k] = stmp;
            }

            jw[n + jrow] = 0;

            double fact = w[jj] * alu[jrow];
            if (std::fabs(fact) <= droptol) continue;

            for (int k = ju[jrow]; k <= jlu[jrow + 1] - 1; ++k) {
                double s = fact * alu[k];
                int j = jlu[k];
                int jpos = jw[n + j];

                if (j >= ii) {
                    if (jpos == 0) {
                        lenu++;
                        if (lenu > n) return -1;
                        int idx = ii + lenu - 1;
                        jw[idx] = j;
                        jw[n + j] = idx;
                        w[idx] = -s;
                    } else {
                        w[jpos] -= s;
                    }
                } else {
                    if (jpos == 0) {
                        lenl++;
                        if (lenl > n) return -1;
                        jw[lenl] = j;
                        jw[n + j] = lenl;
                        w[lenl] = -s;
                    } else {
                        w[jpos] -= s;
                    }
                }
            }

            iLen++;
            w[iLen] = fact;
            jw[iLen] = jrow;
        }

        // Reset indicator (U-part)
        for (int k = 1; k <= lenu; ++k) {
            jw[n + jw[ii + k - 1]] = 0;
        }

        // Update L-matrix
        lenl = iLen;
        iLen = (lenl < lfil) ? lenl : lfil;

        qsplit(&w[1], &jw[1], lenl, iLen);

        for (int k = 1; k <= iLen; ++k) {
            if (ju0 > iwk) return -2;
            alu[ju0] = w[k];
            jlu[ju0] = jw[k];
            ju0++;
        }

        ju[ii] = ju0;

        // Update U-matrix
        iLen = 0;
        for (int k = 1; k <= lenu - 1; ++k) {
            if (std::fabs(w[ii + k]) > droptol * tnorm) {
                iLen++;
                w[ii + iLen] = w[ii + k];
                jw[ii + iLen] = jw[ii + k];
            }
        }
        lenu = iLen + 1;
        iLen = (lenu < lfil) ? lenu : lfil;

        qsplit(&w[ii + 1], &jw[ii + 1], lenu - 1, iLen);

        double t = std::fabs(w[ii]);
        if (iLen + ju0 > iwk) return -3;
        for (int k = ii + 1; k <= ii + iLen - 1; ++k) {
            jlu[ju0] = jw[k];
            alu[ju0] = w[k];
            t += std::fabs(w[k]);
            ju0++;
        }

        if (w[ii] == 0.0) w[ii] = (0.0001 + droptol) * tnorm;
        alu[ii] = 1.0 / w[ii];
        jlu[ii + 1] = ju0;
    }

    return 0;
}

} // extern "C"


/*
 * QSPLIT - Quick-sort partitioning (1-based arrays)
 *
 * NOTE: The caller passes &w[1] and &jw[1] (pointers to element [1]
 * of the 1-based arrays). Within qsplit, a[0] is the first element.
 * This matches the Fortran convention where CALL QSPLIT(w,jw,lenl,iLen)
 * passes w(1) as the first element.
 */
static void qsplit(double* a, int* ind, int n, int ncut)
{
    // a[0..n-1], ind[0..n-1] — 0-based within this function
    // because the caller already offset the pointer
    int first = 0;
    int last = n - 1;

    if (ncut < 1 || ncut > n) return;
    int ncut0 = ncut - 1;  // Convert to 0-based target

    while (true) {
        int mid = first;
        double abskey = std::fabs(a[mid]);

        for (int j = first + 1; j <= last; ++j) {
            if (std::fabs(a[j]) > abskey) {
                mid++;
                double tmp = a[mid]; a[mid] = a[j]; a[j] = tmp;
                int itmp = ind[mid]; ind[mid] = ind[j]; ind[j] = itmp;
            }
        }

        double tmp = a[mid]; a[mid] = a[first]; a[first] = tmp;
        int itmp = ind[mid]; ind[mid] = ind[first]; ind[first] = itmp;

        if (mid == ncut0) return;
        if (mid > ncut0) last = mid - 1;
        else first = mid + 1;
    }
}
