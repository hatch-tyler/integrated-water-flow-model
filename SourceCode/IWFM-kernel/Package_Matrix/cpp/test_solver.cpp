/*
 *  Integrated Water Flow Model (IWFM)
 *  Copyright (C) 2005-2025
 *  State of California, Department of Water Resources
 *
 *  Standalone unit test for the C++ sparse solver library.
 *
 *  IMPORTANT: The C API functions expect Fortran-style arrays where
 *  the pointer points to element "0" (one before the first data element).
 *  In the tests below, we allocate arrays with index [0] unused and
 *  pass &array[0] to the C functions. Inside the C functions, they do
 *  ptr = arg - 1, so ptr[1] = arg[0], which is our first data element.
 *
 *  Wait — that's wrong. The C functions do ptr = arg - 1. If we pass
 *  &array[0], then ptr = &array[-1], and ptr[1] = array[0].
 *  So array[0] is the first element. This is the standard C convention.
 *
 *  Actually, rethinking: the C functions are designed to be called from
 *  Fortran, where Fortran passes &array(1). In C, that's &array[0]
 *  (since Fortran array(1) is stored at the base address). The C function
 *  then does ptr = arg - 1, so ptr[1] = arg[0] = Fortran array(1). ✓
 *
 *  For C tests, we just use regular 0-based arrays and pass them directly.
 *  Inside the C functions: ptr = arg - 1, so ptr[1] = arg[0]. This means
 *  our array[0] is treated as element 1 by the solver functions.
 *
 *  Build: g++ -O2 -std=c++17 -o test_solver test_solver.cpp blas.cpp
 *         spmv.cpp lusol.cpp ilut.cpp gmres.cpp
 */

#include "iwfm_solver.h"
#include <cstdio>
#include <cmath>
#include <vector>

// =========================================================================
// Test 1: DDOT and DNRM2
// =========================================================================
static bool test_blas()
{
    printf("Test BLAS (ddot, dnrm2)... ");

    // 0-based C array: dx[0..4]
    double dx[] = {1.0, 2.0, 3.0, 4.0, 5.0};
    double dy[] = {2.0, 3.0, 4.0, 5.0, 6.0};

    // iwfm_ddot internally does ptr = arg - 1, then ptr[1..n]
    // So ptr[1] = arg[0] = 1.0, ptr[2] = arg[1] = 2.0, etc. ✓
    double dot = iwfm_ddot(5, dx, 1, dy, 1);
    // Expected: 1*2 + 2*3 + 3*4 + 4*5 + 5*6 = 70
    if (std::fabs(dot - 70.0) > 1e-12) {
        printf("FAILED (ddot = %g, expected 70)\n", dot);
        return false;
    }

    double nrm = iwfm_dnrm2(5, dx, 1);
    // Expected: sqrt(1+4+9+16+25) = sqrt(55)
    if (std::fabs(nrm - std::sqrt(55.0)) > 1e-12) {
        printf("FAILED (dnrm2 = %g, expected %g)\n", nrm, std::sqrt(55.0));
        return false;
    }

    printf("PASSED\n");
    return true;
}

// =========================================================================
// Test 2: AMUX (SpMV)
// =========================================================================
static bool test_amux()
{
    printf("Test AMUX (SpMV)... ");

    /*
     * 4x4 tridiagonal:
     *  [ 2 -1  0  0]
     *  [-1  2 -1  0]
     *  [ 0 -1  2 -1]
     *  [ 0  0 -1  2]
     *
     * CRS (1-based Fortran convention, stored in 0-based C arrays):
     * Row 1: a={2,-1}, ja={1,2}, ia(1)=1
     * Row 2: a={-1,2,-1}, ja={1,2,3}, ia(2)=3
     * Row 3: a={-1,2,-1}, ja={2,3,4}, ia(3)=6
     * Row 4: a={-1,2}, ja={3,4}, ia(4)=9
     * ia(5)=11
     *
     * In C 0-based arrays:
     * a[0..9]:  2,-1, -1,2,-1, -1,2,-1, -1,2
     * ja[0..9]: 1,2,   1,2,3,   2,3,4,   3,4  (1-based column indices!)
     * ia[0..4]: 1,3,6,9,11  (1-based row pointers!)
     */
    int n = 4;
    double a[]  = {2.0, -1.0, -1.0, 2.0, -1.0, -1.0, 2.0, -1.0, -1.0, 2.0};
    int ja[]    = {1, 2, 1, 2, 3, 2, 3, 4, 3, 4};
    int ia[]    = {1, 3, 6, 9, 11};

    double x[] = {1.0, 2.0, 3.0, 4.0};
    double y[4] = {};

    iwfm_amux(n, x, y, a, ja, ia);

    // Expected: y = A*x = [0, 0, 0, 5]
    double expected[] = {0.0, 0.0, 0.0, 5.0};
    for (int i = 0; i < n; ++i) {
        if (std::fabs(y[i] - expected[i]) > 1e-12) {
            printf("FAILED (y[%d] = %g, expected %g)\n", i, y[i], expected[i]);
            return false;
        }
    }

    printf("PASSED\n");
    return true;
}

// =========================================================================
// Test 3: ILUT + LUSOL
// =========================================================================
static bool test_ilut_lusol()
{
    printf("Test ILUT + LUSOL... ");

    int n = 4;
    double a[]  = {2.0, -1.0, -1.0, 2.0, -1.0, -1.0, 2.0, -1.0, -1.0, 2.0};
    int ja[]    = {1, 2, 1, 2, 3, 2, 3, 4, 3, 4};
    int ia[]    = {1, 3, 6, 9, 11};

    int lfil = 5;
    double droptol = 0.01;
    int iwk = n * (2 * lfil + 1);

    std::vector<double> alu(iwk, 0.0);
    std::vector<int> jlu(iwk, 0);
    std::vector<int> ju(n, 0);
    std::vector<double> w(n + 1, 0.0);
    std::vector<int> jw(2 * n, 0);

    int ierr = iwfm_ilut(n, a, ja, ia, lfil, droptol,
                         alu.data(), jlu.data(), ju.data(), iwk,
                         w.data(), jw.data());

    if (ierr != 0) {
        printf("FAILED (ILUT returned %d)\n", ierr);
        return false;
    }

    // Solve (LU)x = b where b = [1, 1, 1, 1]
    double b[] = {1.0, 1.0, 1.0, 1.0};
    double x[4] = {};

    iwfm_lusol(n, b, x, alu.data(), jlu.data(), ju.data());

    // Verify x is not all zeros
    bool all_zero = true;
    for (int i = 0; i < n; ++i) {
        if (std::fabs(x[i]) > 1e-15) all_zero = false;
    }
    if (all_zero) {
        printf("FAILED (LUSOL returned all zeros)\n");
        return false;
    }

    // Verify A*(LU^{-1}*b) ≈ b by checking LU*x = b
    // Since ILUT with lfil=5 on a 4x4 matrix gives exact LU,
    // x should be the exact solution of Ax = b
    // Verify by computing A*x and comparing to b
    double Ax[4] = {};
    iwfm_amux(n, x, Ax, a, ja, ia);
    double max_err = 0.0;
    for (int i = 0; i < n; ++i) {
        double err = std::fabs(Ax[i] - b[i]);
        if (err > max_err) max_err = err;
    }
    if (max_err > 1e-10) {
        printf("FAILED (A*x != b, max error = %e)\n", max_err);
        return false;
    }

    printf("PASSED (A*x = b, max error = %e)\n", max_err);
    return true;
}

// =========================================================================
// Test 4: Full ILUT-preconditioned GMRES solve
// =========================================================================
static bool test_gmres_solve()
{
    printf("Test GMRES solve (4x4 tridiagonal)... ");

    int n = 4;
    double a[]  = {2.0, -1.0, -1.0, 2.0, -1.0, -1.0, 2.0, -1.0, -1.0, 2.0};
    int ja[]    = {1, 2, 1, 2, 3, 2, 3, 4, 3, 4};
    int ia[]    = {1, 3, 6, 9, 11};

    // Known solution: x = [1, 2, 3, 4]
    // RHS = A * x_true = [0, 0, 0, 5]
    double rhs[] = {0.0, 0.0, 0.0, 5.0};
    double sol[] = {0.0, 0.0, 0.0, 0.0};

    // ILUT
    int lfil = 5;
    double droptol = 0.01;
    int iwk = n * (2 * lfil + 1);

    std::vector<double> alu(iwk, 0.0);
    std::vector<int> jlu(iwk, 0);
    std::vector<int> ju(n, 0);
    std::vector<double> w_ilut(n + 1, 0.0);
    std::vector<int> jw(2 * n, 0);

    int ierr = iwfm_ilut(n, a, ja, ia, lfil, droptol,
                         alu.data(), jlu.data(), ju.data(), iwk,
                         w_ilut.data(), jw.data());
    if (ierr != 0) {
        printf("FAILED (ILUT returned %d)\n", ierr);
        return false;
    }

    // GMRES parameters
    int im = 20;
    int ipar[16] = {};
    double fpar[16] = {};

    ipar[0] = 0;       // ipar(1) = 0: initialize
    ipar[1] = 1;       // ipar(2) = 1: left preconditioning
    ipar[2] = 2;       // ipar(3) = 2: residual-based stopping
    ipar[3] = (n + 3) * (im + 2) + (im + 1) * im / 2;  // ipar(4): workspace
    ipar[4] = im;      // ipar(5): restart dimension
    ipar[5] = 100;     // ipar(6): max iterations

    fpar[0] = 1.0e-10; // fpar(1): relative tolerance
    fpar[1] = 1.0e-16; // fpar(2): machine epsilon

    std::vector<double> w(ipar[3], 0.0);

    int iter = 0;
    bool converged = false;

    for (int loop = 0; loop < 1000; ++loop) {
        iwfm_gmres(n, rhs, sol, ipar, fpar, w.data());

        if (ipar[6] != iter) iter = ipar[6];  // ipar(7): iteration count

        if (ipar[0] == 1) {
            // Matrix-vector multiply: w(ipar(9)) = A * w(ipar(8))
            // ipar[7] = ipar(8), ipar[8] = ipar(9) in 0-based
            // Fortran would do: CALL AMUX(N, W(IPAR(8):), W(IPAR(9):), ...)
            // IPAR(8) is a 1-based offset into W. In C: w_0[IPAR(8)-1]
            int i8 = ipar[7] - 1;  // 0-based offset (ipar(8) is 1-based)
            int i9 = ipar[8] - 1;
            iwfm_amux(n, &w[i8], &w[i9], a, ja, ia);
        } else if (ipar[0] == 3 || ipar[0] == 5) {
            int i8 = ipar[7] - 1;
            int i9 = ipar[8] - 1;
            iwfm_lusol(n, &w[i8], &w[i9],
                       alu.data(), jlu.data(), ju.data());
        } else if (ipar[0] == 0) {
            converged = true;
            break;
        } else if (ipar[0] < 0) {
            printf("FAILED (GMRES error code %d)\n", ipar[0]);
            return false;
        }
    }

    if (!converged) {
        printf("FAILED (did not converge)\n");
        return false;
    }

    double x_true[] = {1.0, 2.0, 3.0, 4.0};
    double max_err = 0.0;
    for (int i = 0; i < n; ++i) {
        double err = std::fabs(sol[i] - x_true[i]);
        if (err > max_err) max_err = err;
    }

    if (max_err > 1e-8) {
        printf("FAILED (max error = %e)\n", max_err);
        printf("  Solution: [%g, %g, %g, %g]\n", sol[0], sol[1], sol[2], sol[3]);
        return false;
    }

    printf("PASSED (max error = %e, %d iterations)\n", max_err, iter);
    return true;
}

// =========================================================================
// Test 5: 10x10 system
// =========================================================================
static bool test_gmres_large()
{
    printf("Test GMRES (10x10 tridiagonal)... ");

    int n = 10;

    // Build CRS tridiagonal: 2 on diagonal, -1 on off-diagonals
    std::vector<double> a;
    std::vector<int> ja;
    std::vector<int> ia(n + 1);

    int nnz_idx = 1;  // 1-based pointer value for ia
    for (int i = 1; i <= n; ++i) {
        ia[i - 1] = nnz_idx;
        if (i > 1) { a.push_back(-1.0); ja.push_back(i - 1); nnz_idx++; }
        a.push_back(2.0); ja.push_back(i); nnz_idx++;
        if (i < n) { a.push_back(-1.0); ja.push_back(i + 1); nnz_idx++; }
    }
    ia[n] = nnz_idx;

    // Known solution and RHS
    std::vector<double> x_true(n), rhs_v(n, 0.0);
    for (int i = 0; i < n; ++i) x_true[i] = static_cast<double>(i + 1);
    iwfm_amux(n, x_true.data(), rhs_v.data(), a.data(), ja.data(), ia.data());

    // ILUT
    int lfil = 5;
    double droptol = 0.01;
    int iwk = n * (2 * lfil + 1);

    std::vector<double> alu(iwk, 0.0);
    std::vector<int> jlu(iwk, 0);
    std::vector<int> ju(n, 0);
    std::vector<double> w_ilut(n + 1, 0.0);
    std::vector<int> jw(2 * n, 0);

    int ierr = iwfm_ilut(n, a.data(), ja.data(), ia.data(), lfil, droptol,
                         alu.data(), jlu.data(), ju.data(), iwk,
                         w_ilut.data(), jw.data());
    if (ierr != 0) {
        printf("FAILED (ILUT returned %d)\n", ierr);
        return false;
    }

    // GMRES
    int im = 20;
    int ipar[16] = {};
    double fpar[16] = {};
    ipar[0] = 0; ipar[1] = 1; ipar[2] = 2;
    ipar[3] = (n + 3) * (im + 2) + (im + 1) * im / 2;
    ipar[4] = im; ipar[5] = 200;
    fpar[0] = 1.0e-12; fpar[1] = 1.0e-16;

    std::vector<double> w(ipar[3], 0.0);
    std::vector<double> sol(n, 0.0);

    bool converged = false;
    int iter = 0;
    for (int loop = 0; loop < 2000; ++loop) {
        iwfm_gmres(n, rhs_v.data(), sol.data(), ipar, fpar, w.data());
        if (ipar[6] != iter) iter = ipar[6];

        if (ipar[0] == 1) {
            iwfm_amux(n, &w[ipar[7] - 1], &w[ipar[8] - 1],
                      a.data(), ja.data(), ia.data());
        } else if (ipar[0] == 3 || ipar[0] == 5) {
            iwfm_lusol(n, &w[ipar[7] - 1], &w[ipar[8] - 1],
                       alu.data(), jlu.data(), ju.data());
        } else if (ipar[0] == 0) {
            converged = true; break;
        } else if (ipar[0] < 0) {
            printf("FAILED (GMRES error %d)\n", ipar[0]);
            return false;
        }
    }

    if (!converged) { printf("FAILED (did not converge)\n"); return false; }

    double max_err = 0.0;
    for (int i = 0; i < n; ++i) {
        double err = std::fabs(sol[i] - x_true[i]);
        if (err > max_err) max_err = err;
    }

    if (max_err > 1e-8) {
        printf("FAILED (max error = %e)\n", max_err);
        return false;
    }

    printf("PASSED (max error = %e, %d iterations)\n", max_err, iter);
    return true;
}

// =========================================================================
int main()
{
    printf("=== IWFM C++ Solver Unit Tests ===\n\n");

    int passed = 0, failed = 0;

    if (test_blas())       ++passed; else ++failed;
    if (test_amux())       ++passed; else ++failed;
    if (test_ilut_lusol()) ++passed; else ++failed;
    if (test_gmres_solve()) ++passed; else ++failed;
    if (test_gmres_large()) ++passed; else ++failed;

    printf("\n%d passed, %d failed\n", passed, failed);
    return failed == 0 ? 0 : 1;
}
