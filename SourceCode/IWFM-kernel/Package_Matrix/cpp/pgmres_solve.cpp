/*
 *  Integrated Water Flow Model (IWFM)
 *  Copyright (C) 2005-2025
 *  State of California, Department of Water Resources
 *
 *  PGMRES_SOLVE - Direct-call ILUT-preconditioned GMRES solver
 *
 *  This function performs the complete ILUT + GMRES(m) solve in a single
 *  C++ call, eliminating the reverse-communication overhead that requires
 *  hundreds of Fortran<->C++ boundary crossings per timestep. The matvec
 *  (AMUX) and preconditioner solve (LUSOL) are called internally.
 *
 *  INDEXING: All array pointers are offset by -1 internally, creating
 *  "virtual 1-based" pointers where p[1] is the first element.
 */

#include "iwfm_solver.h"
#include <cmath>
#include <cstring>
#include <vector>

extern "C" {

int iwfm_pgmres_solve(
    int n,
    double* rhs_0,
    double* sol_0,
    const double* coeff_0,
    const int* jnd_0,
    const int* njd_0,
    int lfil,
    double droptol,
    int im,
    int maxiter,
    double rtol,
    double atol,
    int* iters_out,
    double* residual_out)
{
    /*
     * Direct-call ILUT-preconditioned GMRES(m) solver.
     *
     * Parameters:
     *   n          - system dimension
     *   rhs_0      - right-hand side vector (size n)
     *   sol_0      - solution vector (size n, in/out, should be initialized)
     *   coeff_0    - CRS non-zero values (1-based indices in arrays)
     *   jnd_0      - CRS column indices (1-based)
     *   njd_0      - CRS row pointers (1-based, size n+1)
     *   lfil       - ILUT fill level (typically 5)
     *   droptol    - ILUT drop tolerance (typically 0.01)
     *   im         - GMRES restart dimension (typically 20)
     *   maxiter    - maximum iterations
     *   rtol       - relative tolerance
     *   atol       - absolute tolerance (machine epsilon)
     *   iters_out  - [out] iterations used
     *   residual_out - [out] final residual
     *
     * Returns:
     *    0: converged
     *   -1: did not converge within maxiter
     *   -2: insufficient workspace (shouldn't happen)
     *   -3: breakdown
     *   -5: ILUT error (bad matrix)
     *   -6: ILUT insufficient storage
     *   -7: ILUT zero row
     */

    // ------------------------------------------------------------------
    // Phase 1: ILUT preconditioner
    // ------------------------------------------------------------------
    int iwk = n * (2 * lfil + 1);
    std::vector<double> alu(iwk);
    std::vector<int> jlu(iwk);
    std::vector<int> ju(n);
    std::vector<double> w_ilut(n + 1);
    std::vector<int> jw_ilut(3 * n);

    int ierr = iwfm_ilut(n, coeff_0, jnd_0, njd_0, lfil, droptol,
                         alu.data(), jlu.data(), ju.data(), iwk,
                         w_ilut.data(), jw_ilut.data());

    if (ierr == -1) return -5;
    if (ierr == -2 || ierr == -3) return -6;
    if (ierr <= -5) return -7;

    // ------------------------------------------------------------------
    // Phase 2: GMRES(m) with left preconditioning
    // ------------------------------------------------------------------
    int ipar[16] = {};
    double fpar[16] = {};

    ipar[0] = 0;          // initialize solver
    ipar[1] = 1;          // left preconditioning
    ipar[2] = 2;          // stopping: relative to initial residual
    ipar[3] = (n + 3) * (im + 2) + (im + 1) * im / 2;  // workspace size
    ipar[4] = im;         // restart dimension
    ipar[5] = maxiter;    // max iterations

    fpar[0] = rtol;       // relative tolerance
    fpar[1] = atol;       // absolute tolerance

    std::vector<double> w(ipar[3]);

    int result = 0;
    int iter = 0;

    for (;;) {
        iwfm_gmres(n, rhs_0, sol_0, ipar, fpar, w.data());

        if (ipar[6] != iter) iter = ipar[6];

        if (ipar[0] == 1) {
            // Matvec: w[ipar[7]-1..] = A * w[ipar[8]-1..]
            // ipar[7] is 1-based source offset, ipar[8] is 1-based dest offset
            // w.data() is 0-based, so w.data() + (offset-1) gives the right pointer
            iwfm_amux(n, w.data() + (ipar[7] - 1), w.data() + (ipar[8] - 1),
                       coeff_0, jnd_0, njd_0);
        }
        else if (ipar[0] == 3 || ipar[0] == 5) {
            // Preconditioner solve
            iwfm_lusol(n, w.data() + (ipar[7] - 1), w.data() + (ipar[8] - 1),
                        alu.data(), jlu.data(), ju.data());
        }
        else if (ipar[0] == 0) {
            // Converged
            result = 0;
            break;
        }
        else if (ipar[0] < 0) {
            // Error
            if (ipar[0] == -1) result = -1;      // max iterations
            else if (ipar[0] == -2) result = -2;  // workspace
            else if (ipar[0] == -3) result = -3;  // breakdown
            else result = ipar[0];                 // other error
            break;
        }
    }

    *iters_out = iter;
    *residual_out = fpar[4];  // FPAR(5) in 1-based = fpar[4] in 0-based

    return result;
}

} // extern "C"
