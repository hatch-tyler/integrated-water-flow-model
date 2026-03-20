/*
 *  Integrated Water Flow Model (IWFM)
 *  Copyright (C) 2005-2025
 *  State of California, Department of Water Resources
 *
 *  C++ Sparse Solver Library - C API Header
 *
 *  This library provides high-performance replacements for the Fortran
 *  SPARSKIT routines used in IWFM's Package_Matrix.
 *
 *  INDEXING CONVENTION:
 *  All functions accept standard C pointers (0-based). Internally, each
 *  function offsets the pointer by -1 to create "virtual 1-based" access
 *  (ptr[1] = first element). This allows the C++ code to use identical
 *  indexing as the Fortran original. When called from Fortran via BIND(C),
 *  Fortran passes &array(1) which arrives as C ptr[0] — the offset trick
 *  makes ptr[1] = array(1), giving seamless interop.
 *
 *  CRS index values (ia, ja, jlu, ju) stored within arrays use 1-based
 *  Fortran convention, matching the original SPARSKIT format.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 */

#ifndef IWFM_SOLVER_H
#define IWFM_SOLVER_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * ILUT - Incomplete LU factorization with dual truncation strategy
 *
 * Computes an incomplete LU factorization of a sparse matrix stored
 * in CRS format. Uses dual truncation: elements smaller than
 * droptol * (average absolute row value) are dropped, and at most
 * lfil elements are kept in each row of L and U.
 *
 * All arrays use 1-based indexing (Fortran convention).
 *
 * Parameters:
 *   n       - matrix dimension
 *   a       - non-zero values of input matrix (CRS, 1-based)
 *   ja      - column indices (CRS, 1-based)
 *   ia      - row pointers (CRS, 1-based, size n+1)
 *   lfil    - level of fill-in (typically 5-10)
 *   droptol - drop tolerance (typically 0.01)
 *   alu     - output LU values (size iwk)
 *   jlu     - output LU column indices (size iwk)
 *   ju      - pointer to start of U-part of each row (size n)
 *   iwk     - workspace size (typically n*(2*lfil+1))
 *   w       - work array (size n+1)
 *   jw      - integer work array (size 2*n)
 *
 * Returns: 0 on success, negative on error
 *   -1: input matrix has an incomprehensible error
 *   -2: insufficient storage in L
 *   -3: insufficient storage in U
 *   -4: illegal lfil value
 *   -(5+ii): zero row ii encountered
 */
int iwfm_ilut(int n, const double* a, const int* ja, const int* ia,
              int lfil, double droptol,
              double* alu, int* jlu, int* ju, int iwk,
              double* w, int* jw);

/*
 * GMRES(m) - Generalized Minimum Residual method with restart
 *
 * Reverse-communication implementation identical to SPARSKIT.
 * The caller must handle matrix-vector products and preconditioner
 * solves based on the return value in ipar[0].
 *
 * All arrays use 1-based indexing in the work array w.
 *
 * Parameters:
 *   n    - system dimension
 *   rhs  - right-hand side vector (size n)
 *   sol  - solution vector (size n, in/out)
 *   ipar - integer parameter array (size 16, 0-based in C)
 *   fpar - floating-point parameter array (size 16, 0-based in C)
 *   w    - work array (size >= (n+3)*(m+2) + (m+1)*m/2)
 *
 * ipar[0] return codes:
 *    1: need matrix-vector product   w[ipar[8]-1..] = A * w[ipar[7]-1..]
 *    3,5: need preconditioner solve  w[ipar[8]-1..] = M^{-1} * w[ipar[7]-1..]
 *    0: converged
 *   <0: error
 */
void iwfm_gmres(int n, double* rhs, double* sol,
                int* ipar, double* fpar, double* w);

/*
 * AMUX - Sparse matrix-vector multiply: y = A*x
 *
 * CRS format with 1-based indexing. OpenMP parallelized.
 *
 * Parameters:
 *   n  - matrix dimension
 *   x  - input vector (size n, 1-based: x[1]..x[n])
 *   y  - output vector (size n, 1-based: y[1]..y[n])
 *   a  - non-zero values (CRS, 1-based)
 *   ja - column indices (CRS, 1-based)
 *   ia - row pointers (CRS, 1-based, size n+1)
 */
void iwfm_amux(int n, const double* x, double* y,
               const double* a, const int* ja, const int* ia);

/*
 * LUSOL - Solve (LU)x = y using ILUT factors
 *
 * All arrays use 1-based indexing. In-place supported (x == y).
 *
 * Parameters:
 *   n   - system dimension
 *   y   - right-hand side (size n, 1-based)
 *   x   - solution (size n, 1-based)
 *   alu - LU values from ILUT
 *   jlu - LU column indices from ILUT
 *   ju  - U-part row pointers from ILUT (size n)
 */
void iwfm_lusol(int n, const double* y, double* x,
                const double* alu, const int* jlu, const int* ju);

/*
 * DNRM2 - Euclidean norm of a vector with stride
 *
 * Uses scaled accumulation to avoid overflow/underflow.
 *
 * Parameters:
 *   n    - number of elements
 *   x    - input vector (1-based: x[1]..x[n*incx])
 *   incx - stride
 *
 * Returns: ||x||_2
 */
double iwfm_dnrm2(int n, const double* x, int incx);

/*
 * DDOT - Dot product of two vectors with strides
 *
 * Parameters:
 *   n    - number of elements
 *   x    - first vector (1-based)
 *   incx - stride for x
 *   y    - second vector (1-based)
 *   incy - stride for y
 *
 * Returns: x . y
 */
double iwfm_ddot(int n, const double* x, int incx,
                 const double* y, int incy);

/*
 * PGMRES_SOLVE - Direct-call ILUT-preconditioned GMRES(m) solver
 *
 * Performs the complete ILUT factorization + GMRES(m) solve in a single
 * call, eliminating the reverse-communication overhead. Matvec and
 * preconditioner solves happen entirely within C++.
 *
 * Parameters:
 *   n          - system dimension
 *   rhs        - right-hand side vector (size n)
 *   sol        - solution vector (size n, in/out)
 *   coeff      - CRS non-zero values (1-based)
 *   jnd        - CRS column indices (1-based)
 *   njd        - CRS row pointers (1-based, size n+1)
 *   lfil       - ILUT fill level (typically 5)
 *   droptol    - ILUT drop tolerance (typically 0.01)
 *   im         - GMRES restart dimension (typically 20)
 *   maxiter    - maximum iterations
 *   rtol       - relative tolerance
 *   atol       - absolute tolerance
 *   iters_out  - [out] iterations performed
 *   residual_out - [out] final residual norm
 *
 * Returns:
 *    0: converged
 *   -1: max iterations reached
 *   -3: breakdown
 *   -5: ILUT bad matrix
 *   -6: ILUT insufficient storage
 *   -7: ILUT zero row
 */
int iwfm_pgmres_solve(int n, double* rhs, double* sol,
                       const double* coeff, const int* jnd, const int* njd,
                       int lfil, double droptol, int im, int maxiter,
                       double rtol, double atol,
                       int* iters_out, double* residual_out);

#ifdef __cplusplus
}
#endif

#endif /* IWFM_SOLVER_H */
