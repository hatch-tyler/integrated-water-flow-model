/*
 *  Integrated Water Flow Model (IWFM)
 *  Copyright (C) 2005-2025
 *  State of California, Department of Water Resources
 *
 *  GMRES(m) - Generalized Minimum Residual method with restart
 *
 *  INDEXING: All array pointers are offset by -1 internally, creating
 *  "virtual 1-based" pointers where p[1] is the first element.
 *  IPAR/FPAR use Fortran 1-based convention: IPAR(k) = ipar_0[k-1].
 *  IPAR(8)/IPAR(9) return 1-based offsets into the work array,
 *  compatible with Fortran W(IPAR(8):) slicing.
 *
 *  NOT thread-safe (static local state for reverse communication).
 */

#include "iwfm_solver.h"
#include <cmath>

// Internal helpers (forward declarations)
static double distdot_1(int n, const double* x, int ix, const double* y, int iy);
static void givens_1(double& x, double& y, double& c, double& s);
static void bisinit_1(int* ipar, double* fpar, int wksize, int dsc,
                      bool& lp, bool& rp, double* wk);
static void mgsro_1(bool full, int lda, int n, int m, int ind, double& ops,
                    double* vec, double* hh, int& ierr);

extern "C" {

void iwfm_gmres(int n, double* rhs_0, double* sol_0,
                int* ipar_0, double* fpar_0, double* w_0)
{
    // Create 1-based pointers
    double* rhs  = rhs_0  - 1;
    double* sol  = sol_0  - 1;
    double* w    = w_0    - 1;

    // IPAR/FPAR: Fortran ipar(1)..ipar(16) → C ipar_0[0]..ipar_0[15]
    // Macro: IPAR(k) accesses Fortran ipar(k) = C ipar_0[k-1]
    #define IPAR(j) ipar_0[(j)-1]
    #define FPAR(j) fpar_0[(j)-1]

    // Static local variables (SAVE in Fortran)
    static int i_s, ii_s, idx_s, k_s, m_s, ptr_s, p2_s, hess_s, vc_s, vs_s, vrn_s;
    static double alpha_s, c_s, s_s;
    static bool lp_s, rp_s;

    const double one = 1.0, zero = 0.0;

    if (IPAR(1) <= 0) IPAR(10) = 0;

    switch (IPAR(10)) {
    case 1: goto L10;
    case 2: goto L20;
    case 3: goto L30;
    case 4: goto L40;
    case 5: goto L50;
    case 6: goto L60;
    case 7: goto L70;
    default: break;
    }

    // Initialization
    m_s = (IPAR(5) <= 1) ? 15 : IPAR(5);
    idx_s  = n * (m_s + 1);
    hess_s = idx_s + n;
    vc_s   = hess_s + (m_s + 1) * m_s / 2 + 1;
    vs_s   = vc_s + m_s;
    vrn_s  = vs_s + m_s;
    i_s    = vrn_s + m_s + 1;

    bisinit_1(ipar_0, fpar_0, i_s, 1, lp_s, rp_s, w);
    if (IPAR(1) < 0) return;

    // Request initial matrix-vector product A*x
L100:
    IPAR(1) = 1;
    IPAR(8) = n + 1;
    IPAR(9) = 1;
    IPAR(10) = 1;
    k_s = 0;
    for (int i = 1; i <= n; ++i) w[n + i] = sol[i];
    return;

L10:
    IPAR(7)  = IPAR(7) + 1;
    IPAR(13) = IPAR(13) + 1;
    if (lp_s) {
        for (int i = 1; i <= n; ++i) w[n + i] = rhs[i] - w[i];
        IPAR(1) = 3; IPAR(10) = 2;
        return;
    } else {
        for (int i = 1; i <= n; ++i) w[i] = rhs[i] - w[i];
    }
    FPAR(11) = FPAR(11) + n;

L20:
    alpha_s = std::sqrt(distdot_1(n, w, 1, w, 1));
    FPAR(11) = FPAR(11) + 2 * n;
    if (IPAR(7) == 1 && IPAR(3) != 999) {
        if (std::abs(IPAR(3)) == 2) {
            FPAR(4) = FPAR(1) * std::sqrt(distdot_1(n, rhs, 1, rhs, 1)) + FPAR(2);
            FPAR(11) = FPAR(11) + 2 * n;
        } else {
            FPAR(4) = FPAR(1) * alpha_s + FPAR(2);
        }
        FPAR(3) = alpha_s;
    }
    FPAR(5) = alpha_s;
    w[vrn_s + 1] = alpha_s;
    if (alpha_s <= FPAR(4) && IPAR(3) >= 0 && IPAR(3) != 999) {
        IPAR(1) = 0; FPAR(6) = alpha_s; goto L300;
    }
    alpha_s = one / alpha_s;
    for (int ii = 1; ii <= n; ++ii) w[ii] = alpha_s * w[ii];
    FPAR(11) = FPAR(11) + n;

L110:
    k_s++;
    if (rp_s) {
        IPAR(1) = 5;
        IPAR(8) = k_s * n - n + 1;
        IPAR(9) = lp_s ? (k_s * n + 1) : (idx_s + 1);
        IPAR(10) = 3;
        return;
    }

L30:
    IPAR(1) = 1;
    IPAR(8) = rp_s ? IPAR(9) : ((k_s - 1) * n + 1);
    IPAR(9) = lp_s ? (idx_s + 1) : (1 + k_s * n);
    IPAR(10) = 4;
    return;

L40:
    if (lp_s) {
        IPAR(1) = 3; IPAR(8) = IPAR(9); IPAR(9) = k_s * n + 1; IPAR(10) = 5;
        return;
    }

    // Modified Gram-Schmidt
L50:
    IPAR(7) = IPAR(7) + 1;
    ptr_s = k_s * (k_s - 1) / 2 + hess_s;
    p2_s = IPAR(9);
    mgsro_1(false, n, n, k_s + 1, k_s + 1, FPAR(11), w, &w[ptr_s], IPAR(12));
    if (IPAR(12) < 0) goto L200;

    // Apply previous Givens rotations
    p2_s = ptr_s + 1;
    for (int i = 1; i <= k_s - 1; ++i) {
        ptr_s = p2_s; p2_s++;
        alpha_s = w[ptr_s];
        c_s = w[vc_s + i]; s_s = w[vs_s + i];
        w[ptr_s] = c_s * alpha_s + s_s * w[p2_s];
        w[p2_s]  = c_s * w[p2_s] - s_s * alpha_s;
    }
    givens_1(w[p2_s], w[p2_s + 1], c_s, s_s);
    w[vc_s + k_s] = c_s;
    w[vs_s + k_s] = s_s;
    p2_s = vrn_s + k_s;
    alpha_s = -s_s * w[p2_s];
    w[p2_s] = c_s * w[p2_s];
    w[p2_s + 1] = alpha_s;

    FPAR(11) = FPAR(11) + 6 * k_s + 2;
    alpha_s = std::fabs(alpha_s);
    FPAR(5) = alpha_s;
    if (k_s < m_s && !(IPAR(3) >= 0 && alpha_s <= FPAR(4))
        && (IPAR(6) <= 0 || IPAR(7) < IPAR(6)))
        goto L110;

    // Back-solve upper triangular system
L200:
    ptr_s = hess_s + k_s * (k_s + 1) / 2;
    p2_s = vrn_s + k_s;
    if (w[ptr_s] == zero) {
        k_s--;
        if (k_s > 0) goto L200;
        IPAR(1) = -3; IPAR(12) = -4; goto L300;
    }
    w[p2_s] = w[p2_s] / w[ptr_s];
    for (int i = k_s - 1; i >= 1; --i) {
        ptr_s -= i + 1;
        for (int ii = 1; ii <= i; ++ii)
            w[vrn_s + ii] -= w[p2_s] * w[ptr_s + ii];
        p2_s--;
        w[p2_s] = w[p2_s] / w[ptr_s];
    }

    for (int ii = 1; ii <= n; ++ii) w[ii] *= w[p2_s];
    for (int i = 1; i <= k_s - 1; ++i) {
        ptr_s = i * n;
        p2_s++;
        for (int ii = 1; ii <= n; ++ii)
            w[ii] += w[p2_s] * w[ptr_s + ii];
    }
    FPAR(11) = FPAR(11) + 2 * k_s * n - n + k_s * (k_s + 1);

    if (rp_s) {
        IPAR(1) = 5; IPAR(8) = 1; IPAR(9) = idx_s + 1; IPAR(10) = 6;
        return;
    }

L60:
    if (rp_s) {
        for (int i = 1; i <= n; ++i) sol[i] += w[idx_s + i];
    } else {
        for (int i = 1; i <= n; ++i) sol[i] += w[i];
    }
    FPAR(11) = FPAR(11) + n;

    if (IPAR(3) == 999) {
        IPAR(1) = 10; IPAR(8) = -1; IPAR(9) = idx_s + 1; IPAR(10) = 7;
        return;
    } else if (IPAR(3) < 0) {
        if (IPAR(7) <= m_s + 1) {
            FPAR(3) = std::fabs(w[vrn_s + 1]);
            if (IPAR(3) == -1) FPAR(4) = FPAR(1) * FPAR(3) + FPAR(2);
        }
        FPAR(6) = std::fabs(w[vrn_s + k_s]);
    } else {
        FPAR(6) = FPAR(5);
    }

L70:
    if (IPAR(12) != 0) { IPAR(1) = -3; goto L300; }
    if ((IPAR(7) < IPAR(6) || IPAR(6) <= 0) &&
        ((IPAR(3) == 999 && IPAR(11) == 0) ||
         (IPAR(3) != 999 && FPAR(6) > FPAR(4))))
        goto L100;

    if (IPAR(1) > 0) {
        if (IPAR(3) == 999 && IPAR(11) == 1)        IPAR(1) = 0;
        else if (IPAR(3) != 999 && FPAR(6) <= FPAR(4)) IPAR(1) = 0;
        else if (IPAR(7) >= IPAR(6) && IPAR(6) > 0) IPAR(1) = -1;
        else                                          IPAR(1) = -10;
    }

L300:
    if (FPAR(3) != zero && FPAR(6) != zero && IPAR(7) > IPAR(13))
        FPAR(7) = std::log10(FPAR(3) / FPAR(6)) / static_cast<double>(IPAR(7) - IPAR(13));
    else
        FPAR(7) = zero;

    #undef IPAR
    #undef FPAR
}

} // extern "C"


// =========================================================================
// Internal helpers — all use 1-based pointers (already offset by caller)
// =========================================================================

static double distdot_1(int n, const double* x, int ix, const double* y, int iy)
{
    // x and y are already 1-based (offset by -1 in caller)
    // iwfm_ddot expects 0-based pointers, so pass x+1 and y+1
    return iwfm_ddot(n, x + 1, ix, y + 1, iy);
}

static void givens_1(double& x, double& y, double& c, double& s)
{
    const double one = 1.0, zero = 0.0;
    if (x == zero && y == zero) { c = one; s = zero; }
    else if (std::fabs(y) > std::fabs(x)) {
        double t = x / y;
        x = std::sqrt(one + t * t);
        s = (y >= 0.0 ? one / x : -one / x);
        c = t * s;
    } else if (std::fabs(y) <= std::fabs(x)) {
        double t = y / x;
        y = std::sqrt(one + t * t);
        c = (x >= 0.0 ? one / y : -one / y);
        s = t * c;
    } else {
        x = zero; y = zero; c = one; s = zero;
    }
    x = std::fabs(x * y);
}

static void bisinit_1(int* ipar_0, double* fpar_0, int wksize, int dsc,
                       bool& lp, bool& rp, double* wk)
{
    // wk is already a 1-based pointer (offset by -1)
    #define IPAR(j) ipar_0[(j)-1]
    #define FPAR(j) fpar_0[(j)-1]
    const double zero = 0.0, one = 1.0;

    if (IPAR(4) < wksize) { IPAR(1) = -2; IPAR(4) = wksize; return; }

    if      (IPAR(2) > 2) { lp = true;  rp = true;  }
    else if (IPAR(2) == 2) { lp = false; rp = true;  }
    else if (IPAR(2) == 1) { lp = true;  rp = false; }
    else                    { lp = false; rp = false; }
    if (IPAR(3) == 0) IPAR(3) = dsc;

    for (int i = 7; i <= 13; ++i) IPAR(i) = 0;

    if (FPAR(1) < zero || FPAR(1) >= one || FPAR(2) < zero ||
        (FPAR(1) == zero && FPAR(2) == zero)) {
        if (IPAR(1) == 0) { IPAR(1) = -4; return; }
        FPAR(1) = 1.0e-6; FPAR(2) = 1.0e-16;
    }

    for (int i = 3; i <= 10; ++i) FPAR(i) = zero;
    if (FPAR(11) < zero) FPAR(11) = zero;
    for (int i = 1; i <= wksize; ++i) wk[i] = zero;  // 1-based

    #undef IPAR
    #undef FPAR
}

static void mgsro_1(bool full, int lda, int n_vec, int m, int ind, double& ops,
                     double* vec, double* hh, int& ierr)
{
    /*
     * vec is a 1-based pointer to the work array w.
     * vec(k, col) in Fortran = vec[(col-1)*lda + k] with 1-based vec.
     * hh is a 1-based pointer: hh[1]..hh[m].
     *
     * Note: hh is passed as &w[ptr_s] from the caller, where w is
     * already 1-based. So hh[1] = w[ptr_s+1].
     */
    const double zero = 0.0, one = 1.0, reorth = 0.98;

    // Compute norm of column 'ind'
    double nrm0 = 0.0;
    for (int k = 1; k <= n_vec; ++k) {
        double v = vec[(ind - 1) * lda + k];
        nrm0 += v * v;
    }
    ops += 2.0 * n_vec;
    double thr = nrm0 * reorth;

    if (nrm0 <= zero) { ierr = -1; return; }
    if (nrm0 > zero && one / nrm0 > zero) ierr = 0;
    else { ierr = -2; return; }

    // Full orthogonalization (ind+1..m)
    if (full) {
        for (int i = ind + 1; i <= m; ++i) {
            double fct = 0.0;
            for (int k = 1; k <= n_vec; ++k)
                fct += vec[(ind - 1) * lda + k] * vec[(i - 1) * lda + k];
            hh[i] = fct;
            for (int k = 1; k <= n_vec; ++k)
                vec[(ind - 1) * lda + k] -= fct * vec[(i - 1) * lda + k];
            ops += 4.0 * n_vec + 2;
            if (fct * fct > thr) {
                double fct2 = 0.0;
                for (int k = 1; k <= n_vec; ++k)
                    fct2 += vec[(ind - 1) * lda + k] * vec[(i - 1) * lda + k];
                hh[i] += fct2;
                for (int k = 1; k <= n_vec; ++k)
                    vec[(ind - 1) * lda + k] -= fct2 * vec[(i - 1) * lda + k];
                ops += 4.0 * n_vec + 1;
            }
            nrm0 -= hh[i] * hh[i];
            if (nrm0 < zero) nrm0 = zero;
            thr = nrm0 * reorth;
        }
    }

    // Orthogonalize against columns 1..ind-1
    for (int i = 1; i <= ind - 1; ++i) {
        double fct = 0.0;
        for (int k = 1; k <= n_vec; ++k)
            fct += vec[(ind - 1) * lda + k] * vec[(i - 1) * lda + k];
        hh[i] = fct;
        for (int k = 1; k <= n_vec; ++k)
            vec[(ind - 1) * lda + k] -= fct * vec[(i - 1) * lda + k];
        ops += 4.0 * n_vec + 2;
        if (fct * fct > thr) {
            double fct2 = 0.0;
            for (int k = 1; k <= n_vec; ++k)
                fct2 += vec[(ind - 1) * lda + k] * vec[(i - 1) * lda + k];
            hh[i] += fct2;
            for (int k = 1; k <= n_vec; ++k)
                vec[(ind - 1) * lda + k] -= fct2 * vec[(i - 1) * lda + k];
            ops += 4.0 * n_vec + 1;
        }
        nrm0 -= hh[i] * hh[i];
        if (nrm0 < zero) nrm0 = zero;
        thr = nrm0 * reorth;
    }

    // Test and scale
    double nrm1 = 0.0;
    for (int k = 1; k <= n_vec; ++k) {
        double v = vec[(ind - 1) * lda + k];
        nrm1 += v * v;
    }
    nrm1 = std::sqrt(nrm1);
    ops += 2.0 * n_vec;
    hh[ind] = nrm1;

    if (nrm1 <= zero) { ierr = -3; return; }

    double fct = one / nrm1;
    for (int k = 1; k <= n_vec; ++k)
        vec[(ind - 1) * lda + k] *= fct;
    ops += n_vec + 1;

    ierr = 0;
}
