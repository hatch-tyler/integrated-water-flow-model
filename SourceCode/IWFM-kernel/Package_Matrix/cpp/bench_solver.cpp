/*
 *  IWFM C++ Solver Benchmark
 *
 *  Benchmarks SpMV (AMUX), ILUT, and GMRES on tridiagonal and
 *  5-point stencil matrices at sizes relevant to IWFM models.
 *
 *  Build with the same CMake or manually:
 *    cl /O2 /EHsc /openmp -std:c++17 bench_solver.cpp blas.cpp spmv.cpp
 *       lusol.cpp ilut.cpp gmres.cpp
 */

#include "iwfm_solver.h"
#include <cstdio>
#include <cmath>
#include <vector>
#include <chrono>
#include <cstdlib>
#include <algorithm>

#ifdef _OPENMP
#include <omp.h>
#endif

using Clock = std::chrono::high_resolution_clock;
using Duration = std::chrono::duration<double, std::milli>;

// =========================================================================
// Build a 5-point stencil matrix (2D Laplacian on sqrt(n) x sqrt(n) grid)
// Falls back to tridiagonal if n is not a perfect square.
// Returns CRS arrays with 1-based index values.
// =========================================================================
struct CRSMatrix {
    int n;
    std::vector<double> a;
    std::vector<int> ja;
    std::vector<int> ia;
    int nnz() const { return static_cast<int>(a.size()); }
};

CRSMatrix build_laplacian_2d(int n)
{
    int nx = static_cast<int>(std::sqrt(static_cast<double>(n)));
    if (nx * nx != n) {
        // Not a perfect square — just use tridiagonal
        nx = n;
    }
    int ny = n / nx;
    int N = nx * ny;

    CRSMatrix m;
    m.n = N;
    m.ia.resize(N + 1);

    // For a 2D Laplacian, each interior node has 5 entries
    m.a.reserve(5 * N);
    m.ja.reserve(5 * N);

    int ptr = 1;  // 1-based row pointer
    for (int row = 0; row < N; ++row) {
        m.ia[row] = ptr;
        int ix = row % nx;
        int iy = row / nx;

        // Neighbors: south, west, center, east, north
        // Keep column indices sorted (required by ILUT)
        struct Entry { int col; double val; };
        Entry entries[5];
        int ne = 0;

        if (iy > 0)      entries[ne++] = {row - nx + 1, -1.0};  // south (1-based)
        if (ix > 0)      entries[ne++] = {row - 1 + 1,  -1.0};  // west
        entries[ne++] = {row + 1, 4.0};                           // center (1-based)
        if (ix < nx - 1) entries[ne++] = {row + 1 + 1,  -1.0};  // east
        if (iy < ny - 1) entries[ne++] = {row + nx + 1, -1.0};  // north

        // Sort by column index
        std::sort(entries, entries + ne, [](const Entry& a, const Entry& b) {
            return a.col < b.col;
        });

        for (int e = 0; e < ne; ++e) {
            m.a.push_back(entries[e].val);
            m.ja.push_back(entries[e].col);
            ++ptr;
        }
    }
    m.ia[N] = ptr;
    return m;
}

CRSMatrix build_tridiagonal(int n)
{
    CRSMatrix m;
    m.n = n;
    m.ia.resize(n + 1);
    m.a.reserve(3 * n);
    m.ja.reserve(3 * n);

    int ptr = 1;
    for (int i = 1; i <= n; ++i) {
        m.ia[i - 1] = ptr;
        if (i > 1) { m.a.push_back(-1.0); m.ja.push_back(i - 1); ++ptr; }
        m.a.push_back(2.0); m.ja.push_back(i); ++ptr;
        if (i < n) { m.a.push_back(-1.0); m.ja.push_back(i + 1); ++ptr; }
    }
    m.ia[n] = ptr;
    return m;
}

// =========================================================================
// Benchmark SpMV
// =========================================================================
void bench_spmv(const char* label, const CRSMatrix& mat, int repeats)
{
    int n = mat.n;
    std::vector<double> x(n, 1.0), y(n, 0.0);

    // Warmup
    for (int r = 0; r < 3; ++r)
        iwfm_amux(n, x.data(), y.data(), mat.a.data(), mat.ja.data(), mat.ia.data());

    auto t0 = Clock::now();
    for (int r = 0; r < repeats; ++r)
        iwfm_amux(n, x.data(), y.data(), mat.a.data(), mat.ja.data(), mat.ia.data());
    auto t1 = Clock::now();

    double ms = Duration(t1 - t0).count() / repeats;
    double gflops = 2.0 * mat.nnz() / (ms * 1e6);
    printf("  %-30s  n=%-6d  nnz=%-8d  %.3f ms/call  %.3f GFLOP/s\n",
           label, n, mat.nnz(), ms, gflops);
}

// =========================================================================
// Benchmark ILUT
// =========================================================================
void bench_ilut(const char* label, const CRSMatrix& mat, int lfil, int repeats)
{
    int n = mat.n;
    int iwk = n * (2 * lfil + 1);
    std::vector<double> alu(iwk);
    std::vector<int> jlu(iwk), ju(n);
    std::vector<double> w(n + 1);
    std::vector<int> jw(2 * n);

    // Warmup
    iwfm_ilut(n, mat.a.data(), mat.ja.data(), mat.ia.data(), lfil, 0.01,
              alu.data(), jlu.data(), ju.data(), iwk, w.data(), jw.data());

    auto t0 = Clock::now();
    for (int r = 0; r < repeats; ++r) {
        std::fill(alu.begin(), alu.end(), 0.0);
        std::fill(jlu.begin(), jlu.end(), 0);
        std::fill(ju.begin(), ju.end(), 0);
        iwfm_ilut(n, mat.a.data(), mat.ja.data(), mat.ia.data(), lfil, 0.01,
                  alu.data(), jlu.data(), ju.data(), iwk, w.data(), jw.data());
    }
    auto t1 = Clock::now();

    double ms = Duration(t1 - t0).count() / repeats;
    printf("  %-30s  n=%-6d  lfil=%-2d      %.3f ms/call\n", label, n, lfil, ms);
}

// =========================================================================
// Benchmark full GMRES solve (ILUT + GMRES reverse-comm)
// =========================================================================
void bench_gmres(const char* label, const CRSMatrix& mat, int lfil, int repeats)
{
    int n = mat.n;
    int iwk = n * (2 * lfil + 1);
    std::vector<double> alu(iwk);
    std::vector<int> jlu(iwk), ju(n);
    std::vector<double> w_ilut(n + 1);
    std::vector<int> jw(2 * n);

    // Build RHS: A * [1,1,...,1]
    std::vector<double> x_true(n, 1.0), rhs(n, 0.0);
    iwfm_amux(n, x_true.data(), rhs.data(), mat.a.data(), mat.ja.data(), mat.ia.data());

    // ILUT (once)
    int ierr = iwfm_ilut(n, mat.a.data(), mat.ja.data(), mat.ia.data(), lfil, 0.01,
                         alu.data(), jlu.data(), ju.data(), iwk, w_ilut.data(), jw.data());
    if (ierr != 0) {
        printf("  %-30s  ILUT failed (ierr=%d)\n", label, ierr);
        return;
    }

    int im = 20;
    int wsize = (n + 3) * (im + 2) + (im + 1) * im / 2;

    double total_ms = 0.0;
    int total_iters = 0;
    int solves = 0;

    for (int r = 0; r < repeats; ++r) {
        int ipar[16] = {};
        double fpar[16] = {};
        ipar[0] = 0; ipar[1] = 1; ipar[2] = 2;
        ipar[3] = wsize; ipar[4] = im; ipar[5] = 500;
        fpar[0] = 1e-10; fpar[1] = 1e-16;

        std::vector<double> w(wsize, 0.0);
        std::vector<double> sol(n, 0.0);
        std::vector<double> rhs_copy(rhs);

        auto t0 = Clock::now();
        bool converged = false;
        for (int loop = 0; loop < 5000; ++loop) {
            iwfm_gmres(n, rhs_copy.data(), sol.data(), ipar, fpar, w.data());

            if (ipar[0] == 1) {
                iwfm_amux(n, &w[ipar[7] - 1], &w[ipar[8] - 1],
                          mat.a.data(), mat.ja.data(), mat.ia.data());
            } else if (ipar[0] == 3 || ipar[0] == 5) {
                iwfm_lusol(n, &w[ipar[7] - 1], &w[ipar[8] - 1],
                           alu.data(), jlu.data(), ju.data());
            } else if (ipar[0] == 0) {
                converged = true;
                break;
            } else if (ipar[0] < 0) {
                break;
            }
        }
        auto t1 = Clock::now();

        if (converged) {
            total_ms += Duration(t1 - t0).count();
            total_iters += ipar[6];
            ++solves;
        }
    }

    if (solves > 0) {
        double ms = total_ms / solves;
        double avg_iter = static_cast<double>(total_iters) / solves;
        printf("  %-30s  n=%-6d  %.3f ms/solve  %.0f iters avg\n",
               label, n, ms, avg_iter);
    } else {
        printf("  %-30s  n=%-6d  FAILED to converge\n", label, n);
    }
}

// =========================================================================
int main()
{
    printf("=== IWFM C++ Solver Benchmark ===\n\n");

#ifdef _OPENMP
    printf("OpenMP threads: %d\n\n", omp_get_max_threads());
#else
    printf("OpenMP: disabled (sequential)\n\n");
#endif

    // Build test matrices at various sizes
    int sizes_tri[] = {100, 1000, 5000, 10000, 30000};
    int sizes_2d[]  = {100, 900, 2500, 10000, 30625};  // Perfect squares for 2D grids

    // --- SpMV Benchmark ---
    printf("--- SpMV (AMUX) ---\n");
    for (int n : sizes_tri) {
        auto m = build_tridiagonal(n);
        int reps = n < 5000 ? 10000 : (n < 20000 ? 1000 : 200);
        char label[64];
        snprintf(label, sizeof(label), "Tridiagonal %d", n);
        bench_spmv(label, m, reps);
    }
    for (int n : sizes_2d) {
        if (n > 100) {
            auto m = build_laplacian_2d(n);
            int reps = n < 5000 ? 5000 : (n < 20000 ? 500 : 100);
            int nx = static_cast<int>(std::sqrt(static_cast<double>(n)));
            char label[64];
            snprintf(label, sizeof(label), "2D Laplacian %dx%d", nx, nx);
            bench_spmv(label, m, reps);
        }
    }

    // --- ILUT Benchmark ---
    printf("\n--- ILUT Preconditioner ---\n");
    for (int n : sizes_tri) {
        if (n <= 10000) {
            auto m = build_tridiagonal(n);
            int reps = n < 1000 ? 1000 : (n < 5000 ? 100 : 10);
            char label[64];
            snprintf(label, sizeof(label), "Tridiagonal %d", n);
            bench_ilut(label, m, 5, reps);
        }
    }
    for (int n : sizes_2d) {
        if (n >= 900 && n <= 10000) {
            auto m = build_laplacian_2d(n);
            int reps = n < 5000 ? 100 : 10;
            int nx = static_cast<int>(std::sqrt(static_cast<double>(n)));
            char label[64];
            snprintf(label, sizeof(label), "2D Laplacian %dx%d", nx, nx);
            bench_ilut(label, m, 5, reps);
        }
    }

    // --- GMRES Solve Benchmark ---
    printf("\n--- GMRES Solve (ILUT precond, tol=1e-10) ---\n");
    for (int n : sizes_tri) {
        auto m = build_tridiagonal(n);
        int reps = n < 1000 ? 100 : (n < 5000 ? 20 : 5);
        char label[64];
        snprintf(label, sizeof(label), "Tridiagonal %d", n);
        bench_gmres(label, m, 5, reps);
    }
    for (int n : sizes_2d) {
        if (n >= 900) {
            auto m = build_laplacian_2d(n);
            int reps = n < 5000 ? 20 : 5;
            int nx = static_cast<int>(std::sqrt(static_cast<double>(n)));
            char label[64];
            snprintf(label, sizeof(label), "2D Laplacian %dx%d", nx, nx);
            bench_gmres(label, m, 5, reps);
        }
    }

    printf("\n=== Done ===\n");
    return 0;
}
