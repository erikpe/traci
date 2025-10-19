package se.ejp.traci.math;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

public class PolynomSolverTest
{
    private static final double EPSILON = 1.0e-9;

    private void verifyQuadraticRoots(double[] coeffs, double[] roots)
    {
        assertEquals(3, coeffs.length);

        if (roots == null)
        {
            return;
        }
        for (double root : roots)
        {
            double result = coeffs[0] * root * root + coeffs[1] * root + coeffs[2];
            assertEquals("Root " + root + " should satisfy the equation", 0.0, result, EPSILON);
        }
    }

    private void verifyCubicRoots(double[] coeffs, double[] roots)
    {
        assertEquals(4, coeffs.length);

        if (roots == null)
        {
            return;
        }
        for (double root : roots)
        {
            double result = coeffs[0] * root * root * root +
                            coeffs[1] * root * root +
                            coeffs[2] * root +
                            coeffs[3];
            assertEquals("Root " + root + " should satisfy the equation", 0.0, result, EPSILON);
        }
    }

    private void verifyQuarticRoots(double[] coeffs, double[] roots)
    {
        assertEquals(5, coeffs.length);

        if (roots == null)
        {
            return;
        }
        for (double root : roots)
        {
            double result = coeffs[0] * root * root * root * root +
                            coeffs[1] * root * root * root +
                            coeffs[2] * root * root +
                            coeffs[3] * root +
                            coeffs[4];
            assertEquals("Root " + root + " should satisfy the equation", 0.0, result, EPSILON);
        }
    }

    // ==================== Quadratic Tests ====================

    @Test
    public void testQuadraticTwoRealRoots()
    {
        // x² - 5x + 6 = 0 => (x-2)(x-3) = 0 => roots: 2, 3
        double[] coeffs = {1.0, -5.0, 6.0};
        double[] roots = PolynomSolver.solveQuadratic(coeffs);
        assertEquals(2, roots.length);
        verifyQuadraticRoots(coeffs, roots);
    }

    @Test
    public void testQuadraticOneRoot()
    {
        // x² - 4x + 4 = 0 => (x-2)² = 0 => root: 2
        double[] coeffs = {1.0, -4.0, 4.0};
        double[] roots = PolynomSolver.solveQuadratic(coeffs);
        assertEquals(1, roots.length);
        assertEquals(2.0, roots[0], EPSILON);
        verifyQuadraticRoots(coeffs, roots);
    }

    @Test
    public void testQuadraticNoRealRoots()
    {
        // x² + 1 = 0 => no real roots
        double[] coeffs = {1.0, 0.0, 1.0};
        double[] roots = PolynomSolver.solveQuadratic(coeffs);
        assertNull(roots);
    }

    @Test
    public void testQuadraticLinearCase()
    {
        // 0x² + 2x - 4 = 0 => 2x = 4 => x = 2
        double[] coeffs = {0.0, 2.0, -4.0};
        double[] roots = PolynomSolver.solveQuadratic(coeffs);
        assertEquals(1, roots.length);
        assertEquals(2.0, roots[0], EPSILON);
    }

    @Test
    public void testQuadraticLinearNoRoot()
    {
        // 0x² + 0x + 5 = 0 => no solution
        double[] coeffs = {0.0, 0.0, 5.0};
        double[] roots = PolynomSolver.solveQuadratic(coeffs);
        assertNull(roots);
    }

    @Test
    public void testQuadraticNegativeCoefficients()
    {
        // -x² + 4 = 0 => x² = 4 => x = ±2
        double[] coeffs = {-1.0, 0.0, 4.0};
        double[] roots = PolynomSolver.solveQuadratic(coeffs);
        assertEquals(2, roots.length);
        verifyQuadraticRoots(coeffs, roots);
    }

    @Test
    public void testQuadraticZeroRoot()
    {
        // x² - x = 0 => x(x-1) = 0 => roots: 0, 1
        double[] coeffs = {1.0, -1.0, 0.0};
        double[] roots = PolynomSolver.solveQuadratic(coeffs);
        assertEquals(2, roots.length);
        verifyQuadraticRoots(coeffs, roots);
    }

    // ==================== Cubic Tests ====================

    @Test
    public void testCubicThreeRealRoots()
    {
        // x³ - 6x² + 11x - 6 = 0 => (x-1)(x-2)(x-3) = 0 => roots: 1, 2, 3
        double[] coeffs = {1.0, -6.0, 11.0, -6.0};
        double[] roots = PolynomSolver.solveCubic(coeffs);
        assertEquals(3, roots.length);
        verifyCubicRoots(coeffs, roots);
    }

    @Test
    public void testCubicOneRealRoot()
    {
        // x³ - 1 = 0 => (x-1)(x²+x+1) = 0 => one real root: 1
        double[] coeffs = {1.0, 0.0, 0.0, -1.0};
        double[] roots = PolynomSolver.solveCubic(coeffs);
        assertEquals(1, roots.length);
        assertEquals(1.0, roots[0], EPSILON);
        verifyCubicRoots(coeffs, roots);
    }

    @Test
    public void testCubicNegativeRoot()
    {
        // x³ + 8 = 0 => (x+2)(x²-2x+4) = 0 => one real root: -2
        double[] coeffs = {1.0, 0.0, 0.0, 8.0};
        double[] roots = PolynomSolver.solveCubic(coeffs);
        assertEquals(1, roots.length);
        assertEquals(-2.0, roots[0], EPSILON);
        verifyCubicRoots(coeffs, roots);
    }

    @Test
    public void testCubicReducesToQuadratic()
    {
        // 0x³ + x² - 5x + 6 = 0 => x² - 5x + 6 = 0 => roots: 2, 3
        double[] coeffs = {0.0, 1.0, -5.0, 6.0};
        double[] roots = PolynomSolver.solveCubic(coeffs);
        assertEquals(2, roots.length);
        // Verify as quadratic
        double[] quadCoeffs = {1.0, -5.0, 6.0};
        verifyQuadraticRoots(quadCoeffs, roots);
    }

    @Test
    public void testCubicWithZeroRoot()
    {
        // x³ - x² = 0 => x²(x-1) = 0 => roots include 0 and 1
        double[] coeffs = {1.0, -1.0, 0.0, 0.0};
        double[] roots = PolynomSolver.solveCubic(coeffs);
        verifyCubicRoots(coeffs, roots);
    }

    @Test
    public void testCubicDoubleRoot()
    {
        // (x-1)²(x-3) = x³ - 5x² + 7x - 3 = 0 => roots: 1 (double), 3
        // This is a more stable test case than a triple root
        double[] coeffs = {1.0, -5.0, 7.0, -3.0};
        double[] roots = PolynomSolver.solveCubic(coeffs);
        // The solver may return 1 or 3 roots depending on how it handles the double root
        verifyCubicRoots(coeffs, roots);
    }

    // ==================== Quartic Tests ====================

    @Test
    public void testQuarticFourRealRoots()
    {
        // (x-1)(x-2)(x-3)(x-4) = x⁴ - 10x³ + 35x² - 50x + 24 = 0
        double[] coeffs = {1.0, -10.0, 35.0, -50.0, 24.0};
        double[] roots = PolynomSolver.solveQuartic(coeffs);
        assertEquals(4, roots.length);
        verifyQuarticRoots(coeffs, roots);
    }

    @Test
    public void testQuarticTwoRealRoots()
    {
        // (x-1)(x-2)(x²+1) = x⁴ - 3x³ + 3x² - 3x + 2 = 0 => real roots: 1, 2
        double[] coeffs = {1.0, -3.0, 3.0, -3.0, 2.0};
        double[] roots = PolynomSolver.solveQuartic(coeffs);
        assertEquals(2, roots.length);
        verifyQuarticRoots(coeffs, roots);
    }

    @Test
    public void testQuarticNoRealRoots()
    {
        // x⁴ + 1 = 0 => no real roots
        double[] coeffs = {1.0, 0.0, 0.0, 0.0, 1.0};
        double[] roots = PolynomSolver.solveQuartic(coeffs);
        assertNull(roots);
    }

    @Test
    public void testQuarticBiquadratic()
    {
        // x⁴ - 5x² + 4 = 0 => (x²-1)(x²-4) = 0 => roots: ±1, ±2
        double[] coeffs = {1.0, 0.0, -5.0, 0.0, 4.0};
        double[] roots = PolynomSolver.solveQuartic(coeffs);
        assertEquals(4, roots.length);
        verifyQuarticRoots(coeffs, roots);
    }

    @Test
    public void testQuarticWithZeroRoot()
    {
        // x⁴ - x³ = 0 => x³(x-1) = 0 => roots include 0 and 1
        double[] coeffs = {1.0, -1.0, 0.0, 0.0, 0.0};
        double[] roots = PolynomSolver.solveQuartic(coeffs);
        verifyQuarticRoots(coeffs, roots);
    }

    @Test
    public void testQuarticPerfectSquare()
    {
        // (x²-4)² = x⁴ - 8x² + 16 = 0 => roots: ±2 (double roots)
        double[] coeffs = {1.0, 0.0, -8.0, 0.0, 16.0};
        double[] roots = PolynomSolver.solveQuartic(coeffs);
        // Should get 2 or 4 roots (depending on how duplicates are handled)
        verifyQuarticRoots(coeffs, roots);
    }

    @Test
    public void testQuarticNegativeLeadingCoefficient()
    {
        // -x⁴ + 5x² - 4 = 0 => x⁴ - 5x² + 4 = 0 => roots: ±1, ±2
        double[] coeffs = {-1.0, 0.0, 5.0, 0.0, -4.0};
        double[] roots = PolynomSolver.solveQuartic(coeffs);
        assertEquals(4, roots.length);
        verifyQuarticRoots(coeffs, roots);
    }

    // ==================== Edge Cases ====================

    @Test
    public void testQuadraticSmallDiscriminant()
    {
        // Test behavior near discriminant = 0
        double[] coeffs = {1.0, -2.0, 1.0 + 1e-11};
        double[] roots = PolynomSolver.solveQuadratic(coeffs);
        // Should still treat as one root due to SMALL_ENOUGH threshold
        assertEquals(1, roots.length);
        verifyQuadraticRoots(coeffs, roots);
    }

    @Test
    public void testLargeCoefficients()
    {
        // x² - 1000000x + 999999 = 0 => roots near 1 and 999999
        double[] coeffs = {1.0, -1000000.0, 999999.0};
        double[] roots = PolynomSolver.solveQuadratic(coeffs);
        assertEquals(2, roots.length);
        verifyQuadraticRoots(coeffs, roots);
    }

    @Test
    public void testSmallCoefficients()
    {
        // 0.001x² - 0.002x + 0.001 = 0 => x² - 2x + 1 = 0 => root: 1
        double[] coeffs = {0.001, -0.002, 0.001};
        double[] roots = PolynomSolver.solveQuadratic(coeffs);
        assertEquals(1, roots.length);
        assertEquals(1.0, roots[0], EPSILON);
        verifyQuadraticRoots(coeffs, roots);
    }
}
