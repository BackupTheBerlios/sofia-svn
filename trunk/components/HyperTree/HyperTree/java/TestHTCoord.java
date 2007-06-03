/*
 * TestHTCoord.java
 *
 * www.bouthier.net
 * 2001
 */

package hypertree;

import junit.framework.*;


/**
 * The TestHTCoord is a JUnit test class
 * for the HTCoordS and HTCoordE classes.
 */
public class TestHTCoord
    extends TestCase {

    private HTCoordE ze      = null; 
    private HTCoordE zi      = null;
    private HTCoordS zs      = null;
    private HTCoordS zz      = null;
    private HTCoordS sOrigin = null;
    private HTCoordS sMax    = null;
    

  /* --- Constructor --- */

    /**
     * Constructor,
     * Take the name of the test in parameter.
     *
     * @param name    the name of the test
     */
    public TestHTCoord(String name) {
        super(name);
    }


  /* --- Suite --- */

    /**
     * Test Suite.
     */
    public static Test suite() {
        return new TestSuite(TestHTCoord.class);
    }


  /* --- Setup --- */

    /**
     * Setup the fixture.
     */
    protected void setUp() {
        ze = new HTCoordE();
        zi = new HTCoordE(0.42, -0.25);
        zs = new HTCoordS(42, -25);
        zz = new HTCoordS();
        sOrigin = new HTCoordS(100, 125);
        sMax    = new HTCoordS(90, 115);
    }

    
  /* --- Tests --- */

    /**
     * Test the euclidian to screen projection.
     */
    public void testEtoSProjection() {
        ze.x = 0.0;
        ze.y = 0.0;
        zs.projectionEtoS(ze, sOrigin, sMax);
        assertEquals("* S Proj origin x *", 100, zs.x);
        assertEquals("* S Proj origin y *", 125, zs.y);
        ze.x = 0.9;
        ze.y = -0.4;
        zs.projectionEtoS(ze, sOrigin, sMax);
        assertEquals("* S Proj x *", 181, zs.x);
        assertEquals("* S Proj y *", 171, zs.y);
    }

    /**
     * Test the screen contains method
     */
    public void testSContains() {
        zz.x = 30;
        zz.y = 20;
        assertTrue("* S Not contains *", ! zs.contains(zz));
        assertTrue("* S Same contains *", zs.contains(zs));
        zz.x = 45;
        zz.y = -28;
        assertTrue("* S Just in *", zs.contains(zz));
        zz.x = 46; 
        zz.y = -28;
        assertTrue("* S Just out *", ! zs.contains(zz));
    }

    /**
     * Test the screen distance
     */
    public void testSDistance() {
        zz.x = 2;
        zz.y = 5;
        assertEquals("* S distance nulle *", 0, zs.getDistance(zs));
        assertEquals("* S distance *", 50, zs.getDistance(zz));
    }

    /**
     * Test the screen to euclidian projection
     */
    public void testStoEProjection() {
        ze.projectionStoE(100, 125, sOrigin, sMax);
        assertEquals("* E Proj origin x *", 0.0, ze.x, 0.0);
        assertEquals("* E Proj origin y *", 0.0, ze.y, 0.0); 
        ze.projectionStoE(181, 171, sOrigin, sMax);
        assertEquals("* E Proj x *", 0.9, ze.x, 0.0);
        assertEquals("* E Proj y *", -0.4, ze.y, 0.0);
    }

    /**
     * Test the euclidian valid method
     */
    public void testEValid() {
        assertTrue("* E Origin valid *", ze.isValid());
        assertTrue("* E zi valid *", zi.isValid());
        ze.x = 1.0;
        ze.y = 1.0;
        assertTrue("* E 1:1 not valid *", ! ze.isValid());
        ze.x = 0.42;
        ze.y = 0.907;
        assertTrue("* E Just in *", ze.isValid());
        ze.y = 0.908;
        assertTrue("* E Just out *", ! ze.isValid());
    }

    /**
     * Test the euclidian multiplication
     */
    public void testEMultiplication() {
        ze.multiply(ze);
        assertEquals("* E mul 0*0 x *", 0.0, ze.x, 0.0);
        assertEquals("* E mul 0*0 y *", 0.0, ze.y, 0.0);
        ze.multiply(zi);
        assertEquals("* E mul 0*X x *", 0.0, ze.x, 0.0);
        assertEquals("* E mul 0*X y *", 0.0, ze.y, 0.0);
        ze.x = 0.5;
        ze.y = 0.3;
        ze.multiply(zi);
        assertEquals("* E mul X*Y x *", 0.285, ze.x, 0.0001);
        assertEquals("* E mul X*Y y *", 0.001, ze.y, 0.0001);
        ze.x = 0.5;
        ze.y = 0.3;
        zi.multiply(ze);
        assertEquals("* E mul Y*X x *", 0.285, zi.x, 0.0001);
        assertEquals("* E mul Y*X y *", 0.001, zi.y, 0.0001);
    }
    
    /**
     * Test the euclidian division
     */
    public void testEDivision() {
        ze.divide(zi);
        assertEquals("* E div 0/X x *", 0.0, ze.x, 0.0);
        assertEquals("* E div 0/X y *", 0.0, ze.y, 0.0);
        ze.x = 0.5;
        ze.y = 0.3;
        ze.divide(zi);
        assertEquals("* E div X/Y x *", 0.565, ze.x, 0.001);
        assertEquals("* E div X/Y y *", 1.050, ze.y, 0.001);
    }

    /**
     * Test euclidian substraction
     */
    public void testESub() {
        zi.sub(zi, ze);
        assertEquals("* E sub X-0 x *", 0.42, zi.x, 0.0);
        assertEquals("* E sub X-0 y *", -0.25, zi.y, 0.0);
        ze.x = 0.5;
        ze.y = 0.3;
        zi.sub(ze, zi);
        assertEquals("* E sub X-Y x *", 0.08, zi.x, 0.0001);
        assertEquals("* E sub X-Y y *", 0.55, zi.y, 0.0001);
    }

    /**
     * Test euclidian argument
     */
    public void testEArg() {
        ze.x = 0.5;
        ze.y = 0.5;
        assertEquals("* E Arg X *", Math.PI/4, ze.arg(), 0.001);
        ze.x = -0.5;
        ze.y = 0.5;
        assertEquals("* E Arg X *", 3 * Math.PI/4, ze.arg(), 0.001);
        ze.x = -0.5;
        ze.y = -0.5;
        assertEquals("* E Arg X *", 5 * Math.PI/4, ze.arg(), 0.001);
        ze.x = 0.5;
        ze.y = - 0.5;
        assertEquals("* E Arg X *", 7 * Math.PI/4, ze.arg(), 0.001);
    }

    /**
     * Test euclidian square distance
     */
    public void testED2() {
        assertEquals("* E d2 origin *", 0.0, ze.d2(), 0.0);
        assertEquals("* E d2 X *", 0.2389, zi.d2(), 0.00001);
    }

    /**
     * Test euclidian distance to origin
     */
    public void testEDtoO() {
        assertEquals("* E DtO origin *", 0.0, ze.d(), 0.0);
        assertEquals("* E DtO X *", 0.488, zi.d(), 0.001);
    }
   
    /**
     * Test euclidian distance
     */
    public void testEDistance() {
        assertEquals("* E dist 0-X *", 0.488, ze.d(zi), 0.001);
        ze.x = 0.5;
        ze.y = 0.3;
        assertEquals("* E dist X-Y *", 0.555, zi.d(ze), 0.001);
    }
    
    /**
     * Test hyperbolic translation
     */
    public void testTranslation() {
        ze.translate(zi);
        assertEquals("* E trans orig to X x *", zi.x, ze.x, 0.0);
        assertEquals("* E trans orig to X y *", zi.y, ze.y, 0.0);
        ze.x = 0.5;
        ze.y = 0.3;
        ze.translate(zi);
        assertEquals("* E trans X to Y x *", 0.782, ze.x, 0.001);
        assertEquals("* E trans X to Y y *", -0.128, ze.y, 0.001);
        ze.x = 0.5;
        ze.y = 0.3;
        ze.translate(ze, zi);
        assertEquals("* E trans X to Y x *", 0.782, ze.x, 0.001);
        assertEquals("* E trans X to Y y *", -0.128, ze.y, 0.001);
    }

    /**
     * Test hyperbolic transformation
     */
    public void testTransformation() {
        HTTransformation t = new HTTransformation();
        t.P = new HTCoordE(0.5, 0.3);
        t.O = new HTCoordE(0.0, 1.0);
        ze.transform(t);
        assertEquals("* E transf orig x *", 0.5, ze.x, 0.0);
        assertEquals("* E transf orig y *", 0.3, ze.y, 0.0);
        zi.transform(t);
        assertEquals("* E transf X x *", 0.654, zi.x, 0.001);
        assertEquals("* E transf Y y *", 0.504, zi.y, 0.001);
    }

    /**
     * Test translation composition
     */
    public void testComposition() {
        HTCoordE ztt = new HTCoordE(0.5, 0.3);
        HTCoordE zt = new HTCoordE(ztt);
        HTCoordE z1 = new HTCoordE(0.42, -0.25);
        HTCoordE z2 = new HTCoordE(-0.21, 0.94);
        HTTransformation t = new HTTransformation();
        t.composition(z1, z2);
        ztt.translate(z1);
        ztt.translate(z2);
        zt.transform(t);
        assertEquals("* E compose x *", ztt.x, zt.x, 1E-14);
        assertEquals("* E compose y *", ztt.y, zt.y, 1E-14);
    }

}

