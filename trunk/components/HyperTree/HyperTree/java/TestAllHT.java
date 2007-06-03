/**
 * TestAllHT.java
 *
 * www.bouthier.net
 * 2001
 */

package hypertree;

import junit.framework.Test;
import junit.framework.TestSuite;


/**
 * The TestAllHT class launch the whole
 * set of JUnit test for the hypertree package.
 */
public class TestAllHT {

    /**
     * Suite of the whole set of tests.
     */
    public static Test suite() {
        TestSuite suite = new TestSuite("All hypertree Tests");
        suite.addTest(TestHTModel.suite());
        suite.addTest(TestHTCoord.suite());
        return suite;
    }
}

