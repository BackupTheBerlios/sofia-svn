/*
 * TestHTModel.java
 *
 * www.bouthier.net
 * 2001
 */

package hypertree;

import junit.framework.*;

import java.util.Iterator;
import java.util.ArrayList;


/**
 * The TestHTModel is a JUnit test class
 * for the HTModel, HTModelNode and HTModelNodeComposite classes.
 */
public class TestHTModel
    extends TestCase {

    private TestNode             root   = null;
    private TestNode             r1     = null;
    private TestNode             f      = null;
    private TestNode             r1f    = null;
    private TestNode             r1r1   = null;
    private TestNode             r1r2   = null;
    private TestNode             r1r1f  = null;
    private HTModel              model  = null;
    private HTModelNodeComposite mroot  = null;
    private HTModelNodeComposite mr1    = null;
    private HTModelNode          mf     = null;
    private HTModelNode          mr1f   = null;
    private HTModelNodeComposite mr1r1  = null;
    private HTModelNodeComposite mr1r2  = null;
    private HTModelNode          mr1r1f = null;


  /* --- Constructor --- */

    /**
     * Constructor,
     * Take the name of the test in parameter.
     *
     * @param name    the name of the test
     */
    public TestHTModel(String name) {
        super(name);
    }


  /* --- Suite --- */

    /**
     * Test Suite.
     */
    public static Test suite() {
        return new TestSuite(TestHTModel.class);
    }


  /* --- Setup --- */

    /**
     * Setup the fixture.
     */
    protected void setUp() {
        root  = new TestNode("root", false);
        r1    = new TestNode("r1", false);
        f     = new TestNode("f");
        r1f   = new TestNode("r1f");
        r1r1  = new TestNode("r1r1", false);
        r1r2  = new TestNode("r1r2", false);
        r1r1f = new TestNode("r1r1f");
        root.add(r1);
        root.add(f);
        r1.add(r1f);
        r1.add(r1r1);
        r1.add(r1r2);
        r1r1.add(r1r1f);
        model = new HTModel(root);
        mroot = (HTModelNodeComposite) model.getRoot();
        Iterator i = mroot.children();
        mr1 = (HTModelNodeComposite) i.next();
        mf = (HTModelNode) i.next();
        i = mr1.children();
        mr1f = (HTModelNode) i.next();
        mr1r1 = (HTModelNodeComposite) i.next();
        mr1r2 = (HTModelNodeComposite) i.next();
        i = mr1r1.children();
        mr1r1f = (HTModelNode) i.next();
    }

    
  /* --- Tests --- */

    /**
     * Tests the number of nodes in HTModel.
     */
    public void testNumberOfNodes() {
        assertEquals("* Nbr Nodes", 7, model.getNumberOfNodes());
    }

    /**
     * Test names.
     */
    public void testRootName() {
        assertEquals("* root name *", "root", model.getRoot().getName());
        assertEquals("* root name again *", "root", mroot.getName());
        assertEquals("* r1 name *", "r1", mr1.getName());
        assertEquals("* f name *", "f", mf.getName());
        assertEquals("* r1f name *", "r1f", mr1f.getName());
        assertEquals("* r1r1 name *", "r1r1", mr1r1.getName());
        assertEquals("* r1r2 name *", "r1r2", mr1r2.getName());
        assertEquals("* r1r1f name *", "r1r1f", mr1r1f.getName());
    }

    /**
     * Test the leaf thing.
     */
    public void testLeaf() {
        assertTrue("* root !leaf *", ! model.getRoot().isLeaf());
        assertTrue("* mroot !leaf *", ! mroot.isLeaf());
        assertTrue("* mr1 !leaf *", ! mr1.isLeaf());
        assertTrue("* f leaf *", f.isLeaf());
        assertTrue("* mr1f leaf *", mr1f.isLeaf());
        assertTrue("* mr1r1 !leaf *", ! mr1r1.isLeaf());
        assertTrue("* mr1r2 !leaf *", ! mr1r2.isLeaf());
        assertTrue("* mr1r1f leaf *", mr1r1f.isLeaf());
    }

    /**
     * Test children access.
     */
    public void testChildrenAccess() {
        HTModelNodeComposite root = (HTModelNodeComposite) model.getRoot();
        Iterator i = root.children();
        HTModelNode child = (HTModelNode) i.next();
        assertEquals("* child name *", "r1", child.getName());
        assertTrue("* child !leaf *", ! child.isLeaf());
        assertTrue("* child composite *", 
                   (child instanceof HTModelNodeComposite));
        HTModelNodeComposite rChild = (HTModelNodeComposite) child;
        Iterator i1 = rChild.children();
        HTModelNode cChild = (HTModelNode) i1.next();
        assertEquals("* cChild name *", "r1f", cChild.getName());
        assertTrue("* cChild leaf *", cChild.isLeaf());
        assertTrue("* cChild !composite", 
                   ! (cChild instanceof HTModelNodeComposite));
    }

    /**
     * Test parent access.
     */
    public void testParentAccess() {
        assertEquals("* mr1 parent *", mroot, mr1.getParent());
        assertEquals("* mf parent *", mroot, mf.getParent());
        assertEquals("* mr1f parent *", mr1, mr1f.getParent());
        assertEquals("* mr1 parent *", mr1, mr1r1.getParent());
        assertEquals("* mr1r2 parent *", mr1, mr1r2.getParent());
        assertEquals("* mr1r1 parent *", mr1r1, mr1r1f.getParent());
        assertNull("* mroot parent *", mroot.getParent());
    }

    /**
     * Test weight computing.
     */
    public void testWeightComputing() {
        assertEquals("* mr1r1f weight *", 1.0, mr1r1f.getWeight(), 0.0);
        assertEquals("* mr1f weight *", 1.0, mr1f.getWeight(), 0.0);
        assertEquals("* mf weight *", 1.0, mf.getWeight(), 0.0);
        assertEquals("* mr1r2 weight *", 1.0, mr1r2.getWeight(), 0.0);
        assertEquals("* mr1r1 weight *", 1.0, mr1r1.getWeight(), 0.0);
        assertEquals("* mr1 weight *", 2.098, mr1.getWeight(), 0.001);
        assertEquals("* mroot weight *", 2.130, mroot.getWeight(), 0.001);
    }

    /**
     * Test layout of tree.
     */
    public void testLayout() {
        assertEquals("* mroot.x *", 0.0, mroot.getCoordinates().x, 0.0);
        assertEquals("* mroot.y *", 0.0, mroot.getCoordinates().y, 0.0);
        assertEquals("* mr1.x *", 0.184, mr1.getCoordinates().x, 0.001);
        assertEquals("* mr1.y *", -0.295, mr1.getCoordinates().y, 0.001);
        assertEquals("* mf.x *", -0.184, mf.getCoordinates().x, 0.001);
        assertEquals("* mf.y *", 0.295, mf.getCoordinates().y, 0.001);
        assertEquals("* mr1f.x *", -0.119, mr1f.getCoordinates().x, 0.001);
        assertEquals("* mr1f.y *", -0.469, mr1f.getCoordinates().y, 0.001);
        assertEquals("* mr1r1.x *", 0.344, mr1r1.getCoordinates().x, 0.001);
        assertEquals("* mr1r1.y *", -0.553, mr1r1.getCoordinates().y, 0.001);
        assertEquals("* mr1r2.x *", 0.474, mr1r2.getCoordinates().x, 0.001);
        assertEquals("* mr1r2.y *", -0.100, mr1r2.getCoordinates().y, 0.001);
        assertEquals("* mr1r1f.x *", 0.420, mr1r1f.getCoordinates().x, 0.001);
        assertEquals("* mr1r1f.y *", -0.675, mr1r1f.getCoordinates().y, 0.001);
    }


  /* --- Inners nodes --- */

    /**
     * Implements HTNode
     */
    class TestNode
        extends AbstractHTNode {

        private String  name     = null; // name of the node
        private boolean leaf     = true; // is a leaf
        private ArrayList  children = null; // children
        
            
     /* --- Standard --- */

        /**
         * Constructor.
         */
        TestNode(String name) {
            this.name = name;
            children = new ArrayList();
        }

        /**
         * Constructor.
         */
        TestNode(String name, boolean leaf) {
            this(name);
            this.leaf = leaf;
        }
        
        /**
         * Add child.
         */
        void add(TestNode child) {
            children.add(child);
        }


      /* --- HTNode --- */

        /**
         * Is leaf ?
         */
        public boolean isLeaf() {
            return leaf;
        }
        
        /**
         * Returns name.
         */
        public String getName() {
            return name;
        }

        /**
         * Returns children.
         */
        public Iterator children() {
            return this.children.iterator();
        }
    }

}

