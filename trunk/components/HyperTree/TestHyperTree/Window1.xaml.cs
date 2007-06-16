using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Shapes;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using HyperTreeControl;
using System.Windows.Markup;

namespace TestHyperTree
{
    /// <summary>
    /// Interaction logic for Window1.xaml
    /// </summary>

    public partial class Window1 : Window
    {
        #region fields
        private TestNode _root = null;
        private TestNode _r1 = null;
        private TestNode _f = null;
        private TestNode _r1f = null;
        private TestNode _r1r1 = null;
        private TestNode _r1r2 = null;
        private TestNode _r1r1f = null;
        private HtModel _model = null;


        WpfHtView _ht;
        #endregion

        public Window1()
        {
            InitializeComponent();
            this.SetUp();
        }

        public void Window1_Loaded(object sender, RoutedEventArgs e)
        {            

            _ht = new WpfHtView(_model, _grid);
            _ht.Height = 500;
            _ht.Width = 500;
            
        }

        private void SetUp()
        {            
            _root = new TestNode("root", false);            
            _r1 = new TestNode("r1", false);
            _f = new TestNode("f");
            _r1f = new TestNode("r1f");
            _r1r1 = new TestNode("r1r1", false);
            _r1r2 = new TestNode("r1r2", false);
            _r1r1f = new TestNode("r1r1f");

            _root.Add(_r1);
            _root.Add(_f);
            _r1.Add(_r1f);
            _r1.Add(_r1r1);
            _r1.Add(_r1r2);
            _r1r1.Add(_r1r1f);

            _model = new HtModel(_root);    
        }
    }

    #region Inner classes

    class TestNode : WpfHtNode
    {

        private string _name = null; // name of the node
        private bool _leaf = true; // is a leaf
        private List<IHtNode> _children = null; // children

        #region ctor

        public TestNode(string name)
        {
            _name = name;
            _children = new List<IHtNode>();
        }


        public TestNode(string name, bool leaf)
            : this(name)
        {
            _leaf = leaf;
        }

        #endregion


        /// <summary> Is leaf ?
        /// </summary>
        public override bool IsLeaf
        {
            get
            {
                return _leaf;
            }
        }

        /// <summary> Gets the name.
        /// </summary>
        public override string NodeName
        {
            get
            {
                return _name;
            }
        }

        public override List<IHtNode> ChildNodes
        {
            get
            {
                return _children;
            }
        }

        public void Add(IHtNode node)
        {            
            _children.Add(node);
        }

    }

    #endregion
}
