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
        private TestNode _r1r2r1 = null;
        private TestNode _r2 = null;
        private TestNode _r3 = null;
        private Model _model = null;


        View _ht;
        #endregion

        public Window1()
        {
            InitializeComponent();
            this.SetUp();
        }

        public void Window1_Loaded(object sender, RoutedEventArgs e)
        {

            this.Width = 500;
            this.Height = 500;


            _ht = new View(_model, _grid);
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
            _r1r2r1 = new TestNode("r1r2r1", false);
            _r1r1f = new TestNode("r1r1f");

            _r2 = new TestNode("r2", false);
            _r3 = new TestNode("r3", false);

            _root.Add(_r1);
            _root.Add(_f);
            _root.Add(_r1r2);
            _root.Add(_r2);
            _root.Add(_r3);

            _r2.Add(_r1r2r1);
            _r1r2.Add(new TestNode("bug1", false));
            _r1r2.Add(new TestNode("bug2", false));
            _r1r2.Add(new TestNode("bug3", false));
            _r1r2.Add(new TestNode("bug4", false));

            _r1.Add(_r1f);
            _r1.Add(_r1r1);

            _r1r1.Add(_r1r1f);



            _model = new Model(_root);
        }
    }

    #region Inner classes

    class TestNode : NodeBase
    {
        #region ctor

        public TestNode(string name)
            : base(name)
        {
        }

        public TestNode(string name, bool leaf)
            : base(name, leaf)
        {
        }

        #endregion

    }

    #endregion
}
