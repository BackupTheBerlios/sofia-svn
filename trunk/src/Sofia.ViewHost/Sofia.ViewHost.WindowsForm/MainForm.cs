using System;
using System.Collections.Generic;
using System.Collections;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Reflection;

using Sofia.ViewHost.WindowsForm.Properties;

using Sofia.Plugins;
using Sofia.Mvc;
using Sofia.DesignPatterns;

#if GTK
using Sofia.Plugins.Gtk;
#else
using Sofia.Plugins.WindowsForm;
#endif

namespace Sofia.ViewHost.WindowsForm
{
    public partial class MainForm : ViewHostBase
    {
        public MainForm()
            : base()
        {
            InitializeComponent();
            _ViewTabs = new Hashtable();
            PluginManager.AutoRegister();

            //Recherche
            IPlugin plugin = PluginManager["Sofia.Plugins.Core.Search"];
            IView view = plugin.CreateView();
            ShowToolBar(view, 1);
            (view as IObservable).RegisterObserver(this);
        }

        #region Méthodes héritées

        public override void ShowView(IView view, ViewDestination destination)
        {
            Control control = view.Control as Control;
            control.Dock = DockStyle.Fill;

            if (destination == ViewDestination.Tabbed)
            {
                TabPage tabPage = new TabPage(view.Tags[0]);
                tabPage.Name = view.ContentId.ToString("N");
                tabPage.Location = new System.Drawing.Point(6, 6);
                tabPage.Controls.Add(control);
                tabPage.Tag = view;
                _Pages.Controls.Add(tabPage);
            }

        }

        public override void ShowToolBar(IView view, int row)
        {
            if (view.Toolbar != null)
            {
                ToolStrip toolbar = view.Toolbar as ToolStrip;

                //Calcul de la position de la barre d'outils
                int controlCount = _MainbarContainer.TopToolStripPanel.Controls.Count;
                Control lastControl = _MainbarContainer.TopToolStripPanel.Controls[controlCount - 1];
                ToolStripPanelRow[] rows = _MainbarContainer.TopToolStripPanel.Rows;
                Point p = new Point(lastControl.Bounds.Width + 3, rows[row].Bounds.Top);

                _MainbarContainer.TopToolStripPanel.Join(toolbar, p);
            }
        }

        public override void Save()
        {
            IView view = _Pages.SelectedTab.Tag as IView;
            view.SaveTo(ViewFormat.Xml);
        }

        public override void New()
        {
            IPlugin plugin = PluginManager["Sofia.Plugins.General.Contact"];
            IView view = plugin.CreateView();
            view.ContentId = Guid.NewGuid();

            if (plugin.Controller.ViewCount == 1)
                ShowToolBar(view, 2);

            ShowView(view, ViewDestination.Tabbed);
        }

        public override void Update(object sender, object notification)
        {
            if (sender is IView)
            {
                IView view = sender as IView;
                if ((ViewNotification)notification == ViewNotification.Show)
                    ShowView(view, ViewDestination.Tabbed);
            }
        }

        #endregion

        #region UI

        private void enregistrerToolStripButton_Click(object sender, EventArgs e)
        {
            Save();
        }

        private void enregistrerToolStripMenuItem_Click(object sender, EventArgs e)
        {
            Save();
        }

        private void nouveauToolStripMenuItem_Click(object sender, EventArgs e)
        {
            New();
        }

        private void nouveauToolStripButton_Click(object sender, EventArgs e)
        {
            New();
        }

        #endregion

    }
}