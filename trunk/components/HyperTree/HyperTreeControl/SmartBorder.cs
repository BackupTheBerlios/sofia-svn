using System;
using System.Collections.Generic;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace WiredPrairie.Decorators
{
    public class SmartBorder : Decorator
    {
        #region Dependency Properties
        public static readonly DependencyProperty GlowBrushProperty =
            DependencyProperty.Register("GlowBrush", typeof(Brush), typeof(SmartBorder), new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender));

        public static readonly DependencyProperty CornerRadiusProperty =
            DependencyProperty.Register("CornerRadius", typeof(double), typeof(SmartBorder), new FrameworkPropertyMetadata(8.0, FrameworkPropertyMetadataOptions.AffectsRender));

        public static readonly DependencyProperty OuterGlowBrushProperty =
            DependencyProperty.Register("OuterGlowBrush", typeof(Brush), typeof(SmartBorder), new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender));

        public static readonly DependencyProperty RenderIsMouseOverProperty =
            DependencyProperty.Register("RenderIsMouseOver", typeof(bool), typeof(SmartBorder), new PropertyMetadata(false, new PropertyChangedCallback(OnRenderIsMouseOverChanged)));

        public static readonly DependencyProperty RenderIsPressedProperty =
            DependencyProperty.Register("RenderIsPressed", typeof(bool), typeof(SmartBorder), new PropertyMetadata(false, new PropertyChangedCallback(OnRenderIsPressedChanged)));

        public static readonly DependencyProperty BorderWidthProperty =
            DependencyProperty.Register("BorderWidth", typeof(double), typeof(SmartBorder), new FrameworkPropertyMetadata(2.0, FrameworkPropertyMetadataOptions.AffectsRender));

        public static readonly DependencyProperty BorderBrushProperty =
            DependencyProperty.Register("BorderBrush", typeof(Brush), typeof(SmartBorder), new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender));

        public static readonly DependencyProperty BackgroundProperty =
            DependencyProperty.Register("Background", typeof(Brush), typeof(SmartBorder), new FrameworkPropertyMetadata(null, FrameworkPropertyMetadataOptions.AffectsRender));
        #endregion

        #region Dependency Property backing CLR properties
        public Brush Background
        {
            get { return (Brush)GetValue(BackgroundProperty); }
            set { SetValue(BackgroundProperty, value); }
        }

        public Brush BorderBrush
        {
            get { return (Brush)GetValue(BorderBrushProperty); }
            set { SetValue(BorderBrushProperty, value); }
        }

        public double BorderWidth
        {
            get { return (double)GetValue(BorderWidthProperty); }
            set { SetValue(BorderWidthProperty, value); }
        }

        public bool RenderIsPressed
        {
            get { return (bool)GetValue(RenderIsPressedProperty); }
            set { SetValue(RenderIsPressedProperty, value); }
        }

        public bool RenderIsMouseOver
        {
            get { return (bool)GetValue(RenderIsMouseOverProperty); }
            set { SetValue(RenderIsMouseOverProperty, value); }
        }

        public Brush OuterGlowBrush
        {
            get { return (Brush)GetValue(OuterGlowBrushProperty); }
            set { SetValue(OuterGlowBrushProperty, value); }
        }

        public double CornerRadius
        {
            get { return (double)GetValue(CornerRadiusProperty); }
            set { SetValue(CornerRadiusProperty, value); }
        }

        public Brush GlowBrush
        {
            get { return (Brush)GetValue(GlowBrushProperty); }
            set { SetValue(GlowBrushProperty, value); }
        }
        #endregion

        // if the button is pressed, this fires
        private static void OnRenderIsPressedChanged(DependencyObject o, DependencyPropertyChangedEventArgs e)
        {
            SmartBorder border = o as SmartBorder;
            if (border != null)
            {
                if ((bool) e.NewValue == true)
                {
                }
                border.InvalidateVisual();
            }
        }

        // if the mouse is over the control, this fires
        private static void OnRenderIsMouseOverChanged(DependencyObject o, DependencyPropertyChangedEventArgs e)
        {
            SmartBorder border = o as SmartBorder;
            if (border != null)
            {
                border.InvalidateVisual();
            }

        }

        // a series of methods which all make getting the default or currently selected brush easier
        protected virtual Brush GetBackgroundBrush()
        {
            Brush b = Background;
            if (b != null)
            {
                return b;
            }

            return Brushes.Orange;
        }

        protected virtual Brush GetBorderBrush()
        {
            Brush b = BorderBrush;
            if (b != null)
            {
                return b;
            }

            return Brushes.DarkGray;
        }

        protected virtual Brush GetOuterGlowBrush()
        {
            Brush b = OuterGlowBrush;
            if (b != null)
            {
                return b;
            }

            return new SolidColorBrush(Color.FromArgb(100, 255, 255, 200));
        }

        protected virtual Brush GetGlowBrush()
        {
            Brush b = GlowBrush;
            if (b != null)
            {
                return b;
            }
            return new RadialGradientBrush(Color.FromArgb(200, 255, 255, 255), Color.FromArgb(0, 255, 255, 255));
        }

        protected override void OnRender(DrawingContext dc)
        {
            Rect rc = new Rect(0, 0, this.ActualWidth, this.ActualHeight);

            LinearGradientBrush gradientOverlay = GetGradientOverlay();
            Brush glowBrush = GetGlowBrush();
            Brush backBrush = GetBackgroundBrush();
            Brush borderBrush = GetBorderBrush();
            Pen borderPen = new Pen(borderBrush, BorderWidth);
            double cornerRadiusCache = CornerRadius;

            // draw the highlight as necessary
            if (RenderIsMouseOver)
            {
                Rect rcGlow = rc;
                double glowMove = BorderWidth * 2;
                rcGlow.Inflate(glowMove, glowMove);
                glowMove = 0;
                rcGlow.Offset(new Vector(glowMove, glowMove));
                dc.DrawRoundedRectangle(GetOuterGlowBrush(), null, rcGlow, cornerRadiusCache, cornerRadiusCache);
            }

            // we want to clip anything that might errantly draw outside of the smart border control
            dc.PushClip(new RectangleGeometry(rc, cornerRadiusCache, cornerRadiusCache));

            dc.DrawRoundedRectangle(backBrush, borderPen, rc, cornerRadiusCache, cornerRadiusCache);
            dc.DrawRoundedRectangle(gradientOverlay, borderPen, rc, cornerRadiusCache, cornerRadiusCache);

            if (!RenderIsPressed)
            {
                double clipBorderSize = BorderWidth * -4.0;
                Rect rcClip = rc;
                rcClip.Offset(clipBorderSize, clipBorderSize);
                rcClip.Inflate(-clipBorderSize, -clipBorderSize);
                dc.PushClip(new RectangleGeometry(rcClip, cornerRadiusCache, cornerRadiusCache));
                dc.DrawEllipse(glowBrush, null, new Point(this.ActualWidth / 2, this.ActualHeight * 0.10), this.ActualWidth * 0.80, this.ActualHeight * 0.40);
                dc.Pop();
            }
            // just draw the border now to make sure it overlaps everything nicely
            dc.DrawRoundedRectangle(null, borderPen, rc, cornerRadiusCache, cornerRadiusCache);

            dc.Pop();
            //base.OnRender(drawingContext);
        }

        protected virtual LinearGradientBrush GetGradientOverlay()
        {
            GradientStopCollection gradientStops = new GradientStopCollection();
            gradientStops.Add(new GradientStop(Color.FromArgb(0, 255, 255, 255), 0.0));
            gradientStops.Add(new GradientStop(Color.FromArgb(80, 80, 80, 80), 1.0));

            LinearGradientBrush gradientOverlay = new LinearGradientBrush(gradientStops, 90.0);

            return gradientOverlay;
        }

        protected override Size MeasureOverride(Size constraint)
        {
            UIElement child = this.Child as UIElement;

            double borderThickness = BorderWidth * 2.0;

            if (child != null)
            {
                Size size = new Size();

                bool widthAvail = constraint.Width < borderThickness;
                bool heightAvail = constraint.Height < borderThickness;

                if (!widthAvail)
                {
                    size.Width = constraint.Width - borderThickness;
                }

                if (!heightAvail)
                {
                    size.Height = constraint.Height - borderThickness;
                }

                child.Measure(size);
                Size desired = child.DesiredSize;

                if (!widthAvail)
                {
                    desired.Width += borderThickness;
                }
                if (!heightAvail)
                {
                    desired.Height += borderThickness;
                }

                return desired;
            }

            return new Size(Math.Min(borderThickness, constraint.Width), Math.Min(borderThickness, constraint.Height));
        }
    }
}
