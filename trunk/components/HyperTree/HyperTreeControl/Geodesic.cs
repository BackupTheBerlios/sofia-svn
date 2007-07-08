using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Controls;
using System.Windows.Shapes;
using System.Windows.Media;
using System.Windows;

namespace HyperTreeControl
{
    /// <summary> The Geodesic class implements a geodesic linking to points in the Poincarre model.
    /// </summary>
    public class Geodesic
    {
        enum DrawType
        {
            Line = 0,
            Arc = 1
        }

        #region fields

        private static readonly double EPSILON = 1.0E-10; // epsilon

        private DrawType _type = DrawType.Line; // type of the geodesic

        private EuclidianVector _za = null; // first point (Euclidian)
        private EuclidianVector _zb = null; // second point (Euclidian)
        private EuclidianVector _zc = null; // control point (Euclidian)
        private EuclidianVector _zo = null; // center of the geodesic;

        private ScreenVector _a = null; // first point (on the screen)
        private ScreenVector _b = null; // second point (on the screen)
        private ScreenVector _c = null; // control point (on the screen)

        #endregion

        #region Constructor

        /// <summary> Constructor.
        /// </summary>
        /// <param name="za">The first point.</param>
        /// <param name="zb">The second point.</param>
        public Geodesic(EuclidianVector za, EuclidianVector zb)
        {
            _za = za;
            _zb = zb;

            _zc = new EuclidianVector();
            _zo = new EuclidianVector();

            _a = new ScreenVector();
            _b = new ScreenVector();
            _c = new ScreenVector();

            this.Rebuild();
        }

        #endregion

        #region Refresh

        /// <summary> Refresh the screen coordinates of this node.
        /// </summary>
        /// <param name="origin">The origin of the screen plane.</param>
        /// <param name="max">The (xMax, yMax) point in the screen plane.</param>
        public void RefreshScreenCoordinates(ScreenVector origin, ScreenVector max)
        {
            _a.ProjectionEtoS(_za, origin, max);
            _b.ProjectionEtoS(_zb, origin, max);
            _c.ProjectionEtoS(_zc, origin, max);
        }

        #endregion

        #region Rebuild

        /// <summary> Builds the geodesic.
        /// </summary>
        public void Rebuild()
        {
            if ((Math.Abs(_za.D()) < EPSILON) ||                          // za == origin
                 (Math.Abs(_zb.D()) < EPSILON) ||                         // zb == origin
                 (Math.Abs((_za.X / _zb.X) - (_za.Y / _zb.Y)) < EPSILON)) // za == lambda.zb
            {
                _type = DrawType.Line;
            }
            else
            {
                _type = DrawType.Arc;

                double __da = 1 + _za.X * _za.X + _za.Y * _za.Y;
                double __db = 1 + _zb.X * _zb.X + _zb.Y * _zb.Y;
                double __dd = 2 * (_za.X * _zb.Y - _zb.X * _za.Y);

                _zo.X = (_zb.Y * __da - _za.Y * __db) / __dd;
                _zo.Y = (_za.X * __db - _zb.X * __da) / __dd;

                double __det = (_zb.X - _zo.X) * (_za.Y - _zo.Y) - (_za.X - _zo.X) * (_zb.Y - _zo.Y);
                double __fa = _za.Y * (_za.Y - _zo.Y) - _za.X * (_zo.X - _za.X);
                double __fb = _zb.Y * (_zb.Y - _zo.Y) - _zb.X * (_zo.X - _zb.X);

                _zc.X = ((_za.Y - _zo.Y) * __fb - (_zb.Y - _zo.Y) * __fa) / __det;
                _zc.Y = ((_zo.X - _za.X) * __fb - (_zo.X - _zb.X) * __fa) / __det;
            }
        }

        #endregion

        #region Draw

        /// <summary> Draw the geodesic.
        /// </summary>
        /// <param name="canvas">The graphic canvas.</param>
        public void Draw(DrawingContext dc)
        {
            List<PathFigure> __pathFigures = new List<PathFigure>();
            List<PathSegment> __pathSegments = new List<PathSegment>();

            switch (_type)
            {
                case DrawType.Line:
                    __pathSegments.Add(new LineSegment(new Point(_b.X, _b.Y), true));
                    break;
                case DrawType.Arc:
                    __pathSegments.Add(new QuadraticBezierSegment(new Point(_c.X, _c.Y), new Point(_b.X, _b.Y), true));
                    break;
                default:
                    break;
            }

            __pathFigures.Add(new PathFigure(new Point(_a.X, _a.Y), __pathSegments, false));
            dc.DrawGeometry(null, new Pen(Brushes.LightGray, 1), new PathGeometry(__pathFigures));
        }

        #endregion

        #region ToString

        /// <summary> Returns a string representation of the object.
        /// </summary>
        /// <returns>A string representation of the object.</returns>
        public override string ToString()
        {
            string __result = "Geodesic betweens : " +
                            " A= " + _za +
                            ";B= " + _zb +
                            " is ";
            switch (_type)
            {
                case DrawType.Line:
                    __result += "a line.";
                    break;
                case DrawType.Arc:
                    __result += "an arc.";
                    break;
                default:
                    __result += "nothing ?";
                    break;
            }
            return __result;
        }

        #endregion
    }
}
