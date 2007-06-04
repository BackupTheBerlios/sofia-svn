using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Controls;
using System.Windows.Shapes;
using System.Windows.Media;
using System.Windows;

namespace HyperTreeControl
{
    public class HtGeodesic
    {
        enum DrawType
        {
            Line = 0,
            Arc = 1
        }

        #region fields

        private static readonly double EPSILON = 1.0E-10; // epsilon

        private DrawType _type = DrawType.Line; // type of the geodesic

        private HtCoordE _za = null; // first point (Euclidian)
        private HtCoordE _zb = null; // second point (Euclidian)
        private HtCoordE _zc = null; // control point (Euclidian)
        private HtCoordE _zo = null; // center of the geodesic;

        private HtCoordS _a = null; // first point (on the screen)
        private HtCoordS _b = null; // second point (on the screen)
        private HtCoordS _c = null; // control point (on the screen)

        #endregion

        #region Constructor

        /// <summary> Constructor.
        /// </summary>
        /// <param name="za">The first point.</param>
        /// <param name="zb">The second point.</param>
        public HtGeodesic(HtCoordE za, HtCoordE zb)
        {
            _za = za;
            _zb = zb;

            _zc = new HtCoordE();
            _zo = new HtCoordE();

            _a = new HtCoordS();
            _b = new HtCoordS();
            _c = new HtCoordS();

            this.Rebuild();
        }

        #endregion

        #region Refresh

        /// <summary> Refresh the screen coordinates of this node.
        /// </summary>
        /// <param name="sOrigin">The origin of the screen plane.</param>
        /// <param name="sMax">The (xMax, yMax) point in the screen plane.</param>
        public void RefreshScreenCoordinates(HtCoordS sOrigin, HtCoordS sMax)
        {
            _a.ProjectionEtoS(_za, sOrigin, sMax);
            _b.ProjectionEtoS(_zb, sOrigin, sMax);
            _c.ProjectionEtoS(_zc, sOrigin, sMax);
        }

        #endregion

        #region Rebuild

        /// <summary> Builds the geodesic.
        /// </summary>
        public void Rebuild()
        {
            if ((Math.Abs(_za.D()) < EPSILON) ||                          // za == origin
                 (Math.Abs(_zb.D()) < EPSILON) ||                         // zb == origin
                 (Math.Abs((_za.X / _zb.X) - (_za.Y / _zb.Y)) < EPSILON)) // za = lambda.zb
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
            canvas.Background = new SolidColorBrush(Colors.Black);
            switch (_type)
            {
                case DrawType.Line:
                    Line __line = new Line();
                    __line.X1 = _a.X;
                    __line.Y1 = _a.Y;
                    __line.X2 = _b.X;
                    __line.Y2 = _b.Y;
                    canvas.Children.Add(__line);
                    break;
                case DrawType.Arc:
                    List<PathFigure> __pathFigures = new List<PathFigure>();
                    List<PathSegment> __pathSegments = new List<PathSegment>();
                    __pathSegments.Add(new QuadraticBezierSegment(new Point(_c.X, _c.Y), new Point(_b.X, _b.Y), false));
                    PathFigure __pathFigure = new PathFigure(new Point(_a.X, _a.Y), __pathSegments, false);
                    __pathFigures.Add(__pathFigure);
                    PathGeometry __pathGeometry = new PathGeometry(__pathFigures);
                    Path __path = new Path();
                    __path.Data = __pathGeometry;
                    canvas.Children.Add(__path);
                    break;
                default:
                    break;
            }
        }

        #endregion

        #region ToString

        /// <summary> Returns a string representation of the object.
        /// </summary>
        /// <returns>A string representation of the object.</returns>
        public override string ToString()
        {
            string __result = "Geodesic betweens : " +
                            "\n\t A: " + _za +
                            "\n\t B: " + _zb +
                            "\n\t is ";
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
