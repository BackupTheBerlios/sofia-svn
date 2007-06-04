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

        private const double EPSILON = 1.0E-10; // epsilon

        private DrawType type = DrawType.Line; // type of the geodesic

        private HtCoordE za = null; // first point (Euclidian)
        private HtCoordE zb = null; // second point (Euclidian)
        private HtCoordE zc = null; // control point (Euclidian)
        private HtCoordE zo = null; // center of the geodesic;

        private HtCoordS a = null; // first point (on the screen)
        private HtCoordS b = null; // second point (on the screen)
        private HtCoordS c = null; // control point (on the screen)


        /* --- Constructor --- */

        /**
         * Constructor.
         *
         * @param za       the first point
         * @param zb       the second point
         */
        public HtGeodesic(HtCoordE za, HtCoordE zb)
        {
            this.za = za;
            this.zb = zb;

            zc = new HtCoordE();
            zo = new HtCoordE();

            a = new HtCoordS();
            b = new HtCoordS();
            c = new HtCoordS();

            this.Rebuild();
        }


        /* --- Refresh --- */

        /**
         * Refresh the screen coordinates of this node.
         *
         * @param sOrigin   the origin of the screen plane
         * @param sMax      the (xMax, yMax) point in the screen plane
         */
        public void RefreshScreenCoordinates(HtCoordS sOrigin, HtCoordS sMax)
        {
            a.ProjectionEtoS(za, sOrigin, sMax);
            b.ProjectionEtoS(zb, sOrigin, sMax);
            c.ProjectionEtoS(zc, sOrigin, sMax);
        }


        /* --- Rebuild --- */

        /**
         * Builds the geodesic.
         */
        public void Rebuild()
        {
            if ((Math.Abs(za.D()) < EPSILON) ||                       // za == origin
                 (Math.Abs(zb.D()) < EPSILON) ||                       // zb == origin
                 (Math.Abs((za.X / zb.X) - (za.Y / zb.Y)) < EPSILON)) // za = lambda.zb
            {
                type = DrawType.Line;
            }
            else
            {
                type = DrawType.Arc;

                double da = 1 + za.X * za.X + za.Y * za.Y;
                double db = 1 + zb.X * zb.X + zb.Y * zb.Y;
                double dd = 2 * (za.X * zb.Y - zb.X * za.Y);

                zo.X = (zb.Y * da - za.Y * db) / dd;
                zo.Y = (za.X * db - zb.X * da) / dd;

                double det = (zb.X - zo.X) * (za.Y - zo.Y) - (za.X - zo.X) * (zb.Y - zo.Y);
                double fa = za.Y * (za.Y - zo.Y) - za.X * (zo.X - za.X);
                double fb = zb.Y * (zb.Y - zo.Y) - zb.X * (zo.X - zb.X);

                zc.X = ((za.Y - zo.Y) * fb - (zb.Y - zo.Y) * fa) / det;
                zc.Y = ((zo.X - za.X) * fb - (zo.X - zb.X) * fa) / det;
            }
        }


        /* --- Draw --- */

        /**
         * Draws this geodesic.
         *
         * @param g    the graphic context
         */
        public void Draw(Canvas g)
        {
            g.Background = new SolidColorBrush(Colors.Black);
            switch (type)
            {
                case DrawType.Line:
                    Line line = new Line();
                    line.X1 = a.X;
                    line.Y1 = a.Y;
                    line.X2 = b.X;
                    line.Y2 = b.Y;
                    g.Children.Add(line);
                    break;
                case DrawType.Arc:
                    List<PathFigure> __pathFigures = new List<PathFigure>();
                    List<PathSegment> __pathSegments = new List<PathSegment>();
                    __pathSegments.Add(new QuadraticBezierSegment(new Point(c.X, c.Y), new Point(b.X, b.Y), false));
                    PathFigure __pathFigure = new PathFigure(new Point(a.X, a.Y), __pathSegments, false);
                    __pathFigures.Add(__pathFigure);
                    PathGeometry __pathGeometry = new PathGeometry(__pathFigures);
                    Path __path = new Path();
                    __path.Data = __pathGeometry;
                    g.Children.Add(__path);
                    break;
                default:
                    break;
            }
        }


        /* --- ToString --- */

        /**
         * Returns a string representation of the object.
         *
         * @return    a String representation of the object
         */
        public override string ToString()
        {
            string result = "Geodesic betweens : " +
                            "\n\t A: " + za +
                            "\n\t B: " + zb +
                            "\n\t is ";
            switch (type)
            {
             case   DrawType.Line:
                    result += "a line.";
                    break;
                case DrawType.Arc:
                    result += "an arc.";
                    break;
                default:
                    result += "nothing ?";
                    break;
            }
            return result;
        }

    }
}
