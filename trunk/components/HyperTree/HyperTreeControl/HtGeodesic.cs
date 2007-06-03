using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Controls;
using System.Windows.Shapes;
using System.Windows.Media;
using System.Windows;

namespace HyperTreeControl
{
    class HtGeodesic
    {
        enum DrawType
        {
            Line = 0,
            Arc = 1
        }

        private static const double EPSILON = 1.0E-10; // epsilon

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
        HtGeodesic(HtCoordE za, HtCoordE zb)
        {
            this.za = za;
            this.zb = zb;

            zc = new HtCoordE();
            zo = new HtCoordE();

            a = new HtCoordS();
            b = new HtCoordS();
            c = new HtCoordS();

            rebuild();
        }


        /* --- Refresh --- */

        /**
         * Refresh the screen coordinates of this node.
         *
         * @param sOrigin   the origin of the screen plane
         * @param sMax      the (xMax, yMax) point in the screen plane
         */
        void refreshScreenCoordinates(HtCoordS sOrigin, HtCoordS sMax)
        {
            a.ProjectionEtoS(za, sOrigin, sMax);
            b.ProjectionEtoS(zb, sOrigin, sMax);
            c.ProjectionEtoS(zc, sOrigin, sMax);
        }


        /* --- Rebuild --- */

        /**
         * Builds the geodesic.
         */
        void rebuild()
        {
            if ((Math.abs(za._d()) < EPSILON) ||                       // za == origin
                 (Math.abs(zb._d()) < EPSILON) ||                       // zb == origin
                 (Math.abs((za.x / zb.x) - (za.y / zb.y)) < EPSILON)) // za = lambda.zb
            {
                type = DrawType.Line;
            }
            else
            {
                type = DrawType.Arc;

                double da = 1 + za.x * za.x + za.y * za.y;
                double db = 1 + zb.x * zb.x + zb.y * zb.y;
                double dd = 2 * (za.x * zb.y - zb.x * za.y);

                zo.x = (zb.y * da - za.y * db) / dd;
                zo.y = (za.x * db - zb.x * da) / dd;

                double det = (zb.x - zo.x) * (za.y - zo.y) - (za.x - zo.x) * (zb.y - zo.y);
                double fa = za.y * (za.y - zo.y) - za.x * (zo.x - za.x);
                double fb = zb.y * (zb.y - zo.y) - zb.x * (zo.x - zb.x);

                zc.x = ((za.y - zo.y) * fb - (zb.y - zo.y) * fa) / det;
                zc.y = ((zo.x - za.x) * fb - (zo.x - zb.x) * fa) / det;
            }
        }


        /* --- Draw --- */

        /**
         * Draws this geodesic.
         *
         * @param g    the graphic context
         */
        void draw(Canvas g)
        {
            g.Background = new SolidColorBrush(Colors.Black);
            switch (type)
            {
                case DrawType.Line:
                    Line line = new Line();
                    line.X1 = a.x;
                    line.Y1 = a.y;
                    line.X2 = b.x;
                    line.Y2 = b.y;
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
