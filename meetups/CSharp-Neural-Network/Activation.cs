using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp_Neural_Network
{
    abstract class Activation
    {
        public abstract double CalcValue(double x);
        public abstract double CalcDerivative(double x);

        class SigmoidActivation : Activation
        {

            public override double CalcValue(double x)
            {
                return 1 / (1 + Math.Exp(-x));
            }

            public override double CalcDerivative(double x)
            {
                return x * (1 - x);
            }
        }

        readonly static Activation sigmoid = new SigmoidActivation();
        public static Activation Sigmoid { get { return sigmoid; } }
    }
}
