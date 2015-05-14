using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp_Neural_Network
{
    class Program
    {
        static void Main(string[] args)
        {
            var input1 = new double[] { 1, 1 };
            var input2 = new double[] { 0, 1 };
            var net = new Network(Activation.Sigmoid, 2, 1);
            var output = net.FeedForward(input1)[0];
            output = net.FeedForward(input1)[0];
            output = net.FeedForward(input2)[0];
            output = net.FeedForward(input2)[0];
            output = net.FeedForward(input1)[0];

            Console.ReadKey();
        }
    }
}
