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
            //LogicGates.Train();
            Mnist.Train(18900, 0.015);
            Console.ReadKey();
        }
    }
}
