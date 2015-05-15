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
            var orCases = new[]{
                Sample.Create("0 0", "0"),
                Sample.Create("0 1", "1"),
                Sample.Create("1 0", "1"),
                Sample.Create("1 1", "1"),
            };

            var andCases = new[]{
                Sample.Create("0 0", "0"),
                Sample.Create("0 1", "0"),
                Sample.Create("1 0", "0"),
                Sample.Create("1 1", "1"),
            };

            var nandCases = new[]{
                Sample.Create("0 0", "1"),
                Sample.Create("0 1", "1"),
                Sample.Create("1 0", "1"),
                Sample.Create("1 1", "0"),
            };

            var xorCases = new[]{
                Sample.Create("0 0", "0"),
                Sample.Create("0 1", "1"),
                Sample.Create("1 0", "1"),
                Sample.Create("1 1", "0"),
            };

            var trainInfo = new TrainingInfo(0.1);
            var inputSize = 2;
            var hiddenSizes = new int[] { 2 };
            var outputSize = 1;
            var net = new Network(Activation.Sigmoid, trainInfo, inputSize, hiddenSizes, outputSize);

            while (true)
            {
                foreach (var s in xorCases)
                {
                    var output = net.FeedForward(s.input);
                    Console.WriteLine("{0} - {1:n3}", s.target[0], output[0]);
                    net.PropagateBack(s.target);
                }
                if (Console.KeyAvailable)
                {
                    Console.ReadKey();
                    Console.ReadKey();
                }
                Console.WriteLine("============");
            }

        }
    }
}
