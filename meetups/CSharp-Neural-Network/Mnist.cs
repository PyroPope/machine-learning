using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp_Neural_Network
{
    class Mnist
    {
        public static void Train(int trainingCount, double learnRate)
        {
            Console.WriteLine("Starting MNIST trainging");
            Console.Write("Loading samples... ");
            var mnistSamples = new MnistSamples();
            Console.WriteLine("done");

            Console.Write("Creating network... ");
            var net = new Network(784, new[] { 500, 100 }, 10, learnRate);
            Console.WriteLine("done");

            var training = mnistSamples.Training.Take(trainingCount).ToArray();
            var testing = mnistSamples.Testing.Take(training.Length).ToArray();

            var trainer = new Trainer(net, training, CheckCorrect, testing);

        }

        static bool CheckCorrect(double[] target, double[] output)
        {

        }

       
    }
}
