using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp_Neural_Network
{
    class Network
    {
        readonly InputLayer inputLayer;
        readonly OutputLayer outputLayer;

        public Network(Activation activation, int inputSize, int outputSize)
        {
            this.inputLayer = new InputLayer(inputSize);
            this.outputLayer = new OutputLayer(activation, outputSize);
            ConnectLayers(inputLayer, outputLayer);
        }

        static Random rnd = new Random();
        double GetRandomWeight()
        {
            return (rnd.NextDouble() * 2) - 1;
        }

        void ConnectLayers(Layer fromLayer, Layer toLayer)
        {
            foreach (var from in fromLayer.Neurons)
                foreach (var to in toLayer.Neurons)
                    new Connection(from, to, GetRandomWeight());
        }

        public double[] FeedForward(double[] input)
        {
            inputLayer.SetValues(input);
            outputLayer.FeedForward();
            return outputLayer.Values;
        }
    }
}
