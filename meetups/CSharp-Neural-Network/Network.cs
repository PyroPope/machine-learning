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
        readonly Activation activation;

        public Network(Activation activation, TrainingInfo trainInfo, int inputSize, int outputSize)
        {
            this.activation = activation;
            this.inputLayer = new InputLayer(inputSize);
            this.outputLayer = new OutputLayer(activation, trainInfo, outputSize);
            ConnectLayers(inputLayer, outputLayer);
        }

        void ConnectLayers(Layer fromLayer, Layer toLayer)
        {
            foreach (var from in fromLayer.Neurons)
                foreach (var to in toLayer.Neurons)
                    new Connection(from, to, activation.GetRandomWeight());
        }

        public double[] FeedForward(double[] input)
        {
            inputLayer.SetInputValues(input);
            outputLayer.FeedForward();
            return outputLayer.Values;
        }

        public void PropagateBack(double[] target)
        {
            outputLayer.SetTargetValues(target);
            outputLayer.PropagateBack();
        }

    }
}
