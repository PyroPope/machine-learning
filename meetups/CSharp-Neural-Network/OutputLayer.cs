using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp_Neural_Network
{
    class OutputLayer : Layer
    {
        readonly OutputNeuron[] neurons;

        public OutputLayer(Activation activation, int size)
        {
            var neurons = new OutputNeuron[size];
            for (int i = 0; i < size; i++)
                neurons[i] = new OutputNeuron(activation);
            this.neurons = neurons;
        }

        public override Neuron[] Neurons
        {
            get { return neurons; }
        }

        public double[] Values
        {
            get
            {
                return neurons.Select(n => n.Value).ToArray();
            }
        }

        class OutputNeuron : Neuron
        {
            public OutputNeuron(Activation activation)
                : base(activation)
            { }

            public override void FeedForward()
            {
                Value = CalcValue();
            }

            public override void PropagateBack()
            {
                throw new NotImplementedException();
            }
        }

    }

}
