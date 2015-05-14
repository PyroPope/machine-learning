using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp_Neural_Network
{

    class InputLayer : Layer
    {
        readonly Neuron[] neurons;
        public InputLayer(int size)
        {
            var neurons = new Neuron[size + 1];
            for (int i = 0; i < size; i++)
                neurons[i] = new InputNeuron();
            neurons[size] = new BiasNeuron();
            this.neurons = neurons;
        }

        public override Neuron[] Neurons
        {
            get { return neurons; }
        }

        public void SetValues(double[] input)
        {
            for (int i = 0; i < input.Length; i++)
                ((InputNeuron)neurons[i]).SetValue(input[i]);
        }

        class InputNeuron : Neuron
        {
            public InputNeuron() : base(null)
            {}

            public void SetValue(double value)
            {
                Value = value;
            }

            public override void FeedForward()
            {
                // do nothing
            }

            public override void PropagateBack()
            {
                // do nothing
            }

        }

    }
}
