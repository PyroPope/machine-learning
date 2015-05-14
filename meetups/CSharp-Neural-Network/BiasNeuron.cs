using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp_Neural_Network
{
    class BiasNeuron : Neuron
    {
        public BiasNeuron() : base(null)
        {
            Value = 1;
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
