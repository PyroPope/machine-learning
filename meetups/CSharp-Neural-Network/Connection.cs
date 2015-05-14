using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp_Neural_Network
{
    class Connection
    {
        readonly Neuron from;
        readonly Neuron to;
        double weight;

        public Connection(Neuron from, Neuron to, double weight)
        {
            from.AddOutboundConnection(this);
            this.from = from;

            to.AddInboundConnection(this);
            this.to = to;

            this.weight = weight;
        }

        public double Value
        {
            get { return from.Value * weight; }
        }
    }
}
