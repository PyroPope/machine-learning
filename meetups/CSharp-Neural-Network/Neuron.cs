﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp_Neural_Network
{
    abstract class Neuron
    {
        protected readonly Activation activation;
        public Neuron(Activation activation)
        {
            this.activation = activation;
        }
 
        readonly protected List<Connection> inboundConnections = new List<Connection>();
        public void AddInboundConnection(Connection connection)
        {
            inboundConnections.Add(connection);
        }


        readonly protected List<Connection> outboundConnections = new List<Connection>();
        public void AddOutboundConnection(Connection connection)
        {
            outboundConnections.Add(connection);
        }
        public double Value { get; protected set; }

        public double Error { get; protected set; }
     
        public abstract void FeedForward();

        protected double CalcValue()
        {
            double sum = 0;
            foreach (var conn in inboundConnections)
                sum += conn.Value;
            return activation.CalcValue(sum);
        }       

        public abstract void PropagateBack();

    }

}