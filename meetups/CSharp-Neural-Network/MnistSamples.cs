using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharp_Neural_Network
{
    class MnistSamples
    {
        public Sample[] Training { get; private set; }
        public Sample[] Testing { get; private set; }

        public MnistSamples()
        {
            var sourceDir = new DirectoryInfo(AppDomain.CurrentDomain.BaseDirectory).Parent.Parent;
            var dataDir = Path.Combine(sourceDir.FullName, "MnistData");
            var dataFile = Path.Combine(dataDir, "Training.csv");

            var allSamples = File.ReadAllLines(dataFile)
                .Skip(1)
                .Select(line => Sample.Create(line.Substring(0,1), line.Substring(2)))
                .ToArray();
            var testCount = allSamples.Length / 10;
            Training = new Sample[allSamples.Length - testCount];
            Array.Copy(allSamples, Training, Training.Length);
            Testing = new Sample[testCount];
            if (testCount > 0)
                Array.Copy(allSamples, Training.Length, Testing, 0, Testing.Length);
        }
    }
}
