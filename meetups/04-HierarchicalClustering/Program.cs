using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace HierarchicalClustering
{
    class Cluster
    {
        double[,] distances = new double[,]
                {{0.00,0.74,0.85,0.54,0.83,0.92,0.89},
                {0.74,0.00,1.59,1.35,1.20,1.48,1.55},
                {0.85,1.59,0.00,0.63,1.13,0.69,0.73},
                {0.54,1.35,0.63,0.00,0.66,0.43,0.88},
                {0.83,1.20,1.13,0.66,0.00,0.72,0.55},
                {0.92,1.48,0.69,0.43,0.72,0.00,0.80},
                {0.89,1.55,0.73,0.88,0.55,0.80,0.00}};

        List<int> points = new List<int>();

        public void AddPoint(int point)
        {
            points.Add(point);
        }

        public double Distance(Cluster other)
        {
            var allDistances =
                from localPoint in points
                from otherPoint in other.points
                select distances[localPoint, otherPoint];
            return allDistances.Average();
        }

        public Cluster GetCombined(Cluster other)
        {
            var combined = new Cluster();
            foreach (var point in points.Union(other.points))
                combined.AddPoint(point);
            return combined;
        }

        public void WriteOut()
        {
            Console.WriteLine(string.Join(", ", points));
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello");

            var clusters = new List<Cluster>();
            for (int i = 0; i < 7; i++)
            {
                var cluster = new Cluster();
                cluster.AddPoint(i);
                clusters.Add(cluster);
            }

            while (clusters.Count > 1)
            {
                var allPairs = GetAllPairs(clusters);
                var nearestPair = allPairs.OrderBy(pair => pair.Item1.Distance(pair.Item2)).First();
                var combinedCluster = nearestPair.Item1.GetCombined(nearestPair.Item2);

                combinedCluster.WriteOut();
                clusters.Remove(nearestPair.Item1);
                clusters.Remove(nearestPair.Item2);
                clusters.Add(combinedCluster);
            }
            Console.ReadKey();

        }

        public static IEnumerable<Tuple<Cluster, Cluster>> GetAllPairs(IEnumerable<Cluster> clusters)
        {
            var arrayOfClusters = clusters.ToArray();
            var length = arrayOfClusters.Length;
            for (int i = 0; i < length; i++)
                for (int j = i + 1; j < length; j++ )
                    yield return Tuple.Create(arrayOfClusters[i], arrayOfClusters[j]);
        }
    }
}
