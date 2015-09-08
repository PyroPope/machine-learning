#include <vector>
#include <list>
#include <string>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <algorithm>
#include <iterator>

#include <boost/cstdlib.hpp>

double distance(const std::vector<std::size_t> &lhs, const std::vector<std::size_t> &rhs, const std::vector<std::vector<double>> &distances)
{
  double sum_d = 0.0;
  std::size_t n = 0;
  for(std::size_t x : lhs) {
    for(std::size_t y : rhs) {
      //std::cerr << "distances[" << x << "][" << y << "]=" << distances.at(x).at(y) << std::endl;
      sum_d += distances.at(x).at(y);
      ++n;
    }
  }
  //std::cerr << "sum_d=" << sum_d << std::endl;
  return sum_d / n;
}

int main()
{
  std::cerr.precision(3);
  //std::cerr << std::fixed;

  std::size_t n;
  std::cin >> n;
  //std::cerr << "n=" << n << std::endl;

  std::vector<std::vector<double>> distances(n, std::vector<double>(n, 0.0));
  std::string line;
  std::getline(std::cin, line);
  std::size_t l = 0;
  while(true) {
    std::getline(std::cin, line);
    if(!std::cin) {
      break;
    }

    std::istringstream linestream(line);
    for(std::size_t i = 0; i < n; ++i) {
      double x;
      linestream >> x;
      distances.at(l).at(i) = x;
    }
    ++l;
  }

  //std::cerr << "distances=[" << std::endl;
  //for(const std::vector<double> &row : distances) {
    //std::cerr << "  [ ";
    //std::copy(row.begin(), row.end(), std::ostream_iterator<double>(std::cerr, " "));
    //std::cerr << "]" << std::endl;
  //}
  //std::cerr << "]" << std::endl;

  std::list<std::vector<std::size_t>> clusters;
  typedef std::list<std::vector<std::size_t>>::iterator clusters_iterator;
  for(std::size_t i = 0; i < n; ++i) {
    clusters.push_back(std::vector<std::size_t>(1, i));
  }

  //for(const auto &cluster: clusters) {
    //std::cerr << "cluster=[ ";
    //std::copy(cluster.begin(), cluster.end(), std::ostream_iterator<std::size_t>(std::cerr, " "));
    //std::cerr << "]" << std::endl;
  //}

  while(clusters.size() > 1) {
    const clusters_iterator c_end = clusters.end();
    double min_d = std::numeric_limits<double>::max(); // minimum distance
    std::pair<clusters_iterator, clusters_iterator> min_c = std::make_pair(c_end, c_end); // indices of two clusters with minimum distance
    for(clusters_iterator c_i = clusters.begin(); c_i != c_end; ++c_i) {
      for(clusters_iterator c_j = c_i; c_j != c_end; ++c_j) {
        if(c_i == c_j) {
          continue;
        }
        double d = distance(*c_i, *c_j, distances);
        if(d <= min_d) {
          min_d = d;
          min_c = std::make_pair(c_i, c_j);
        }
      }
    }
    //std::cerr << "min_d=" << min_d << std::endl;

    //std::cerr << "min_c.first=[ ";
    //std::copy(min_c.first->begin(), min_c.first->end(), std::ostream_iterator<std::size_t>(std::cerr, " "));
    //std::cerr << "]" << std::endl;
    //std::cerr << "min_c.second=[ ";
    //std::copy(min_c.second->begin(), min_c.second->end(), std::ostream_iterator<std::size_t>(std::cerr, " "));
    //std::cerr << "]" << std::endl;

    std::copy(min_c.second->begin(), min_c.second->end(), std::back_inserter(*min_c.first));
    //std::sort(min_c.first->begin(), min_c.first->end());

    //std::cerr << "min_c.first=[ ";
    //std::copy(min_c.first->begin(), min_c.first->end(), std::ostream_iterator<std::size_t>(std::cerr, " "));
    //std::cerr << "]" << std::endl;
    //std::cerr << "min_c.second=[ ";
    //std::copy(min_c.second->begin(), min_c.second->end(), std::ostream_iterator<std::size_t>(std::cerr, " "));
    //std::cerr << "]" << std::endl;

    clusters.erase(min_c.second);

    //for(const auto &cluster: clusters) {
      //std::cerr << "cluster=[ ";
      //std::copy(cluster.begin(), cluster.end(), std::ostream_iterator<std::size_t>(std::cerr, " "));
      //std::cerr << "]" << std::endl;
    //}
    for(std::size_t i: *min_c.first) {
      std::cout << (i + 1) << " ";
    }
    std::cout << std::endl;
    //std::cout << std::endl;
  }

  return boost::exit_success;
}


// vim: set ts=2 sw=2 et:


