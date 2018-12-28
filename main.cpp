#include <iostream>
#include <vector>
#include <string>
#include <cmath>
#include <limits>
#include <chrono>
#include <array>

using std::string;
using std::cout;
using std::endl;
using Clock = std::chrono::high_resolution_clock;
using std::chrono::duration;

class Perf {
 public:
  explicit Perf(const string& msg) : beg_(Clock::now()), msg_(msg) {}
  explicit Perf(string&& msg) : beg_(Clock::now()), msg_(std::move(msg)) {}

  inline void elaspe() {
    auto end = Clock::now();
    duration<double> diff = end - beg_;
    cout << "[" << msg_ << "]: " << diff.count() * 1000.0 << "ms" << endl;
  }

  inline double tik_mili_sec() {
    auto end = Clock::now();
    duration<double> diff = end - beg_;
    return diff.count() * 1000.0;
  }

  ~Perf() {
    elaspe();
  }

 private:
  using Time = decltype(Clock::now());
  Time beg_;
  string msg_;
};

template<uint64_t v>
struct power_10 {
  static const uint64_t value = power_10<v-1>::value * 10ULL;
};

template<>
struct power_10<0> {
  static const uint64_t value = 1ULL;
};

class BigUInt {
 public:
  BigUInt() : value_(1, 0) {}

  template<typename T>
  BigUInt(const T &v) : value_(1, v) {}

  BigUInt(const BigUInt &v) : value_(v.value_) {}

  BigUInt(BigUInt &&v) : value_(std::move(v.value_)) {}

  BigUInt &operator=(const BigUInt &v) {
    value_ = v.value_;
    return *this;
  }

  BigUInt &operator=(BigUInt &&v) {
    value_ = std::move(v.value_);
    return *this;
  }

  BigUInt operator+(const BigUInt &v) {
    const auto this_length = value_.size();
    const auto that_length = v.value_.size();
    const auto min_length = std::min(this_length, that_length);

    uint64_t idx = 0;
    uint64_t j = 0;
    BigUInt r;
    r.value_.clear();
    while (idx < min_length) {
      auto this_v = value_[idx];
      auto that_v = v.value_[idx];
      auto sum_v = this_v + that_v + j;
      r.value_.emplace_back(sum_v % max_each_);
      j = sum_v / max_each_;
      ++idx;
    }
    while (idx < this_length) {
      auto this_v = value_[idx];
      auto sum_v = this_v + j;
      r.value_.emplace_back(sum_v % max_each_);
      j = sum_v / max_each_;
      ++idx;
    }
    while (idx < that_length) {
      auto that_v = v.value_[idx];
      auto sum_v = that_v + j;
      r.value_.emplace_back(sum_v % max_each_);
      j = sum_v / max_each_;
      ++idx;
    }
    if (j != 0) {
      r.value_.emplace_back(j);
    }
    return r;
  }

  BigUInt operator*(const BigUInt &v) const {
    const auto &this_value = value_;
    const auto &that_value = v.value_;
    const auto this_size = this_value.size();
    const auto that_size = that_value.size();
    BigUInt r;
    r.value_.resize(this_size + that_size, 0);
    for (size_t i = 0; i < this_size; ++i) {
      uint64_t this_v = this_value[i];
      for (size_t j = 0; j < that_size; ++j) {
        uint64_t that_v = that_value[j];
        uint64_t mul = this_v * that_v + r.value_[i + j];
        r.value_[i + j] = mul % max_each_;
        r.value_[i + j + 1] += mul / max_each_;
      }
    }
    while (r.value_.size() > 1 && r.value_.back() == 0) {
      r.value_.pop_back();
    }
    return r;
  }

  BigUInt &operator+=(const BigUInt &v) {
    auto r = this->operator+(v);
    value_ = std::move(r.value_);
    return *this;
  }
  BigUInt &operator*=(const BigUInt &v) {
    auto r = this->operator*(v);
    value_ = std::move(r.value_);
    return *this;
  }

  BigUInt &operator++() {
    auto r = this->operator+(1);
    value_ = std::move(r.value_);
    return *this;
  }

  BigUInt operator++(int) {
    BigUInt r = *this;
    auto r1 = this->operator*(1);
    value_ = std::move(r1.value_);
    return r;
  }

  bool operator<(const BigUInt &v) {
    if (value_.size() != v.value_.size()) {
      return value_.size() < v.value_.size();
    }
    auto s = value_.size();
    for (uint64_t i = 0; i < s; ++i) {
      if (value_[i] != v.value_[i]) {
        return value_[i] < v.value_[i];
      }
    }
    return false;
  }

  bool operator==(const BigUInt &v) {
    if (value_.size() != v.value_.size()) {
      return false;
    }
    auto s = value_.size();
    for (uint64_t i = 0; i < s; ++i) {
      if (value_[i] != v.value_[i]) {
        return false;
      }
    }
    return true;
  }

  bool operator<=(const BigUInt &v) {
    return this->operator==(v) || this->operator<(v);
  }

  bool operator>(const BigUInt &v) {
    return !this->operator==(v) && !this->operator<(v);
  }

  bool operator>=(const BigUInt &v) {
    return !this->operator<(v);
  }
 protected:
  friend std::ostream &operator<<(std::ostream &s, const BigUInt &i);
  std::vector<uint64_t> value_;


  // static constexpr uint64_t width_each_ = 2;
  static constexpr uint64_t width_each_ = static_cast<uint64_t>(std::numeric_limits<uint64_t>::digits10 / 2);
  static constexpr uint64_t max_each_ = power_10<width_each_>::value;
};

std::ostream &operator<<(std::ostream &s, const BigUInt &i) {
  int64_t index = i.value_.size() - 1;
  s << std::to_string(i.value_[index]);
  --index;
  for (; index >= 0; --index) {
    std::string n_str = std::to_string(i.value_[index]);
    s << std::string(BigUInt::width_each_ - n_str.length(), '0').append(n_str);
  }
  return s;
}

template<typename T>
class Mat22 {
 public:
  Mat22() = default;

  template<typename U>
  Mat22(const U &v) : mat_({v, v, v, v}) {}

  template<typename U>
  Mat22(const U &v11, const U &v12, const U &v21, const U &v22) : mat_({v11, v12, v21, v22}) {}

  Mat22(const Mat22 &v) : mat_(v.mat_) {}

  Mat22(Mat22 &&v) : mat_(std::move(v.mat_)) {}

  Mat22 &operator=(const Mat22 &v) {
    mat_ = v.mat_;
    return *this;
  }

  Mat22 &operator=(Mat22 &&v) {
    mat_ = std::move(v.mat_);
    return *this;
  }

  const T &operator()(int i, int j) const {
    return mat_[i * 2 + j];
  }

  T &operator()(int i, int j) {
    return mat_[i * 2 + j];
  }

  Mat22 operator*(const Mat22 &v) const {
    const auto &t = *this;
    return Mat22(t(0, 0) * v(0, 0) + t(0, 1) * v(1, 0),
        t(0, 0) * v(0, 1) + t(0, 1) * v(1, 1),
        t(1, 0) * v(0, 0) + t(1, 1) * v(1, 0),
        t(1, 0) * v(0, 1) + t(1, 1) * v(1, 1));
  }

  Mat22 &operator*=(const Mat22 &v) {
    auto r = this->operator*(v);
    mat_ = std::move(r.mat_);
    return *this;
  }
 protected:
  std::array<T, 4> mat_;
};

template<typename T>
std::ostream &operator<<(std::ostream &s, const Mat22<T> &m) {
  s << m(0, 0) << ", " << m(0, 1) << std::endl << m(1, 0) << ", " << m(1, 1);
  return s;
}

template<typename T, typename U>
T fast_power(const T &v, const U &times) {
  if (times == 0) {
    return T(1);
  }
  std::vector<std::pair<U , T> > times_cache = {
      {1, v}
  };
  U cur_times = 1;
  T cur_v = v;
  while (true) {
    if (2*cur_times > times) {
      break;
    }
    cur_v *= cur_v;
    cur_times *= 2;
    times_cache.emplace_back(cur_times, cur_v);
  }
  U rest_times = times - cur_times;
  T r = std::move(cur_v);
  size_t cache_index = times_cache.size() - 2;
  while (rest_times != 0) {
    const auto &item = times_cache[cache_index];
    if (item.first <= rest_times) {
      r *= item.second;
      rest_times -= item.first;
    }
    --cache_index;
  }
  return r;
}

// n: 1 based
BigUInt fibnacci_fast(const uint64_t n) {
  if (n == 0) {
    return 1;
  } else if (n == 1) {
    return 1;
  }
  using Mat = Mat22<BigUInt>;
  Mat m(1, 1, 1, 0);
  auto r = fast_power(m, n-1);
  return r(0, 0);
}

#define N 100
#define REP 10

int main() {
  // {
  //   Perf p{"pow"};
  //   BigUInt r = 1;
  //   for (int n = 0; n < REP; ++n) {
  //     BigUInt a = 2;
  //     BigUInt b = N;
  //     r = 1;
  //     for (BigUInt i = 0; i < b; ++i) {
  //       r *= a;
  //     }
  //   }
  //   std::cout << p.tik_mili_sec() / REP << "ms" << std::endl;
  //   std::cout << r << std::endl;
  // }
  // std::cout << "=========" << std::endl;
  // {
  //   Perf p{"fast pow"};
  //   BigUInt r = 1;
  //   for (int n = 0; n < REP; ++n) {
  //     BigUInt a = 2;
  //     r = fast_power(a, N);
  //   }
  //   std::cout << p.tik_mili_sec() / REP << "ms" << std::endl;
  //   std::cout << r << std::endl;
  // }
  // std::cout << "=========" << std::endl;
  // {
  //   Perf p{"double pow"};
  //   double r = 0;
  //   for (int n = 0; n < REP; ++n) {
  //     r = std::pow(2, N);
  //   }
  //   std::cout << p.tik_mili_sec() / REP << "ms" << std::endl;
  //   std::cout << r << std::endl;
  // }
  std::cout << "=========" << std::endl;
  {
    Perf p{"fast fibnacci"};
    BigUInt r;
    for (int n = 0; n < REP; ++n) {
      r = fibnacci_fast(1000000);
    }
    std::cout << p.tik_mili_sec() / REP << "ms" << std::endl;
    std::cout << r << std::endl;
  }
  return 0;
}