/*
 * valarray_PhaseB_unittests.cpp
 * EPL - Spring 2015
 */

#include <chrono>
#include <complex>
#include <cstdint>
#include <future>
#include <iostream>
#include <stdexcept>

#include "InstanceCounter.h"
#include "Valarray.h"
#include "Vector.h"

#include "gtest/gtest.h"

using std::cout;
using std::endl;
using std::string;
using std::complex;

int InstanceCounter::counter = 0;

using namespace epl;

template <typename X, typename Y>
bool match(X x, Y y) {
    double d = x - y;
    if (d < 0) { d = -d; }
    return d < 1.0e-9; // not really machine epsilon, but close enough
}
/*********************************************************************/
// Phase B Tests
/*********************************************************************/

#if defined(PHASE_B1_0) | defined(PHASE_B)
TEST(PhaseB1, Sqrt) {
    valarray<int> v1(10);
    valarray<int> v4 = 4 + v1;

    valarray<float> v5 = v4.sqrt();
    for (uint64_t i = 0; i<10; i++) {
        EXPECT_EQ(2.0, v5[i]);
    }
}
#endif

#if defined(PHASE_B1_1) | defined(PHASE_B)
TEST(PhaseB1, Apply) {
    valarray<int> v1(10);
    valarray<int> v4 = 4 + v1;

    valarray<int> v7 = v4.apply(std::negate<int>());

    EXPECT_EQ(10, v7.size());

    for (uint64_t i = 0; i<10; i++) {
        EXPECT_EQ(-4, v7[i]);
    }
}
#endif

#if defined(PHASE_B1_2) | defined(PHASE_B)
TEST(PhaseB1, Accumulate) {
    valarray<int> v1{1, 2, 3, 4, 5};
    int sum = v1.accumulate(std::plus<int>());
    int product = v1.accumulate(std::multiplies<int>());
    EXPECT_EQ(15, sum);
    EXPECT_EQ(120, product);
}
#endif

#if defined(PHASE_B1_3) | defined(PHASE_B)
TEST(PhaseB1, Lazy) {
    // lazy evaluation test
    valarray <double> v1, v2, v3, v4;
    for (int i = 0; i<10; ++i) {
        v1.push_back(1.0);
        v2.push_back(1.0);
        v3.push_back(1.0);
        v4.push_back(1.0);
    }
    int cnt = InstanceCounter::counter;
    v1 + v2 - (v3 * v4);
    EXPECT_EQ(cnt, InstanceCounter::counter);

    valarray<double> ans(10);
    ans = v1 + v2 - (v3*v4);
    EXPECT_TRUE(match(ans[3], (v1[3] + v2[3] - (v3[3] * v4[3]))));
}
#endif
#if defined(PHASE_B1_4) | defined(PHASE_B)
TEST(PhaseB1, ProxyIterator) {
	// lazy evaluation test
	valarray<int> v4{ 1, 2, 3 };
	valarray<int> v7{ 3, 2, 1 };
	for (auto val : (v4 + v7)) {
		EXPECT_EQ(4, val);
	}

	valarray < complex<int>> v0{ { 3, 3 },{ 2, 3 },{ 1, 3 } };
	valarray<int> v5{ 1, 2, 3 };
	for (auto val : (v0 + v5)) {
		EXPECT_EQ(complex<int>(4, 3), val);
	}
}
#endif
#if defined(PHASE_B1_5) | defined(PHASE_B)
TEST(PhaseB1, ProxyAccumulate) {
	// lazy evaluation test
	valarray<int> v4{ 1, 2, 3 };
	valarray<int> v7{ 3, 2, 1 };
	auto s = v4 + v7;
	int result = s.accumulate(std::plus<int>{});
	EXPECT_EQ(12, result);

}
#endif
#if defined(PHASE_B1_6) | defined(PHASE_B)
TEST(PhaseB1, ProxySum) {
	// lazy evaluation test
	valarray<int> v4{ 1, 2, 3 };
	valarray<int> v7{ 3, 2, 1 };
	auto s = v4 + v7;
	int result = s.sum();
	EXPECT_EQ(12, result);

}
#endif
#if defined(PHASE_B1_7) | defined(PHASE_B)
TEST(PhaseB1, ProxyApply) {
	// lazy evaluation test
	valarray<int> v4{ 1, 2, 3 };
	valarray<int> v7{ 3, 2, 1 };
	auto s = v4 + v7;
	auto prod = s.apply(std::negate<int>{});
	for (auto p : prod) {
		EXPECT_EQ(-4, p);
	}

}
#endif
#if defined(PHASE_B1_7) | defined(PHASE_B)
TEST(PhaseB1, ProxySqrt) {
	// lazy evaluation test
	valarray<int> v4{ 2, 3, 4 };
	valarray<int> v7{ 3, 2, 1 };
	auto s = (v4 + v7).sqrt();
	for (auto p : s) {
		ASSERT_TRUE(match(sqrt(5), p));
	}

}
#endif
#if defined(PHASE_B1_8) | defined(PHASE_B)
TEST(PhaseB1, SqrtComplex) {
	// lazy evaluation test
	valarray<complex<double>> v4{ { 0, 8 },{ 0, 8 },{ 0, 8 } };
	auto ans = v4.sqrt();
	for (auto p : ans) {
		EXPECT_EQ(complex<double>(2, 2), p);
	}

	valarray<complex<double>> v0{ { 0, 4 },{ 0, 4 },{ 0, 4 } };
	valarray<complex<double>> v1{ { 0, 4 },{ 0, 4 },{ 0, 4 } };
	auto s = (v0 + v1).sqrt();
	auto s2 = (v0 + v1).sqrt() + 3;
	for (auto p : s) {
		EXPECT_EQ(complex<double>(2, 2), p);
	}

}
#endif
#if defined(PHASE_B1_8) | defined(PHASE_B)
TEST(PhaseB1, VectorAssn) {
	valarray<int> y(5);
	y = 0;
	valarray<int> x = { 1 };
	x = y;
	EXPECT_EQ(x.size(), 1);
	for (int i = 0; i < 1; ++i) {
		EXPECT_EQ(x[i], 0);
	}
	x = 5;
	y = x;
	EXPECT_EQ(y.size(), 5);
	EXPECT_EQ(y[0], 5);
	for (int i = 1; i < 5; ++i) {
		EXPECT_EQ(y[i], 0);
	}
}
#endif
struct Foo {};
#if defined(PHASE_B1_8) | defined(PHASE_B)
TEST(PhaseB1, FooTest) {
	valarray<int> y(5);
	Foo x;
	4 - 5;
	4.0 - 5;
	4 - 5.0f;
	4.0 - complex<double>{4, 3};
	valarray<complex<double>> xx(10);
	valarray<complex<float>> xxx(10);
	xx + xxx;
	// should not compile
	//valarray<Foo> bob;
	//xx + bob;
	//x + y;
	//complex<float>{3, 5} +complex<double>{5, 2};
	//y + x;
}
#endif
