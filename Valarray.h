#include "Vector.h"
#include <iostream>
#include <complex>
#include <math.h>
using std::complex;
using epl::vector;

// forward declarations
template<typename T> class vec_wrap;
template<typename L, typename R, typename Fn> struct Proxy;

// alias for valarray
template<typename T> using valarray = vec_wrap<vector<T>>;

// metastruct: is our value complex? default to false, true in the specialization
// base cases
template<typename T> struct IsComplex { static const bool value = false; };
template <typename T> struct IsComplex<complex<T>> { static const bool value = true; };

// need to ask these containers about the types they contain
template<typename T> struct IsComplex<T&> { static const bool value = IsComplex<T>::value; };
template<typename T> struct IsComplex<const T> { static const bool value = IsComplex<T>::value; };
template <typename T> struct IsComplex<vector<T>> { static const bool value = IsComplex<T>::value; };
template <typename T> struct IsComplex<vec_wrap<T>> { static const bool value = IsComplex<T>::value; };
template <typename L, typename R, typename op>
struct IsComplex<Proxy<L, R, op>> { static const bool value = IsComplex<typename Proxy<L, R, op>::value_type>::value; };

// the ranks of different types of variables
template <typename T> struct Rank;
template <> struct Rank<int> { static constexpr int rank = 0; };
template <> struct Rank<float> { static constexpr int rank = 1; };
template <> struct Rank<double> { static constexpr int rank = 2; };

// for containers, propagate the call down
template <typename T> struct Rank<T&> { static constexpr int rank = Rank<T>::rank; };
template <typename T> struct Rank<complex<T>> { static constexpr int rank = Rank<T>::rank; };
template <typename T> struct Rank<vector<T>> { static constexpr int rank = Rank<T>::rank; };
template <typename T> struct Rank<vec_wrap<T>> { static constexpr int rank = Rank<T>::rank; };
template <typename L, typename R, typename op> struct Rank<Proxy<L, R, op>> { static constexpr int rank = Rank<typename Proxy<L, R, op>::value_type>::rank; };

// map from int, bool combo to a type
template<int T, bool B> struct stype;
template<> struct stype<0, false> { using type = int; };
template<> struct stype<1, false> { using type = float; };
template<> struct stype<2, false> { using type = double; };
template<int T> struct stype<T, true> { using type = complex<typename stype<T, false>::type>; };

// metastruct: choose the best type for two types
template<typename T1, typename T2>
struct choose_type {
	static constexpr int t1_rank = Rank<T1>::rank;
	static constexpr int t2_rank = Rank<T2>::rank;
	static constexpr int rank = t1_rank < t2_rank ? t2_rank : t1_rank;
	static constexpr bool is_complex = IsComplex<T1>::value || IsComplex<T2>::value;
	using type = typename stype<rank, is_complex>::type;
};
// alias for choose_type::type
template <typename L, typename R> using ChooseType = typename choose_type<L, R>::type;

// should we use a reference? Answer for vector is yes; everyone else, no.
template <typename T> struct is_ref { using type = T; };
template<typename T> struct is_ref<vector<T>> { using type = const vector<T>&; };
template<typename T> struct is_ref<const T> { using type = is_ref<T>; };

// alias for is_ref::type
template<typename T> using IsRef = typename is_ref<T>::type;

// what's the value type of this object?
template<typename T> struct value_type { using type = T; };
template<typename T> struct value_type<const T> { using type = typename value_type<T>::type; };
template<typename T> struct value_type<vector<T>> { using type = T; };
template<typename L, typename R, typename Fn> struct value_type<Proxy<L, R, Fn>> { using type = typename Proxy<L, R, Fn>::value_type; };

// alias for value_type::type
template<typename T> using ValueType = typename value_type<T>::type;

// a structure to add vector functionality to anything that doesn't already have it... need op[] and size()
template<typename T>
struct vector_concept {
	IsRef<T> t;
	ValueType<T> operator[](uint64_t k) {
		// delegate
		return _bracket(k, t);
	}
	template<typename U>
	auto _bracket(uint64_t k, const U& _t) -> decltype(_t[k]) {
		// the type already has a bracket... use it.
		return _t[k];
	}
	T _bracket(uint64_t k, ...) {
		// the type has no bracket. Must be a constant.
		return t;
	}
	uint64_t size() {
		// delegate
		return _size(t);
	}
	template<typename U>
	auto _size(const U& _t) -> decltype(_t.size()) {
		// the type already has a size()... use it.
		return _t.size();
	}
	uint64_t _size(...) {
		// the type has no size. Must be one of our scalars. BIG NUMBERZ!
		return 100000000;
	}
};

// adapter - takes a unary operator and creates a binary operator() for it so we only need one proxy struct
template<typename L, typename R, typename Fn>
struct OneToTwoAdapter {
	Fn f;
	using result_type = typename Fn::result_type;
	result_type operator() (const ChooseType<L, R>& val, const ChooseType<L, R>&) const {
		return f(val);
	}
};

//Proxy class - will hold (maybe) references to operands and know how to apply the appropriate function to them.
template<typename L, typename R, typename Fn>
struct Proxy {
	using value_type = typename Fn::result_type;
	IsRef<L> l;
	IsRef<R> r;
	Fn f;
	// constructors
	Proxy() : l(), r(), f() {}
	Proxy(const L& _l, const R& _r, const Fn& _f) : l(_l), r(_r), f(_f) {}
	Proxy(const Proxy& rhs) : l(rhs.l), r(rhs.r), f(rhs.f) {}
	Proxy(const Proxy&& rhs) : l(std::move(rhs.l)), r(std::move(rhs.r)), f(std::move(rhs.f)) {}
	// establish the vector concept
	value_type operator[](uint64_t k) const {
		return f((value_type)(vector_concept<L>{l}[k]), (value_type)(vector_concept<R>{r}[k]));
	}
	uint64_t size() const {
		return vector_concept<L>{l}.size() < vector_concept<R>{r}.size() ? vector_concept<L>{l}.size() : vector_concept<R>{ r }.size();
	}
	// define an iterator
	struct const_iterator {
		// keep a copy of the proxy, in case it goes out of scope.
		const Proxy<L, R, Fn> p;
		uint64_t k;

		value_type operator*() const {
			return p[k];
		}
		const_iterator& operator++() {
			++k;
			return *this;
		}
		const_iterator operator++(int) {
			// post-increment. 
			const_iterator tmp{ p, k };
			++k;
			return tmp;
		}
		bool operator==(const const_iterator& rhs) const {
			return k == rhs.k;
		}
		bool operator!=(const const_iterator& rhs) const {
			return !(*this == rhs);
		}

	};
	const_iterator begin() const {
		return const_iterator{ *this, 0 };
	}
	const_iterator end() const {
		return const_iterator{ *this, this->size() };
	}
};

// aliases for function objects
template<typename L, typename R> using add = std::plus<ChooseType<L, R>>;
template<typename L, typename R> using mult = std::multiplies<ChooseType<L, R>>;
template<typename L, typename R> using divide = std::divides<ChooseType<L, R>>;
template<typename L, typename R> using sub = std::minus<ChooseType<L, R>>;
template<typename L> using neg = std::negate<ChooseType<L, L>>;

// a squareroot function object, promoted appropriately.
template<typename L>
struct squrt {
	using result_type = ChooseType<L, double>;
	result_type operator()(const ChooseType<L, double>& orig) const {
		return sqrt(orig);
	}
};

// unwrap -- keep scalars the same, remove any vec_wraps
template <typename T> struct unwrap;
template<> struct unwrap<int> { using type = int; };
template<> struct unwrap<float> { using type = float; };
template<> struct unwrap<double> { using type = double; };
template<typename T> struct unwrap<complex<T>> { using type = complex<T>; };
template<typename T> struct unwrap<vec_wrap<T>> { using type = T; };

// alias for unwrap
template<typename T> using Unwrap = typename unwrap<T>::type;

// metafunction to find out if this is a scalar
template<typename T> struct is_scalar : public std::false_type {};
template<> struct is_scalar<int> : public std::true_type{};
template<> struct is_scalar<float> : public std::true_type{};
template<> struct is_scalar<double> : public std::true_type{};
template<typename T> struct is_scalar<complex<T>> : public std::true_type{};

// metafunction to find out if this is a vec_wrap
template<typename T> struct is_wrap : public std::false_type {};
template<typename T> struct is_wrap<vec_wrap<T>> : public std::true_type{};
template<typename T> struct is_wrap< const T> : public is_wrap<T>{};

// metafunction to determine if two types should match our operators
template<typename L, typename R> struct is_valid {
	static constexpr bool value = (is_wrap<L>::value && is_wrap<R>::value) ||
		(is_wrap<L>::value && is_scalar<R>::value) ||
		(is_scalar<L>::value && is_wrap<R>::value);
};

// enable_if structure
template<bool b, typename use> struct enable_if {};
template<typename use> struct enable_if<true, use> { using type = use; };
template<bool b, typename use>using EnableIf = typename enable_if<b, use>::type;

// wrapper class--where the magic happens.
template<typename T>
class vec_wrap : public T {
public:
	// define our value type
	using value_type = typename T::value_type;

	// inherit constructors
	using T::T;

	// define extra constructors
	vec_wrap() : T() {}

	// non-templated copy constructor for like types
	vec_wrap(const vec_wrap<T>& rhs) {
		if (&rhs != this) {
			uint64_t s = this->size() < rhs.size() ? this->size() : rhs.size();
			for (uint64_t i = 0; i < s; ++i) {
				this->operator[](i) = (value_type)(rhs[i]);
			}
		}
	}
	// non-templated move constructor to fix a proxy/auto issue
	vec_wrap(const vec_wrap<T>&& rhs) : T(std::move(rhs)) {}

	// templated copy constructor coming from proxies
	template<typename L, typename R, typename Fn>
	vec_wrap(const Proxy<L, R, Fn>& rhs) : T(rhs.size()) {
		uint64_t s = this->size() < rhs.size() ? this->size() : rhs.size();
		for (uint64_t i = 0; i < s; ++i) {
			this->operator[](i) = (value_type)(rhs[i]);
		}
	}
	// assignment operator - constant
	vec_wrap<T>& operator=(const value_type& t) {
		for (int i = 0; i < this->size(); ++i) {
			(*this)[i] = t;
		}
		return *this;
	}
	// non-templated operator= for like wrapped values.
	vec_wrap<T>& operator=(const vec_wrap<T>& rhs) {
		if (&rhs != this) {
			uint64_t s = this->size() < rhs.size() ? this->size() : rhs.size();
			for (uint64_t i = 0; i < s; ++i) {
				this->operator[](i) = (value_type)(rhs[i]);
			}
		}
		return *this;
	}
	// lazy evaluation: apply
	template<typename Fn>
	vec_wrap<Proxy<T, T, OneToTwoAdapter<T, T, Fn>>> apply(Fn f) const {
		return vec_wrap<Proxy<T, T, OneToTwoAdapter<T, T, Fn>>> {
			*this, *this, OneToTwoAdapter<T, T, Fn>{f}};
	}
	// sqrt -- specific application of apply
	vec_wrap<Proxy<T, T, OneToTwoAdapter<T, T, squrt<T>>>> sqrt() const {
		return apply(squrt<T>{});
	}
	// unary- -- application of apply
	vec_wrap<Proxy<T, T, OneToTwoAdapter<T, T, neg<T>>>> operator-() const {
		return apply(neg<T>{});
	}
	// greedy evaluation
	template <typename Fn>
	typename Fn::result_type accumulate(Fn f) {
		if (this->size() == 0) {
			return 0;
		}
		typename Fn::result_type result = (*this)[0];
		for (uint64_t i = 1; i < this->size(); ++i) {
			result = f(result, (*this)[i]);
		}
		return result;
	}
	// sum -- specific application of accumulate
	typename std::plus<value_type>::result_type sum() {
		return accumulate(std::plus<value_type>());
	}
};
// binary operators w/ enable_if--will cause ambiguity with template<typename L, typename R>
// operator+
template<typename L, typename R>
EnableIf<is_valid<L, R>::value, vec_wrap<Proxy<Unwrap<L>, Unwrap<R>, add<L, R>>>> operator+(const L& l, const R& r) {
	return vec_wrap<Proxy<Unwrap<L>, Unwrap<R>, add<L, R>>>(l, r, add<L, R>{});
}
// operator*
template<typename L, typename R>
EnableIf<is_valid<L, R>::value, vec_wrap<Proxy<Unwrap<L>, Unwrap<R>, mult<L, R>>>> operator*(const L& l, const R& r) {
	return vec_wrap<Proxy<Unwrap<L>, Unwrap<R>, mult<L, R>>>(l, r, mult<L, R>{});
}
//operator-
template<typename L, typename R>
EnableIf<is_valid<L, R>::value, vec_wrap<Proxy<Unwrap<L>, Unwrap<R>, sub<L, R>>>> operator-(const L& l, const R& r) {
	return vec_wrap<Proxy<Unwrap<L>, Unwrap<R>, sub<L, R>>>(l, r, sub<L, R>{});
}
//operator/
template<typename L, typename R>
EnableIf<is_valid<L, R>::value, vec_wrap<Proxy<Unwrap<L>, Unwrap<R>, divide<L, R>>>> operator/(const L& l, const R& r) {
	return vec_wrap<Proxy<Unwrap<L>, Unwrap<R>, divide<L, R>>>(l, r, divide<L, R>{});
}
// operator <<
template<typename T>
std::ostream& operator << (std::ostream& stream, const vec_wrap<T>& arr) {
	stream << "[ ";
	for (typename T::value_type val : arr) {
		stream << val << " ";
	}
	stream << " ]";
	return stream;
}
