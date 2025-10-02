#include <bits/stdc++.h>
using namespace std;

struct BigInteger {
    static const int BASE = 1000000000;
    static const int WIDTH = 9;
    vector<int> d;
    bool neg = false;

    BigInteger() = default;
    BigInteger(long long v) { *this = v; }
    BigInteger& operator=(long long v) {
        d.clear(); neg = false;
        if (v < 0) { neg = true; v = -v; }
        while (v) { d.push_back(int(v % BASE)); v /= BASE; }
        return *this;
    }
    BigInteger(const string& s) { fromString(s); }

    void trim() {
        while (!d.empty() && d.back() == 0) d.pop_back();
        if (d.empty()) neg = false;
    }

    void fromString(const string& s) {
        d.clear(); neg = false;
        int pos = 0, n = s.size();
        if (s[0] == '-') { neg = true; pos = 1; }
        for (int i = n; i > pos; i -= WIDTH) {
            int l = max(pos, i - WIDTH);
            int len = i - l;
            d.push_back(stoi(s.substr(l, len)));
        }
        trim();
    }

    string toString() const {
        if (d.empty()) return "0";
        stringstream ss;
        if (neg) ss << '-';
        ss << d.back();
        for (int i = (int)d.size() - 2; i >= 0; --i) ss << setw(WIDTH) << setfill('0') << d[i];
        return ss.str();
    }

    friend ostream& operator<<(ostream& os, const BigInteger& x) { os << x.toString(); return os; }

    int cmpAbs(const BigInteger& b) const {
        if (d.size() != b.d.size()) return d.size() < b.d.size() ? -1 : 1;
        for (int i = (int)d.size() - 1; i >= 0; --i)
            if (d[i] != b.d[i]) return d[i] < b.d[i] ? -1 : 1;
        return 0;
    }

    int cmp(const BigInteger& b) const {
        if (neg != b.neg) return neg ? -1 : 1;
        int a = cmpAbs(b);
        return neg ? -a : a;
    }

    bool operator<(const BigInteger& b) const { return cmp(b) < 0; }
    bool operator==(const BigInteger& b) const { return neg == b.neg && d == b.d; }
    bool operator!=(const BigInteger& b) const { return !(*this == b); }
    bool operator>(const BigInteger& b) const { return cmp(b) > 0; }
    bool operator<=(const BigInteger& b) const { return cmp(b) <= 0; }
    bool operator>=(const BigInteger& b) const { return cmp(b) >= 0; }

    BigInteger operator-() const {
        BigInteger r = *this;
        if (!r.d.empty()) r.neg = !r.neg;
        return r;
    }

    BigInteger& addAbs(const BigInteger& b) {
        long long carry = 0;
        size_t n = max(d.size(), b.d.size());
        d.resize(n, 0);
        for (size_t i = 0; i < n; ++i) {
            long long sum = carry + d[i] + (i < b.d.size() ? b.d[i] : 0);
            d[i] = int(sum % BASE);
            carry = sum / BASE;
        }
        if (carry) d.push_back(int(carry));
        return *this;
    }

    BigInteger& subAbs(const BigInteger& b) {
        long long carry = 0;
        for (size_t i = 0; i < d.size(); ++i) {
            long long cur = d[i] - (i < b.d.size() ? b.d[i] : 0) - carry;
            if (cur < 0) { cur += BASE; carry = 1; } else carry = 0;
            d[i] = int(cur);
        }
        trim();
        return *this;
    }

    BigInteger operator+(const BigInteger& b) const {
        if (neg == b.neg) {
            BigInteger r = *this;
            r.addAbs(b);
            return r;
        }
        if (cmpAbs(b) >= 0) {
            BigInteger r = *this;
            r.subAbs(b);
            return r;
        } else {
            BigInteger r = b;
            r.subAbs(*this);
            return r;
        }
    }

    BigInteger operator-(const BigInteger& b) const {
        if (neg != b.neg) {
            BigInteger r = *this;
            r.addAbs(b);
            return r;
        }
        if (cmpAbs(b) >= 0) {
            BigInteger r = *this;
            r.subAbs(b);
            return r;
        } else {
            BigInteger r = b;
            r.subAbs(*this);
            r.neg = !r.neg;
            return r;
        }
    }

    BigInteger operator*(const BigInteger& b) const {
        if (d.empty() || b.d.empty()) return BigInteger(0);
        BigInteger r;
        r.neg = neg ^ b.neg;
        r.d.assign(d.size() + b.d.size(), 0);
        for (size_t i = 0; i < d.size(); ++i) {
            long long carry = 0;
            for (size_t j = 0; j < b.d.size() || carry; ++j) {
                long long cur = r.d[i + j] + carry + 1ll * d[i] * (j < b.d.size() ? b.d[j] : 0);
                r.d[i + j] = int(cur % BASE);
                carry = cur / BASE;
            }
        }
        r.trim();
        return r;
    }

    BigInteger& mulInt(long long m) {
        if (m == 0 || d.empty()) { d.clear(); neg = false; return *this; }
        if (m < 0) { neg = !neg; m = -m; }
        long long carry = 0;
        for (size_t i = 0; i < d.size() || carry; ++i) {
            if (i == d.size()) d.push_back(0);
            long long cur = carry + 1ll * d[i] * m;
            d[i] = int(cur % BASE);
            carry = cur / BASE;
        }
        trim();
        return *this;
    }

    BigInteger operator*(long long m) const {
        BigInteger r = *this;
        r.mulInt(m);
        return r;
    }

    pair<BigInteger, int> divModInt(int m) const {
        BigInteger q;
        q.neg = neg ^ (m < 0);
        long long mm = abs(1ll * m);
        q.d.assign(d.size(), 0);
        long long rem = 0;
        for (int i = (int)d.size() - 1; i >= 0; --i) {
            long long cur = d[i] + rem * BASE;
            q.d[i] = int(cur / mm);
            rem = cur % mm;
        }
        q.trim();
        return {q, int(rem)};
    }

    BigInteger divInt(int m) const { return divModInt(m).first; }
    int modInt(int m) const { return divModInt(m).second; }

    static BigInteger divmod(const BigInteger& a1, const BigInteger& b1, BigInteger& rem) {
        BigInteger a = a1; BigInteger b = b1;
        if (b.d.empty()) throw runtime_error("division by zero");
        if (a.d.empty()) { rem = BigInteger(0); return BigInteger(0); }
        int norm = BASE / (b.d.back() + 1);
        a.mulInt(norm);
        BigInteger v = b;
        v.mulInt(norm);
        BigInteger q;
        q.d.assign(a.d.size(), 0);
        rem = BigInteger(0);
        for (int i = (int)a.d.size() - 1; i >= 0; --i) {
            rem.d.insert(rem.d.begin(), a.d[i]);
            if (!rem.d.empty() && rem.d.back() == 0) rem.trim();
            int s1 = rem.d.size() <= v.d.size() ? 0 : rem.d[v.d.size()];
            long long s2 = rem.d.size() <= v.d.size() - 1 ? 0 : rem.d[v.d.size() - 1];
            long long d_guess = ((long long)s1 * BASE + s2) / v.d.back();
            if (d_guess >= BASE) d_guess = BASE - 1;
            BigInteger t = v * (long long)d_guess;
            while (rem < t) { --d_guess; t = v * (long long)d_guess; }
            rem = rem - t;
            q.d[i] = int(d_guess);
        }
        q.neg = a1.neg ^ b1.neg;
        q.trim();
        rem.neg = a1.neg;
        rem.divInt(norm);
        rem.trim();
        return q;
    }

    BigInteger operator/(const BigInteger& b) const {
        BigInteger r;
        return divmod(*this, b, r);
    }

    BigInteger operator%(const BigInteger& b) const {
        BigInteger r;
        divmod(*this, b, r);
        return r;
    }
};
