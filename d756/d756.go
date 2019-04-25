package main

import(
	"fmt"
)

func main() {
	var n int
	fmt.Scan(&n)
	fmt.Println(Execute(n))
}

func Execute(n int) int {
	primes := makePrimes(n)
	factors := make([]int, len(primes))

	// 1からnまで、各々を素因数分解
	for i := 1; i <= n; i++ {
		factorizeIntoPrimes(i, primes, &factors)
	}

	return solve(factors)
}

func solve(factors []int) int {
	// 約数を作る時、その素数を取らない(0乗)という選択肢も考慮する
	// 0乗分を各素数の指数に追加する
	for i := range factors {
		factors[i] += 1
	}

	// 以下、愚直に計算
	var factor3_5, factor3_25, factor5_15 int
	var factor5, factor15, factor25, factor75 int
	for _, e := range factors {
		if 3 <= e && e < 5 { factor3_5 += 1 }
		if 3 <= e && e < 25 { factor3_25 += 1 }
		if 5 <= e && e < 15 { factor5_15 += 1 }
		if 5 <= e { factor5 += 1 }
		if 15 <= e { factor15 += 1 }
		if 25 <= e { factor25 += 1 }
		if 75 <= e { factor75 += 1 }
	}

	// 素数1種類で七五数となる約数の個数 (75)
	count := factor75
	fmt.Println("count75:", count)

	// 素数2種類で七五数となる約数の個数 (3*25)
	// - 指数3以上25未満の素数から1個、指数25以上の素数から1個で作る場合
	count += factor3_25 * factor25
	fmt.Println("count3-25:", count)
	// - 指数25以上の素数から2個で作る場合
	count += permutation(factor25, 2)
	fmt.Println("factor25", factor25)
	fmt.Println("count3-25:", count)

	// 素数2種類で七五数となる約数の個数 (5*15)
	// - 指数5以上15未満の素数から1個、指数15以上の素数から1個で作る場合
	count += factor5_15 * factor15
	// - 指数15以上の素数から2個で作る場合(5-15, 15-5)
	count += combination(factor15, 2) * 2

	// 素数3種類で七五数となる約数の個数 (3*5*5)
	// - 指数3以上5未満の素数から1個、指数5以上の素数から2個で作る場合
	count += factor3_5 * combination(factor5, 2)
	// - 指数5以上の素数から3個で作る場合(3-5-5, 5-3-5, 5-5-3)
	count += combination(factor5, 3) * 3

	return count
}

// 組み合わせ
func combination(n, r int) int {
	if n < r {
		return 0
	}
	return permutation(n, r) / factorial(r)
}

// 順列
func permutation(n, r int) int {
	if n < r {
		return 0
	}
	return factorial(n) / factorial(n - r)
}

// 階乗
func factorial(n int) int {
	fac := 1
	for i := 1; i <= n; i++ {
		fac *= i
	}
	return fac
}

// 素因数分解する(素数指数表現に変換する)
// num int: 素因数分解対象の数
// primes []int: 素因数分解に使用する素因数一覧(num<=max(primes)となること)
// powers *[]int: numを素数指数表現にした際の各指数の数を各要素に足す
func factorizeIntoPrimes(num int, primes []int, powers *[]int) {
	for i, p := range primes {
		n := num
		for n != 0 && n % p == 0 {
			(*powers)[i] += 1
			n /= p
		}
	}
}

// 2からnまでの素数のスライスを返す
// エラトステネスの篩
// n int: 2からnまで、のn
// return []int: 2からnまでの素数のスライス
func makePrimes(n int) []int {// {{{
	var numbers []int
	for i := 0; i < n; i++ {
		numbers = append(numbers, i + 1)
	}
	numbers[0] = 0

	// 2からnまでの素数
	var primes []int
	p := 1
	for {
		// 0消去されていない最初の数は素数
		for i := p + 1; i <= n; i++ {
			if numbers[i - 1] != 0 {
				p = numbers[i - 1]
				break
			}
		}
		primes = append(primes, p)

		// 素数の倍数は合成数として0消去
		for i := p * 2; i <= n; i += p {
			numbers[i - 1] = 0
		}

		// root(n)まで探索した時点で0消去されていない数は素数
		if p * p > n {
			for i := p + 1; i <= n; i++ {
				if numbers[i - 1] != 0 {
					primes = append(primes, i)
				}
			}
			break
		}
	}
	return primes
}// }}}

// スライス表示用ユーティリティ関数
// 素数指数表現などに使用する
func printSlice(slice []int) {
	fmt.Printf("[")
	for i, e := range slice {
		fmt.Printf("%2d", e)
		if i == len(slice) - 1 {
			break
		}
		fmt.Printf(" ")
	}
	fmt.Printf("]\n")
}
