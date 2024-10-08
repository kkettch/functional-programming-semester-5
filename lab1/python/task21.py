def d(n):
    summa = 1 
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            summa += i
            if i != n // i:  
                summa += n // i
    return summa

def get_amicable_numbers(limit):
    amicable_numbers = []
    for a in range(2, limit):
        b = d(a)
        if b != a and d(b) == a:
            amicable_numbers.append(a)
    return amicable_numbers

limit = 10000
amicable_numbers = get_amicable_numbers(limit)
print("Result for limit 10000: ", sum(amicable_numbers))
