fun isqrtld (a : string) : (string * string) =
    let
        fun longDivision ([], divisor, dividend, quotient) = (Int.toString quotient, Int.toString dividend)
        |   longDivision (x :: y :: xs, divisor, dividend, quotient) =
                let
                    val dividend = dividend * 100 + (Char.ord x - 48)*10 + (Char.ord y - 48) (*appending next pair of digts to dividend *)
                    val (remainder, quotient_units_digit) = 
                        foldl (fn (i, (remainder, quotient_units_digit)) =>
                            if (dividend - ((divisor * 10 + i) * i) >= 0) then (*checking if remainder is positive/zero *)
                                (dividend - ((divisor * 10 + i) * i), i)
                            else
                                (remainder, quotient_units_digit)
                        ) (0, 0) (List.tabulate (10, fn i => i)) (* iterating i from 0 to 9*)
                    val quotient = quotient * 10 + quotient_units_digit (*appending the digit i *)
                    val divisor = quotient * 2
                    val dividend = remainder
                in
                    longDivision (xs, divisor, remainder, quotient) (* recursive call*)
                end
        |   longDivision (x :: xs, divisor, dividend, quotient) = ("0", "0")
    in

        longDivision (explode (if size a mod 2 = 1 then "0"^a else a), 0, 0, 0) (* check if the input string has odd length, if yes add 0 to the left*)
    end
