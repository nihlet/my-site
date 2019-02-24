
module Butterworth
  (butterworthIIR) where

import Algebra.ZeroTestable
import MathObj.Polynomial
import Number.Ratio
import Prelude hiding (const)
import qualified Algebra.Field as AF
import qualified Algebra.ZeroTestable as AZ

-- перевод из дБ в разы
fromdB att = 10**(0.1 * att)

-- порядок фильтра
order (wp, ws) (attPass, attStop) = ceiling $ logBase (ws / wp) (epsilon attStop / epsilon attPass) 

epsilon att = sqrt (fromdB att - 1)

analogTransferFunction :: (AF.C a, AZ.C a, RealFrac a, Floating a) =>
  (a, a) -> (a, a) -> Number.Ratio.T (MathObj.Polynomial.T a)
analogTransferFunction (wp, ws) (attPass, attStop) = const 1 % polynome where
  polynome = const (epsilon attPass) * p1^r * foldl (*) (const 1) (map p2 [1..l])
  p1 = fromCoeffs [alpha, 1]
  p2 n' = fromCoeffs [alpha^^2, 2 * alpha * sin(thetta n'), 1]
  thetta n' = (2 * fromIntegral n' - 1) / (2 * fromIntegral n) * pi
  alpha = epsilon attPass ** (-1 / fromIntegral n)
  (l,r) = divMod n 2
  n = order (wp, ws) (attPass, attStop)

  
bilinear:: (Num a) => a -> Number.Ratio.T (MathObj.Polynomial.T a) 
bilinear fd = (const (2 * fd) * fromCoeffs [1, -1]) :% fromCoeffs [1, 1]
  
digitalTransferFunction :: (AF.C a, AZ.C a, RealFrac a, Floating a) =>
     a -> (a, a) -> (a, a) -> Number.Ratio.T (MathObj.Polynomial.T a)
digitalTransferFunction fd (wp, ws) (attPass, attStop) = substitution sub anTransfer' where
  sub = bilinear fd
  anTransfer = analogTransferFunction (wpWrapped, wsWrapped) (attPass, attStop)
  anTransfer' = substitution (s_omega :% const 1) anTransfer
  s_omega = fromCoeffs [0, 1/(2 * fd * tan(wp/2))]
  wpWrapped = 2 * fd * tan(wp/2) 
  wsWrapped = 2 * fd * tan(ws/2) 

    
butterworthIIR :: (AF.C a, AZ.C a, RealFrac a, Floating a) =>
    a -> (a, a) -> (a, a) -> ([a], [a])
butterworthIIR fd (fp, fs) (attPass, attStop)  = (a,b) where
  b = coeffs $ numerator tf
  a = coeffs $ denominator tf  
  tf = digitalTransferFunction fd (wp, ws) (attPass, attStop)
  wp = fp/(fd/2) * pi
  ws = fs/(fd/2) * pi
    

-- подстановка дробь из полиномов в дробь из полиномов
substitution :: (AF.C a, AZ.C a, Num a, Fractional a) => Number.Ratio.T (MathObj.Polynomial.T a) -> Number.Ratio.T (MathObj.Polynomial.T a) -> Number.Ratio.T (MathObj.Polynomial.T a)
substitution (snum :% sden) (num :% den) = num'' :% den'' where
  maxExp = max (length (coeffs num) - 1) (length (coeffs den) - 1)
  num' = step3 $ step2 sden maxExp $ step1 snum num
  den' = step3 $ step2 sden maxExp $ step1 snum den
  num'' = const k * num'
  den'' = const k * den'
  k = 1 / head (coeffs den')

step1 :: (Num a) =>  MathObj.Polynomial.T a -> MathObj.Polynomial.T a -> [MathObj.Polynomial.T a]
step1 subst poly = step1' 0 c where
  c = coeffs poly 
  step1' _ []     = []
  step1' n (x:xs) = (subst^n * const x) : step1' (n+1) xs

step2 :: (Num a, Integral b) => MathObj.Polynomial.T a -> b -> [MathObj.Polynomial.T a] -> [MathObj.Polynomial.T a]
step2 _ _ []     = []
step2 den n (x:xs) = x * den^n : step2 den (n-1) xs

step3:: (Num a) => [MathObj.Polynomial.T a] -> MathObj.Polynomial.T a
step3 = foldl (+) (const 0) 

