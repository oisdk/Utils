module Types where
  
data Unit = Inches | Feet | Yards | Metres | Cm deriving Eq

--convert :: Unit -> Unit -> Double -> Double
--convert a b | a == b = id
--convert Metres u = (1.09*) . convert Yards  u  
--convert Yards  u = (3   *) . convert Feet   u
--convert Feet   u = (12  *) . convert Inches u
--convert Inches u = (2.54*) . convert Cm     u 
--convert Cm     u = (/100 ) . convert Metres u

data Currency = GBP | EUR | USD deriving Eq

convert :: Currency -> Currency -> Double -> Double
convert a b | a == b = id
convert GBP u = (1.27*) . convert EUR u
convert EUR u = (1.13*) . convert USD u
convert USD u = (0.69*) . convert GBP u