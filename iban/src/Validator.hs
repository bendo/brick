module Validator (validate) where

import Debug.Trace

validate :: [String] -> Bool
validate strings = trace ("Input: " ++ show strings) True

