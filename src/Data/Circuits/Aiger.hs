{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
module Data.Circuits.Aiger 
    where
import Text.Parsec
import Text.Parsec.Char
import Data.String
import Data.Char
import Data.Bits (shiftL, (.&.))
import Control.Monad (forM, liftM2, sequence)
import Data.Circuits.Aig
import Data.List(foldl', find)

number :: Stream s m Char => ParsecT s u m String
number = many1 digit

header :: Stream s m Char => ParsecT s u m [Integer]
header = do
    string "aig"
    spaces
    m <- number
    spaces
    i <- number
    spaces
    l <- number
    spaces
    o <- number
    spaces
    a <- number
    _ <- newline
    return $  map read [m, i, l, o, a]

outputs :: Stream s m Char => Integer -> ParsecT s u m [Integer]
outputs o = forM (replicate (fromInteger o) o) $ \_ -> do
    n <- number
    newline
    return (read n)

beint :: Stream s m Char => ParsecT s u m Integer
beint = beint' 0 0
    where
        beint' b i = do
            x <- anyChar
            let xx =  toInteger $ ord x
            if xx < 0x80 then return (shiftL xx i + b)
            else beint' (shiftL (xx .&.  0x7F) i + b) (i+ 7)  

gateTriples :: Stream s m Char => Integer -> Integer -> ParsecT s u m [(Integer, Integer)]
gateTriples m i = count (fromInteger (m-i)) triple
    where
        triple = do
            a <- beint
            b <- beint
            return (a, b)

symbols :: Stream s m Char => Char -> ParsecT s u m [(Integer, String)]
symbols chr = many $ symbol
    where 
        symbol = do
            char chr
            pos <- number
            spaces
            name <- many1 alphaNum
            _ <- newline
            return (read pos, name)

makeVarNodes :: Integer -> [(Integer, String)] -> [AigEdge]
makeVarNodes i gateNames = nlist i
    where
        nlist 0 = []
        nlist n = var (search  n) : nlist (n-1)
        search ind = case find (\ (x,_) -> x==ind) gateNames of
          Just tupl -> snd tupl
          Nothing -> show ind
        
gateAt :: Integer  -> [AigEdge] -> AigEdge
gateAt i prevGates = maybeNot ((i `mod` 2) == 1) $ prevGates !! negIndex
    where
      negIndex = (length prevGates - (fromInteger i `div` 2))
      maybeNot True a = Data.Circuits.Aig.not a
      maybeNot False a = a
      
makeGate :: (Integer, Integer) -> [AigEdge] -> AigEdge
makeGate (left, right) prevGates = Data.Circuits.Aig.and (gateAt' left') (gateAt' right')
  where
    gateAt' i = gateAt i prevGates
    left' = fromIntegral (length prevGates + 1) * 2 - left
    right' = left' - right


aigFile :: Stream s m Char => ParsecT s u m [AigEdge]
aigFile = do
    [m,i,l,o,a] <- header
    outs <- outputs o
    ands <- gateTriples m i
    inputNames <- symbols 'i'
    outputNames <- symbols 'o'
    let base = makeVarNodes i inputNames
    let allgates = foldl' (\ gateList gateInts-> (makeGate gateInts gateList):gateList) base ands
    -- return $ [m,i,l,o,a]
    -- return ands
    return $ [ gateAt o allgates | o <- outs]

