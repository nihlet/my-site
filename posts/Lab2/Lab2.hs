module Lab2
  ( plotSignal
  , plotDiscrets1
  , plotDiscrets2
  , plotDiscrets3
  , plotAmplitudeSpectre1
  , plotAmplitudeSpectre2
  , plotAmplitudeSpectre3
  , plotPhaseSpectre1
  , plotPhaseSpectre2
  , plotPhaseSpectre3
  , plotRestoredSignal1
  , plotRestoredSignal2
  , plotRestoredSignal3
  ) where

import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy hiding (indices)

import Data.Array
import Data.Complex
import Numeric.Transform.Fourier.FFT

-- Константы согласно заданию
f0 = 1400

k = [0.2, 1.6, 8.0]

m = [2, 5, 12]

n = 1024.0

periods = 10

period :: Double
period = 1 / f0

-- signal t
--   | t > periods * period = 0
signal :: Double -> Double
signal t = (-1) ^^ i
  where
    i = floor (t * f0 * 2)

plotSignal =
  toFile def "signal.svg" $ do
    layout_title .= "Signal"
    setColors [opaque blue]
    let discrets = [0.0,1 / (100 * f0) .. (periods * 2) / f0]
    plot (line "" [zip discrets (map signal discrets)])

plotDiscrets filename k =
  toFile def filename $ do
    layout_title .= ("fd = " ++ show k ++ " * f0")
    setColors [opaque blue, opaque red]
    let discrets = [0.0,1 / (k * f0) .. (periods * 2) / f0]
    plot (line "" [zip discrets (map signal discrets)])
    plot (points "" $zip discrets (map signal discrets))

plotDiscrets1 = plotDiscrets "discretSignal1.svg" (head k)

plotDiscrets2 = plotDiscrets "discretSignal2.svg" (k !! 1)

plotDiscrets3 = plotDiscrets "discretSignal3.svg" (k !! 2)

spectrum k = rfft s
  where
    s =
      listArray (0, floor n - 1) $
      map signal [0.0,1 / (k * f0) .. (n - 1) / (k * f0)]

plotAmplitudeSpectre filename k =
  toFile def filename $ do
    layout_title .= ("spectrum fd = " ++ show k ++ " * f0 ")
    setColors [opaque blue]
    let spect = map magnitude $elems $spectrum k :: [Double]
    let indx = indices $spectrum k :: [Int]
    plot (line "" [zip indx spect])

plotPhaseSpectre filename k =
  toFile def filename $ do
    layout_title .= ("phase spectrum fd = " ++ show k ++ " * f0 ")
    setColors [opaque blue]
    let spect = map phase $elems $spectrum k :: [Double]
    let indx = indices $spectrum k :: [Int]
    plot (line "" [zip indx spect])

restoredSignal k n = irfft modifiedSpectrum
  where
    modifiedSpectrum =
      spectrum k // [(i, 0 :+ 0) | i <- [n + 1 .. end - begin - n]]
    (begin, end) = bounds $ spectrum k

plotRestoredSignal filename k n =
  toFile def filename $ do
    layout_title .= ("restored signal fd = " ++ show k ++ " * f0 ")
    setColors [opaque blue]
    let discrets = [0.0,1 / (k * f0) .. (periods * 2) / f0]
    plot (line "" [zip discrets $ elems $ restoredSignal k n])

plotAmplitudeSpectre1 = plotAmplitudeSpectre "spectrum1.svg" (head k)

plotPhaseSpectre1 = plotPhaseSpectre "phasespectrum1.svg" (head k)

plotAmplitudeSpectre2 = plotAmplitudeSpectre "spectrum2.svg" (k !! 1)

plotPhaseSpectre2 = plotPhaseSpectre "phasespectrum2.svg" (k !! 1)

plotAmplitudeSpectre3 = plotAmplitudeSpectre "spectrum3.svg" (k !! 2)

plotPhaseSpectre3 = plotPhaseSpectre "phasespectrum3.svg" (k !! 2)

plotRestoredSignal1 = plotRestoredSignal "restoredsignal1.svg" (k !! 2) (head m)

plotRestoredSignal2 = plotRestoredSignal "restoredsignal2.svg" (k !! 2) (m !! 1)

plotRestoredSignal3 = plotRestoredSignal "restoredsignal3.svg" (k !! 2) (m !! 2)
