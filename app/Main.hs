module Main where

import System.Random

data Particle = BottomQuark | 
              BottomAntiQuark |
              Gluon | 
              TauLepton | 
              AntiTauLepton | 
              CharmQuark | 
              CharmAntiQuark | 
              Photon | 
              Muon | 
              AntiMuon | 
              Neutrino |
              AntiNeutrino |
              Electron |
              Positron |
              DownQuark |
              DownAntiQuark |
              StrangeQuark |
              StrangeAntiQuark |
              UpQuark |
              UpAntiQuark |
              HBoson |
              ZBoson |
              WBoson |
              TopQuark |
              TopAntiQuark deriving (Show,Eq)

--The possible outcomes for HBoson
fallApartH :: Int -> [Particle]
fallApartH num 
        | num <= 648000 = [BottomQuark,BottomAntiQuark]
        | num <= 789000 = [WBoson, WBoson]
        | num <= 877200 = [Gluon, Gluon]
        | num <= 947600 = [TauLepton, AntiTauLepton]
        | num <= 980300 = [CharmQuark, CharmAntiQuark]
        | num <= 996200 = [ZBoson, ZBoson]
        | num <= 998430 = [Photon, Photon]
        | num <= 999540 = [ZBoson, Photon]
        | num <= 999784 = [Muon,AntiMuon]
        | num <= 1000000 = [TopQuark, TopAntiQuark]
        
--The possible outcomes for WBoson
fallApartW :: Int -> [Particle]
fallApartW num 
        | num <= 333333 = [Positron, Neutrino]
        | num <= 666666 = [AntiMuon, Neutrino]
        | num <= 1000000 = [AntiTauLepton, Neutrino]

--The possible outcomes for ZBoson
fallApartZ :: Int -> [Particle]
fallApartZ num 
        | num <= 206000 = [Neutrino, AntiNeutrino]
        | num <= 240000 = [Electron, Positron]
        | num <= 274000 = [Muon, AntiMuon]
        | num <= 308000 = [TauLepton, AntiTauLepton]
        | num <= 460000 = [DownQuark, DownAntiQuark]
        | num <= 612000 = [StrangeQuark,StrangeAntiQuark]
        | num <= 764000 = [BottomQuark,BottomAntiQuark]
        | num <= 882000 = [TopQuark,TopAntiQuark]
        | num <= 1000000 = [CharmQuark,CharmAntiQuark]

--The possible outcomes for TopQuark     
fallApartTopQ :: Int -> [Particle]
fallApartTopQ num 
        | num <= 333333 = [WBoson, DownQuark]
        | num <= 666666 = [WBoson, StrangeQuark]
        | num <= 1000000 = [WBoson, BottomQuark]

--The possible outcomes for TopAntiQuark  
fallApartTopAQ :: Int -> [Particle]
fallApartTopAQ num 
        | num <= 333333 = [WBoson, DownAntiQuark]
        | num <= 666666 = [WBoson, StrangeAntiQuark]
        | num <= 1000000 = [WBoson, BottomAntiQuark]


active :: Particle -> Bool
active HBoson = True
active ZBoson = True
active WBoson = True
active TopQuark = True
active TopAntiQuark = True
active _ = False
 
takeActive :: [Particle] -> [Particle]
takeActive = filter active 

takeStable :: [Particle] -> [Particle]
takeStable = filter $ not . active

--returns True if there aren't any active particles
noActive :: [Particle] -> Bool
noActive = null . takeActive

intRandomR :: StdGen -> (Int, StdGen)
intRandomR gen1 = (num, gen2)
           where (num,gen2) = randomR (1,1000000) gen1


--the probability for a HBoson to fall apart
sayWhenH :: StdGen ->[Particle]
sayWhenH gen1 = if num1 <= 433   then fallApartH num2
                                  else [HBoson]
        where (num1,gen2) = intRandomR gen1
              (num2,gen3) = intRandomR gen2

--the probability for a ZBoson to fall apart
sayWhenZ :: StdGen -> [Particle]
sayWhenZ gen1  = if num1 <= 500000 then fallApartZ num2 
                                   else [ZBoson]
        where (num1,gen2) = intRandomR gen1
              (num2,_) = intRandomR gen2

--the probability for a WBoson to fall apart              
sayWhenW :: StdGen -> [Particle]
sayWhenW gen1  = if num1 <= 500000 then fallApartW num2 
                                   else [WBoson]
        where (num1,gen2) = intRandomR gen1
              (num2,_) = intRandomR gen2

--the probability for a TopQuark to fall apart                 
sayWhenTopQ :: StdGen -> [Particle]
sayWhenTopQ gen1  = if num1 <= 129500 then fallApartTopQ num2 
                                   else [TopQuark]
        where (num1,gen2) = intRandomR gen1
              (num2,_) = intRandomR gen2

--the probability for a TopAntiQuark to fall apart                 
sayWhenTopAQ :: StdGen -> [Particle]
sayWhenTopAQ gen1  = if num1 <= 129500 then fallApartTopAQ num2 
                                   else [TopAntiQuark]
        where (num1,gen2) = intRandomR gen1
              (num2,_) = intRandomR gen2

randomTwoTimes :: StdGen -> (Int,StdGen)
randomTwoTimes gen0 = (num2,gen2)
        where (num1,gen1) = intRandomR gen0
              (num2,gen2) = intRandomR gen1

--takes list of particles, divides stable active particle from stable ones             
willPutYouBackTogether :: StdGen -> [Particle] -> [Particle]
willPutYouBackTogether gen lst = onebyoneActive gen (takeActive lst) ++ takeStable lst

--receives only active particles and runs the probabilities to fall apart
onebyoneActive :: StdGen -> [Particle] -> [Particle]
onebyoneActive _ [] = []
onebyoneActive gen (x : xs) = case x of 
                             HBoson -> sayWhenH gen
                             WBoson -> sayWhenW gen
                             ZBoson -> sayWhenZ gen
                             TopQuark -> sayWhenTopQ gen
                             TopAntiQuark -> sayWhenTopAQ gen 
                        ++ onebyoneActive newg xs
                where (_,newg) = randomTwoTimes gen


countStable :: Particle -> [Particle] -> String
countStable p lst =  show num ++ " " ++ show p ++ if num > 1 then "s" else ""
        where num = length (filter (== p) lst)

--receives the final stable Universe and counts its particles
particlesToString :: [Particle] -> String
particlesToString [] = []
particlesToString lst@(x:_) = countStable x lst ++ "," ++ particlesToString (filter (/= x) lst)


helpMain :: Int -> [Particle] -> IO()
helpMain n lst = if noActive lst 
                 then do
                        print "Stable Universe: "
                        print $ init $ particlesToString lst
                 else do 
                        gen <- newStdGen 
                        print $ show n ++ "."
                        print lst
                        helpMain (n + 1) $ willPutYouBackTogether gen lst

--from any beggining state  
begginingState :: [Particle] -> IO()
begginingState = helpMain 0

--beggining state is one HBoson
main :: IO()
main = begginingState [HBoson]

