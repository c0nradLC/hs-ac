module Types(Player(..), ACPlayer(..), ACVec(..)) where
import Foreign.C ( CFloat, CInt )
import Foreign
    ( nullPtr,
      Storable(pokeByteOff, peekByteOff, poke, peek, sizeOf, alignment) )
import qualified Offsets

data Player = Player
    { _pos      :: (Float, Float, Float)
    , _team     :: Int
    , _state    :: Int
    , _distance :: Maybe Float
    , _visible  :: Bool
    , _aimX     :: Float
    , _aimY     :: Float
    , _baseAddr :: Word
    }
    deriving (Show)

newtype ACPlayer = ACPlayer
    { _cpTeam :: CInt
    }
    deriving (Show)

instance Storable ACPlayer where
    sizeOf _ = 0x324
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
        if ptr == nullPtr then do
            return ACPlayer {_cpTeam = -1}
        else do
            team <- peekByteOff ptr $ fromIntegral Offsets.playerTeam
            return ACPlayer {_cpTeam = team}

    -- no poke definition because we won't be updating the playerent from Haskell
    poke _ _ = return ()

data ACVec = ACVec
    { _x :: CFloat
    , _y :: CFloat
    , _z :: CFloat
    }
    deriving (Show)

instance Storable ACVec where
    sizeOf _ = 12
    alignment _ = 4

    peek ptr = do
        x <- peekByteOff ptr 0
        y <- peekByteOff ptr 4
        z <- peekByteOff ptr 8
        return $ ACVec x y z

    poke ptr vec = do
        pokeByteOff ptr 0 (_x vec)
        pokeByteOff ptr 4 (_y vec)
        pokeByteOff ptr 8 (_z vec)