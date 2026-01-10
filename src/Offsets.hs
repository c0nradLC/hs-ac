module Offsets
where
import Data.Word (Word64)

playerEntityPointer :: Word64
playerEntityPointer = 0x19d518

playerListPointer :: Word64
playerListPointer = 0x19d520

maxPlayers :: Word64
maxPlayers = 0x19d52C

consumeAmmoInstr :: Word64
consumeAmmoInstr = 0xfd06e

attackPhysicsFunction :: Word64
attackPhysicsFunction = 0xfaf20

isVisibleFunction :: Word64
isVisibleFunction = 0x1253e0

attackFunction :: Word64
attackFunction = 0x78610

playerInCrosshairFunction :: Word64
playerInCrosshairFunction = 0xf7780

playerHealth :: Word64 
playerHealth = 0x100

playerPos :: Word64
playerPos = 0x8

playerAimY :: Word64
playerAimY = 0x3c

playerAimX :: Word64
playerAimX = 0x38

playerTeam :: Word64
playerTeam =  0x320

playerState :: Word64
playerState = 0x32C

playerName :: Word64
playerName = 0x219

nextBot :: Word64
nextBot = 0x10
